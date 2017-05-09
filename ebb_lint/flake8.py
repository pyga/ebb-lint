# coding: utf-8

from __future__ import unicode_literals

import bisect
import io
import sys
from awpa.btm_matcher import BottomMatcher
from awpa import (
    decode_bytes_using_source_encoding,
    load_grammar,
    patcomp,
    read_file_using_source_encoding)

import flake8_polyfill.options
import flake8_polyfill.stdin
import pycodestyle
import six
import venusian
from intervaltree import Interval, IntervalTree

from ebb_lint._version import get_versions
from ebb_lint.errors import Errors
from ebb_lint import checkers


_pycodestyle_noqa = pycodestyle.noqa
# This is a blight. Disable it unconditionally.
pycodestyle.noqa = lambda ign: False

flake8_polyfill.stdin.monkey_patch('pycodestyle')


def tokenize_source_string(grammar, s, base_byte=0):
    lines = Lines(io.StringIO(s))
    for typ, tok, spos, epos, _ in grammar.generate_tokens(s):
        yield typ, tok, Interval(
            lines.byte_of_pos(*spos) + base_byte,
            lines.byte_of_pos(*epos) + base_byte)


def current_python_grammar():
    grammar_name = 'py{0.major}{0.minor}'.format(sys.version_info)
    return load_grammar(grammar_name)


def fix_grammar_for_future_features(grammar, future_features):
    if 'print_function' in future_features and 'print' in grammar.keywords:
        del grammar.keywords['print']


def find_comments(grammar, source, base_byte=0):
    source = six.text_type(source).rstrip(' \t\r\n\\')
    for typ, tok, interval in tokenize_source_string(
            grammar, source, base_byte=base_byte):
        if typ == grammar.token.COMMENT:
            yield tok, interval


class Lines(object):
    def __init__(self, infile):
        count = 0
        self.lines = [(0, '')]
        for line in infile:
            self.lines.append((count, line))
            count += len(line)
        self.last_pos = len(self.lines) - 1, len(self.lines[-1][1])
        self.last_byte = count

    def __getitem__(self, idx):
        return self.lines[idx]

    def __iter__(self):
        for e, (count, line) in enumerate(self.lines):
            if e == 0:
                continue
            yield e, count, line

    def position_of_byte(self, byte):
        lineno = bisect.bisect_left(self.lines, (byte + 1,)) - 1
        column = byte - self.lines[lineno][0]
        return lineno, column

    def byte_of_pos(self, lineno, column):
        # This requires a bit of explanation. The source passed to lib2to3's
        # parser has an extra newline added in some cases, to deal with a bug
        # in lib2to3 where it crashes hard if files don't end with a trailing
        # newline. When that extra line is added, the final DEDENT token in the
        # file will have a lineno equal to the lines in the file plus one,
        # becase it's "at" a location that doesn't exist in the real file. If
        # this case wasn't specifically caught, the self[lineno] would raise an
        # exception because lineno is beyond the last index in self.lines. So,
        # when that case is detected, return the final byte position.
        if lineno == len(self.lines) and column == 0:
            return self.last_byte
        byte, _ = self[lineno]
        byte += column
        return byte

    def byte_of_node(self, node):
        return self.byte_of_pos(node.lineno, node.column)


def byte_intersection(tree, lower, upper):
    ret = 0
    for i in tree.search(lower, upper):
        ret += min(i.end, upper) - max(i.begin, lower)
    return ret


class EbbLint(object):
    name = 'ebb_lint'
    version = get_versions()['version']

    collected_checkers = None
    _source = None
    _lines = None

    def __init__(self, tree, filename):
        self.tree = tree
        self.filename = filename
        self._intervals = {
            'comments': IntervalTree(),
            'string literals': IntervalTree(),
        }

    @classmethod
    def add_options(cls, parser):
        flake8_polyfill.options.register(
            parser, '--hard-max-line-length', default=119, type=int,
            metavar='n', help='absolute maximum line length allowed',
            parse_from_config=True)
        flake8_polyfill.options.register(
            parser, '--permissive-bulkiness-percentage', default=67, type=int,
            metavar='p', help=(
                'integer percentage of a line which must be string literals '
                'or comments to be allowed to pass the soft line limit'),
            parse_from_config=True)

    @classmethod
    def parse_options(cls, options):
        # We implement our own line-length checker because it's not possible to
        # customize how another checker does its checking.
        options.ignore += 'E501',
        cls.options = options

        # This vastly speeds up the test suite, since parse_options is called
        # on every test now, and venusian does a lot of work.
        if cls.collected_checkers is not None:
            return

        collected_checkers = []
        _, grammar, _ = current_python_grammar()

        def register_checker(pattern, checker, extra):
            if ('python_minimum_version' in extra
                    and sys.version_info < extra['python_minimum_version']):
                return
            if ('python_disabled_version' in extra
                    and sys.version_info > extra['python_disabled_version']):
                return
            pattern, tree = patcomp.compile_pattern(
                grammar, pattern, with_tree=True)
            collected_checkers.append((pattern, tree, checker, extra))

        scanner = venusian.Scanner(register=register_checker)
        scanner.scan(checkers)
        matcher = BottomMatcher(grammar)
        for e, (_, tree, _, _) in enumerate(collected_checkers):
            matcher.add_pattern_by_key(tree, e)
        cls.collected_checkers = collected_checkers
        cls.matcher = matcher

    @property
    def source(self):
        if self._source is None:
            if self.filename != 'stdin':
                self._source = read_file_using_source_encoding(self.filename)
            elif six.PY2:  # ✘py3
                # On python 2, reading from stdin gives you bytes, which must
                # be decoded.
                self._source = decode_bytes_using_source_encoding(
                    pycodestyle.stdin_get_value())
            else:  # ✘py2
                # On python 3, reading from stdin gives you text.
                self._source = pycodestyle.stdin_get_value()
        return self._source

    @property
    def lines(self):
        if self._lines is None:
            self._lines = Lines(self.source.splitlines(True))
        return self._lines

    def _message_for_node(self, node, error, **kw):
        line_offset = kw.pop('line_offset', None)
        if line_offset is None:
            byte = self.lines.byte_of_node(node) + kw.pop('offset', 0)
            lineno, column = self.lines.position_of_byte(byte)
        else:
            lineno = node.lineno + line_offset
            column = kw.pop('column')
        return self._message_for_pos((lineno, column), error, **kw)

    def _message_for_pos(self, pos, error, **kw):
        lineno, column = pos
        message = 'L{:03d} {}'.format(
            error.value.code, error.value.message.format(**kw))
        return lineno, column, message, type(self)

    def run(self):
        _, self._grammar, pysyms = current_python_grammar()
        self.future_features = self._grammar.detect_future_features(
            self.source)
        fix_grammar_for_future_features(self._grammar, self.future_features)
        tree, trailing_newline = self._grammar.parse_source(self.source)
        if not trailing_newline:
            yield self._message_for_pos(
                self.lines.last_pos, Errors.no_trailing_newline)

        for error in self._check_tree(tree):
            yield error

        for error in self._check_line_lengths():
            yield error

    def _check_tree(self, tree):
        matches = self.matcher.run(tree.pre_order())
        node_matches = {}
        for checker_idx, nodes in six.iteritems(matches):
            for node in nodes:
                node_matches.setdefault(id(node), set()).add(checker_idx)

        for node in tree.pre_order():
            for error in self._scan_node_for_ranges(node):
                yield error

            for checker_idx in node_matches.get(id(node), ()):
                pattern, tree, checker, extra = (
                    self.collected_checkers[checker_idx])
                results = {}
                if not pattern.match(node, results):
                    continue
                for k in extra.get('comments_for', ()):
                    # XXX: this doesn't use `k` for finding the node; `k` is
                    # supposed to name a specific node, but it isn't used when
                    # choosing which node is added to results.
                    results[k + '_comments'] = [
                        c for c, i in find_comments(
                            self._grammar, node.prefix)]
                if extra.get('pass_filename', False):
                    results['filename'] = self.filename
                if extra.get('pass_future_features', False):
                    results['future_features'] = self.future_features
                if extra.get('pass_grammar', False):
                    results['grammar'] = self._grammar
                for error_node, error, kw in checker(**results):
                    yield self._message_for_node(error_node, error, **kw)

    def _scan_node_for_ranges(self, node):
        token = self._grammar.token

        if node.children or (node.type != token.STRING and not node.prefix):
            return

        byte = self.lines.byte_of_node(node)

        if node.type == token.STRING:
            self._intervals['string literals'].add(Interval(
                byte, byte + len(node.value)))

        comments = list(
            find_comments(self._grammar, node.prefix, byte - len(node.prefix)))
        for c, i in comments:
            self._intervals['comments'].add(i)
            m = _pycodestyle_noqa(c)
            if m is not None:
                yield self._message_for_pos(
                    self.lines.position_of_byte(i.begin + m.start()),
                    Errors.no_noqa)

    def _check_line_lengths(self):
        soft_limit = self.options.max_line_length
        hard_limit = self.options.hard_max_line_length
        permitted_percentage = self.options.permissive_bulkiness_percentage
        for lineno, line_start, line in self.lines:
            line = line.rstrip('\r\n')
            if len(line) <= soft_limit:
                continue
            if len(line) > hard_limit:
                yield self._message_for_pos(
                    (lineno, hard_limit), Errors.line_too_long,
                    length=len(line), which_limit='hard', limit=hard_limit,
                    extra='')
                continue

            line_end = line_start + len(line)
            percentages = {}
            for name, i in self._intervals.items():
                n_bytes = byte_intersection(i, line_start, line_end)
                percentages[name] = p = n_bytes * 100 // len(line)
                assert 0 <= p <= 100, 'line percentage not in range'

            if any(p >= permitted_percentage for p in percentages.values()):
                continue

            extra = ' since the line has ' + '; '.join(
                '{p}% {name}'.format(p=p, name=name)
                for name, p in percentages.items())
            yield self._message_for_pos(
                (lineno, soft_limit), Errors.line_too_long,
                length=len(line), which_limit='soft', limit=soft_limit,
                extra=extra)
