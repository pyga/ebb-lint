# coding: utf-8

from __future__ import unicode_literals

import sys

from codifer import Collected, Source
import flake8_polyfill.options
import flake8_polyfill.stdin
import pycodestyle
from intervaltree import Interval, IntervalTree

from ebb_lint._version import get_versions
from ebb_lint.checkers.registration import _collector
from ebb_lint.errors import Errors


_pycodestyle_noqa = pycodestyle.noqa
# This is a blight. Disable it unconditionally.
pycodestyle.noqa = lambda ign: False

flake8_polyfill.stdin.monkey_patch('pycodestyle')


def byte_intersection(tree, lower, upper):
    ret = 0
    for i in tree.search(lower, upper):
        ret += min(i.end, upper) - max(i.begin, lower)
    return ret


class EbbLint(object):
    name = 'ebb_lint'
    version = get_versions()['version']

    _collected = None

    def __init__(self, tree, filename):
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
        if cls._collected is not None:
            return

        current_python_grammar = 'py{0.major}{0.minor}'.format(sys.version_info)
        cls._collected = Collected.from_grammar_name(current_python_grammar)
        cls._collected.gather_checkers(_collector)

    def run(self):
        source = Source.from_filename(self.filename)
        parsed = self._collected.parse_source(source)
        if not parsed.had_trailing_newline:
            yield source.message_for_pos(
                source.lines.last_pos, Errors.no_trailing_newline)

        for error in self._collected.check_parsed(parsed):
            yield error

        for node in parsed.tree.pre_order():
            for error in self._scan_node_for_ranges(source, node):
                yield error

        for error in self._check_line_lengths(source):
            yield error

    def _scan_node_for_ranges(self, source, node):
        grammar = self._collected.grammar
        token = grammar.token

        if node.children or (node.type != token.STRING and not node.prefix):
            return

        byte = source.lines.byte_of_node(node)

        if node.type == token.STRING:
            self._intervals['string literals'].add(Interval(
                byte, byte + len(node.value)))

        comments = list(self._collected.find_comments(node.prefix, byte - len(node.prefix)))
        for c, i in comments:
            self._intervals['comments'].add(i)
            m = _pycodestyle_noqa(c)
            if m is not None:
                yield source.message_for_pos(
                    source.lines.position_of_byte(i.begin + m.start()),
                    Errors.no_noqa)

    def _check_line_lengths(self, source):
        soft_limit = self.options.max_line_length
        hard_limit = self.options.hard_max_line_length
        permitted_percentage = self.options.permissive_bulkiness_percentage

        for lineno, line_start, line in source.lines:
            line = line.rstrip('\r\n')
            if len(line) <= soft_limit:
                continue
            if len(line) > hard_limit:
                yield source.message_for_pos(
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
            yield source.message_for_pos(
                (lineno, soft_limit), Errors.line_too_long,
                length=len(line), which_limit='soft', limit=soft_limit,
                extra=extra)
