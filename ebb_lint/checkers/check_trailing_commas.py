from __future__ import unicode_literals

from six.moves import reduce

from ebb_lint.checkers.registration import register_checker
from ebb_lint.errors import Errors


def is_comprehension(pysyms, t):
    if t == pysyms.comp_for:
        return True
    elif t == getattr(pysyms, 'list_for', None):
        return True
    return False


def last(seq):
    return reduce(lambda l, r: r, seq)


@register_checker("""

( atom< '(' contents=any+ end=')' >
| atom< '[' contents=any+ end=']' >
| atom< '{' contents=any+ end='}' >
)

""", pass_grammar=True)
def check_trailing_commas(grammar, end, contents):
    pysyms = grammar.symbols
    last_element = contents[-1]
    if (last_element.children and
            is_comprehension(pysyms, last_element.children[-1].type)):
        # It's a comprehension, so ignore it.
        return
    last_element_leaf = last(last_element.pre_order())
    if last_element_leaf.value == ',':
        return
    if end.value == ')' and (
            len(contents) != 1 or contents[0].type != pysyms.testlist_comp):
        # It's not a tuple or a generator expression; it's just something in
        # parentheses, so ignore it.
        return
    # If there's no trailing comma, the whitespace between the last element and
    # the closing delimiter must not contain a newline, as that would mean the
    # end of the last element and the closing delimiter are on different lines.
    if '\n' in end.prefix:
        yield last_element_leaf, Errors.no_trailing_comma_in_literal, {}
