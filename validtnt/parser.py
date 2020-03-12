"""Contains the class that parses TNT text into usable objects."""
import re
from typing import Mapping, Optional, Tuple, Union
from .leaves import *

__all__ = ['ParsingFailed', 'TNTParser']

class ParsingFailed(ValueError):
    def __init__(self, message, lineno, line):
        super().__init__(f'line {lineno}: {line}; {message}')

LOGIC = set('⊃→]&∧^|∨Vv')
OPERATORS = set('+.⋅*')
VARIABLES = set('abcde')
PRIMES = set("'′")
NEGATORS = set('~!')
QUANTIFIERS = set('AE∀∃')
NUMBERS = set('0123456789')
WHITESPACE = set('\t \v\n')

RULES = re.compile('''(?xi)
    joining | separation | double-tilde | fantasy\\ rule
    | carry[- ]over(?:\\ of)?(?:\\ line)?\\ ([0-9]+) | detachment
    | contrapositive | De\\ Morgan | switcheroo
    | specification | generalization | interchange
    | existence | symmetry | transitivity
    | add\\ S | drop\\ S | induction
    | axiom\\ ([1-5]) | premise | push | pop
''')
LINENOS = re.compile(r'(?:[0-9]+)(?:(?:, ?|,? and )(?:[0-9]+))*')
COMMENT = re.compile(r'\[[^\n\]]+\]')

class TNTParser:
    """Parse TNT text into objects."""

    def parse(self, text: str) -> Mapping[int, Statement]:
        """Returns a dictionary of line numbers to Statements parsed from the text.
        Raises ParsingFailed if parsing fails.
        """
        result = {}
        try:
            for lineno, line in enumerate(text.splitlines()):
                statement = self.parse_line(lineno, line)
                if statement is None: #comment
                    continue
                result[statement.lineno] = statement
        except AssertionError as exc:
            raise ParsingFailed(exc.args[0], lineno, line) from None
        return result

    def parse_line(self, lineno: int, line: str) -> Optional[Statement]:
        """Returns a single Statement."""
        text = line
        start = self.whitespace(0, line)
        end = start
        while line[end] in NUMBERS:
            end += 1
        if end != start: # number present
            num = int(line[start:end])
            start = end = self.whitespace(end, line, True)
        else:
            num = lineno
        if COMMENT.match(line, start):
            return None
        start, formula = self.formula(start, text)
        start = end = self.whitespace(start, text, True)
        start, rule, ref = self.rule(start, text)
        start = self.whitespace(start, text)
        if ref is not None:
            refs = [ref]
        else:
            refs = []
        if start < len(text) and text[start] == '(':
            refs.extend(self.refs(start, text))
        return Statement(lineno=num, formula=formula, rule=rule, referrals=refs)

    def whitespace(self, start: int, text: str, required: bool = False) -> int:
        """Skip past whitespace."""
        end = start
        try:
            while text[end] in WHITESPACE:
                end += 1
        except IndexError:
            end = len(text)
        assert not (required and end == start), \
               f'column {start}: required whitespace not present'
        return end

    def formula(self, start: int, text: str) -> Tuple[int, Formula]:
        """Parse the formula."""
        if text[start] == '[':
            return start+1, FantasyMarker(rule=FantasyRule.PUSH)
        if text[start] == ']':
            return start+1, FantasyMarker(rule=FantasyRule.POP)
        if text[start] in NEGATORS: # ~<...>
            end = start+1
            while text[end] in NEGATORS:
                end += 1
            end, fmla = self.formula(end, text)
            fmla = Negated(arg=fmla, negations=end-start)
            return end, fmla
        if text[start] in QUANTIFIERS: # Aa:<...>
            quant = Quantifier(std(text[start]))
            start, var = self.variable(start+1, text)
            end, fmla = self.formula(start+1, text)
            fmla = Quantified(arg=fmla, variable=var, quantifier=quant)
            return end, fmla
        if text[start] == '<': # <...X...>
            start, arg1 = self.formula(start+1, text)
            assert text[start] in LOGIC, f'{text[start]} is not a valid logic operator'
            logop = Logic(std(text[start]))
            start, arg2 = self.formula(start+1, text)
            assert text[start] == '>', f'column {start}: invalid syntax, not a {">"!r}'
            return start+1, Compound(arg1=arg1, arg2=arg2, operator=logop)
        # ...=...
        start, arg1 = self.term(start, text)
        assert text[start] == '=', f'column {start}: invalid syntax, not a {"="!r}'
        start, arg2 = self.term(start+1, text)
        return start, Formula(arg1=arg1, arg2=arg2)

    def variable(self, start: int, text: str) -> Tuple[int, Variable]:
        """Parse a variable."""
        assert text[start] in VARIABLES, f'invalid variable name: {text[start]!r}'
        end = start+1
        while text[end] in PRIMES:
            end += 1
        count = end - start - 1
        return end, Variable(letter=text[start], primes=count)

    def term(self, start: int, text: str) -> Tuple[int, Term]:
        """Parse a term."""
        if text[start] == 'S': # successorship
            end = start+1
            while text[end] == 'S':
                end += 1
            count = end - start
            end, trm = self.term(end, text)
            trm.successors = count
            return end, trm
        if text[start] == '0': # the singleton 0
            return start+1, Numeral()
        if text[start] in VARIABLES: # variable
            return self.variable(start, text)
        if text[start] == '(': # (...X...)
            start, arg1 = self.term(start+1, text)
            assert text[start] in OPERATORS, \
                   f'{text[start]!r} is not a valid arithmetic operator'
            op = Operator(std(text[start]))
            start, arg2 = self.term(start+1, text)
            assert text[start] == ')', f'column {start}: invalid syntax, not a {")"!r}'
            return start, MultiTerm(arg1=arg1, arg2=arg2, operator=op)
        raise AssertionError(f'column {start}: invalid syntax')

    def rule(self, start: int, text: str) -> Tuple[int, Union[Rules], Optional[int]]:
        """Parse a rule. Last element is referral to add, if any"""
        match = RULES.match(text, start)
        assert match is not None, f'invalid rule: {text[start:]!r}'
        num = match.group(1) # if it was the carry rule, this will match, otherwise None
        rul = match.group(0)
        if rul.startswith('carry'):
            rul = re.sub('[0-9]+', 'n', match.group(0))
            # the internal rule uses n to replace the line number
        for r in Rules:
            try:
                rul = r(rul)
            except ValueError:
                pass
            else:
                break
        assert isinstance(rul, Rules), f'Rules<->RULES mismatch, report as bug'
        return match.end(), rul, int(num) if num is not None else None

    def refs(self, start, text):
        """Parse referrals."""
        while text[start] not in NUMBERS:
            start += 1
        match = LINENOS.match(text, start)
        assert match is not None, f'invalid referrals'
        assert text[match.end()] == ')', f'column {match.end()}: invalid syntax, not a {")"!r}'
        for i in re.split(', ?|,? and ', match.group(0)):
            yield int(i)
