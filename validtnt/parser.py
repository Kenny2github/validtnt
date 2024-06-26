"""Contains the class that parses TNT text into usable objects."""
import re
from typing import Optional, Tuple, Iterator, get_args
from .leaves import Statement, Formula, FantasyMarker, FantasyRule, \
     Negated, Quantifier, std, Quantified, Logic, Compound, Variable, \
     Term, Numeral, Operator, Successed, MultiTerm, Rules, \
     Fantasy, Text

__all__ = ['ParsingFailed', 'TNTParser']

class ParsingFailed(ValueError):
    """Parsing TNT text failed."""
    def __init__(self, message, lineno, line):
        super().__init__(f'(text) line {lineno}: {line.strip()}\n{message}')
        self.lineno = lineno
        self.line = line

LOGIC = set('⊃→]&∧^|∨Vv')
OPERATORS = set('+.⋅*')
VARIABLES = set('abcde')
PRIMES = set("'′")
NEGATORS = set('~!')
QUANTIFIERS = set('AE∀∃')
NUMBERS = set('0123456789')
WHITESPACE = set('\t \n')

RULES = re.compile(r'''(?xi)
    joining | separation | double-tilde | fantasy\ rule
    | carry[-\ ]over(?:\ of)?(?:\ line)?\ ([0-9]+) | detachment
    | contrapositive | De\ Morgan | switcheroo
    | specification | generalization | interchange
    | existence | symmetry | transitivity
    | add\ S | drop\ S | induction
    | axiom\ ([1-5]) | premise | push | pop
''')
LINENOS = re.compile(r'(?:[0-9]+)(?:(?:, ?|,? and )(?:[0-9]+))*')
COMMENT = re.compile(r'\[[^\n\]]+\]')

def recurse_vars(formula: Formula | Term, quant: dict[Variable, Quantifier],
                 free: set[Variable]) -> None:
    """Compute which variables are quantified by recursing into each argument."""
    if isinstance(formula, (Negated, Quantified)):
        if isinstance(formula, Quantified):
            quant[formula.variable] = formula.quantifier
        recurse_vars(formula.arg, quant, free)
    elif isinstance(formula, Variable):
        if formula not in quant:
            free.add(formula)
    elif isinstance(formula, (Compound, Formula)):
        recurse_vars(formula.arg1, quant, free)
        recurse_vars(formula.arg2, quant, free)

class TNTParser:
    """Parse TNT text into objects."""

    text: Optional[str] = None

    def __init__(self, text: Optional[str] = None):
        """Initialize the parser, optionally with text."""
        self.text = text

    def parse(self, text: Optional[str] = None) -> Text:
        """Returns an OrderedDict subclass of line numbers to Statements
        parsed from the text.
        Raises ParsingFailed if parsing fails,
        or TypeError if no text is given or set.
        """
        if text is None:
            if self.text is None:
                raise TypeError('No text set at initialization or given in method call.')
            text = self.text
        result = Text()
        content = result
        fantasy = None
        try:
            for lineno, line in enumerate(text.splitlines()):
                statement = self.parse_line(lineno, line)
                if statement is None: # comment/empty
                    continue
                statement.fantasy = fantasy
                if statement.rule is FantasyRule.POP and fantasy is not None:
                    fantasy.content.flash()
                    if not fantasy.content.vals:
                        raise AssertionError('empty fantasy')
                    fantasy = fantasy.fantasy
                    if fantasy is None:
                        content = result
                    else:
                        content = fantasy.content
                elif statement.rule is FantasyRule.POP:
                    raise AssertionError('popping from top level')
                assert statement.lineno is not None, 'failed to parse line number'
                content[statement.lineno] = statement
                result[statement.lineno] = statement
                if statement.rule is FantasyRule.PUSH:
                    fantasy = Fantasy(lineno=lineno,
                                      content=Text(),
                                      fantasy=fantasy)
                    content = fantasy.content
            if fantasy is not None:
                raise AssertionError('unclosed fantasy started on line '
                                     + str(fantasy.lineno))
        except AssertionError as exc:
            raise ParsingFailed(exc.args[0], lineno, line) from None
        result.flash()
        return result

    def parse_line(self, lineno: int, line: Optional[str]) -> Optional[Statement]:
        """Returns a single Statement."""
        if line is None:
            if self.text is None:
                raise TypeError('No text set at initialization '
                                'and no line given in method call.')
            text = self.text.splitlines()[lineno]
        else:
            text = line
        if not text.strip():
            return None # effectively empty
        try:
            start = self.whitespace(0, text)
            end = start
            while text[end] in NUMBERS:
                end += 1
            if end != start: # number present
                num = int(text[start:end])
                start = end = self.whitespace(end, text, True)
            else:
                num = lineno
            if COMMENT.match(text, start):
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
            return Statement(lineno=num, formula=formula,
                             rule=rule, referrals=refs)
        except IndexError:
            raise AssertionError('unexpected EOL')

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
            count = end - start
            end, fmla = self.formula(end, text)
            fmla = Negated(arg=fmla, negations=count)
            return end, fmla
        if text[start] in QUANTIFIERS: # Aa:<...>
            quant = Quantifier(std(text[start]))
            start, var = self.variable(start+1, text)
            assert text[start] == ':', \
                   f'column {start}: invalid syntax, missing colon'
            end, fmla = self.formula(start+1, text) # skip the colon
            fmla = Quantified(arg=fmla, variable=var, quantifier=quant)
            return end, fmla
        if text[start] == '<': # <...X...>
            start, arg1 = self.formula(start+1, text)
            assert text[start] in LOGIC, \
                   f'{text[start]} is not a valid logic operator'
            logop = Logic(std(text[start]))
            start, arg2 = self.formula(start+1, text)
            assert text[start] == '>', \
                   f'column {start}: invalid syntax, not a {">"!r}'
            quantified1 = {}
            free1 = set()
            recurse_vars(arg1, quantified1, free1)
            quantified2 = {}
            free2 = set()
            recurse_vars(arg2, quantified2, free2)
            for var in free1:
                assert var not in quantified2, \
                       f'{str(var)!r} free in first formula ' \
                       'but quantified in second'
            for var in free2:
                assert var not in quantified1, \
                       f'{str(var)!r} free in second formula ' \
                       'but quantified in first'
            return start+1, Compound(arg1=arg1, arg2=arg2, operator=logop)
        # ...=...
        start, arg1 = self.term(start, text)
        assert text[start] == '=', \
               f'column {start}: invalid syntax, not a {"="!r}'
        start, arg2 = self.term(start+1, text)
        return start, Formula(arg1=arg1, arg2=arg2)

    def variable(self, start: int, text: str) -> Tuple[int, Variable]:
        """Parse a variable."""
        assert text[start] in VARIABLES, \
               f'invalid variable name: {text[start]!r}'
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
            trm = Successed(successors=count, arg=trm)
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
            assert text[start] == ')', \
                   f'column {start}: invalid syntax, not a {")"!r}'
            return start+1, MultiTerm(arg1=arg1, arg2=arg2, operator=op)
        raise AssertionError(f'column {start}: invalid syntax')

    def rule(self, start: int, text: str) -> Tuple[int, Rules, Optional[int]]:
        """Parse a rule. Last element is referral to add, if any"""
        match = RULES.match(text, start)
        assert match is not None, f'invalid rule: {text[start:]!r}'
        num = match.group(1) # if carry rule, this will match, otherwise None
        rul = match.group(0)
        if rul.startswith('carry'):
            rul = 'carry'
        for r in get_args(Rules):
            try:
                rul = r(rul)
            except ValueError:
                pass
            else:
                break
        assert isinstance(rul, Rules), \
               'Rules<->RULES mismatch, report as bug'
        return match.end(), rul, int(num) if num is not None else None

    def refs(self, start: int, text: str) -> Iterator[int]:
        """Parse referrals."""
        while text[start] not in NUMBERS:
            start += 1
        match = LINENOS.match(text, start)
        assert match is not None, 'invalid referrals'
        assert text[match.end()] == ')', \
               f'column {match.end()}: invalid syntax, not a {")"!r}'
        for i in re.split(',? and |, ?', match.group(0)):
            yield int(i)
