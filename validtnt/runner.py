"""
Validate TNT.
"""
#pylint: skip-file
from collections import OrderedDict
from typing import Union, Optional
from .parser import TNTParser
from .leaves import *

__all__ = ['TNTRunner']#, 'InvalidRule']

class ProofMistake(Exception):
    """Base class for mistakes in a TNT proof."""
    pass

class InvalidRule(ValueError, ProofMistake):
    """This isn't the right rule."""
    pass

class NotARule(InvalidRule):
    """This rule isn't a valid rule."""
    pass

class InvalidReferral(ValueError, ProofMistake):
    """The line referred to is not correct for this derivation."""
    pass

class TooManyReferrals(InvalidReferral):
    """This refers to more lines than the rule requires."""
    pass

class SpecifyingQuantifiedVariable(ProofMistake):
    """The term which replaces a universally quantified variable
    must not contain any variable that is quantified in the statement.
    """
    pass

class GeneralizingFantasyVariable(ProofMistake):
    """No generalization is allowed in a fantasy
    on any variable which appeared free in the fantasy's premise.
    """
    pass

class MissingArgument(ProofMistake):
    """Missing one or more previous statements for this rule to be valid."""
    pass

parser = TNTParser()

class TNTRunner:
    """Validate TNT."""

    AXIOMS = parser.parse("""
    [ No natural number is a successor of 0. ]
    1 Aa:~Sa=0                axiom 1

    [ The sum of any natural number and 0 is the number. ]
    2 Aa:(a+0)=a              axiom 2

    [ S can be slipped in and out of parentheses. ]
    3 Aa:Ab:(a+Sb)=S(a+b)     axiom 3

    [ Any natural number multiplied by 0 is 0. ]
    4 Aa:(a*0)=0              axiom 4

    [ A natural number multiplied by the successor ]
    [ of another natural number is the two numbers ]
    [ multipled plus the first number. ]
    5 Aa:Ab:(a*Sb)=((a*b)+a)  axiom 5
    """)

    text = None

    def __init__(self, text: Union[str, Text, type(None)] = None):
        """Initialize the runner, optionally with text to validate.
        (If not a string, it must be an OrderedDict, ideally straight
        out of the parser.)
        """
        if isinstance(text, str):
            text = parser.parse(text)
        if text is not None:
            self.text = text

    def validate(self, text: Optional[Text] = None) -> None:
        """Validates the OrderedDict of line numbers to TNT statements.
        If no issues, returns None.
        Otherwise, raises an exception tailored to the rule.
        ``text`` can be set at initialization or passed at call.
        """
        text = text or self.text
        if not isinstance(text, OrderedDict):
            raise TypeError(f'text must be an OrderedDict, not {type(text).__name__!r}')
        if self.text is None:
            self.text = text
        try:
            for i, (lineno, line) in enumerate(text.items()):
                getattr(self, 'rule_' + line.rule.value\
                        .lower().replace(' ', '_'), self.rule_invalid)(i, line)
        except ProofMistake as exc:
            exc.args = (f"line {line.lineno}: '{str(line)}' " + exc.args[0], *exc.args[1:])
            raise

    def find_fantasy(self, fantasy: Fantasy, obj) -> bool:
        """Returns True if ``obj`` has any parent Fantasy ``fantasy``."""
        fant = obj.fantasy
        while fant is not None:
            fant = fant.fantasy
            if fant is fantasy:
                return True
        return False

    def raise_fantasy(self, idx: int, fantasy: Fantasy, rule: str, arg: str) -> bool:
        """Returns True if the loop should continue, False if no action is needed.
        Raises MissingArgument with ``rule`` and ``arg`` in the exception message
        if the fantasy ends prematurely.
        """
        try:
            if self.text.vals[idx].fantasy is not fantasy:
                if self.find_fantasy(fantasy, self.text.vals[idx]):
                    return True
                raise MissingArgument(rule + ' missing corresponding ' + arg)
            return False
        except IndexError:
            raise MissingArgument(rule + ' missing corresponding ' + arg) from None

    def rule_invalid(self, idx: int, line: Statement) -> None:
        """How did you get here?"""
        #TODO: this is commented out while not all rules are handled yet
        #raise NotARule(f'No such rule: {line.rule.value!r}')

    def rule_joining(self, idx: int, line: Statement) -> None:
        """Validate a joining."""
        # first off it has to be an and
        if not isinstance(line.formula.arg, Compound) \
               or line.formula.arg.operator is not Logic.AND:
            raise InvalidRule('formula is not an AND expression')
        arg1 = arg2 = False
        formula = line.formula.arg
        if len(line.referrals) >= 3:
            raise TooManyReferrals('too many referrals for joining: '
                                   f'expected at most 2, got {len(line.referrals)}')
        if len(line.referrals) >= 1:
            form = self.text[line.referrals[0]].formula
            if form == formula.arg1:
                arg1 = True
            if form == formula.arg2:
                arg2 = True
            else:
                raise InvalidReferral(f'line {line.referrals[0]} is not '
                                      f'an operand in {formula!s}')
        if len(line.referrals) == 2:
            form = self.text[line.referrals[1]].formula
            if not arg1 and form == formula.arg1:
                arg1 = True
            elif not arg2 and form == formula.arg2:
                arg2 = True
            else:
                raise InvalidReferral(f'line {line.referrals[1]} is not '
                                      f'an operand in {formula!s}')
        i = idx
        fantasy = line.fantasy
        while not (arg1 and arg2):
            i -= 1
            if self.raise_fantasy(i, fantasy, 'joining', f"argument {'2' if arg1 else '1'}"):
                continue
            if not arg1 and self.text.vals[i].formula == line.formula.arg.arg1:
                arg1 = True
            if not arg2 and self.text.vals[i].formula == line.formula.arg.arg2:
                arg2 = True

    def rule_separation(self, idx: int, line: Statement) -> None:
        """Validate a separation."""
        arg = None
        if len(line.referrals) >= 2:
            raise TooManyReferrals('too many referrals for separation: '
                                   f'expected at most 1, got {len(line.referrals)}')
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]].formula.arg
            if not isinstance(arg, Compound) \
                   or arg.operator is not Logic.AND:
                raise InvalidReferral('referral is not an AND expression')
            if line.formula.arg not in (arg.arg1, arg.arg2):
                raise InvalidReferral('referral does not have formula as operand')
        else:
            i = idx
            fantasy = line.fantasy
            while arg is None:
                i -= 1
                if self.raise_fantasy(i, fantasy, 'separation', 'joined formula'):
                    continue
                formula = self.text.vals[i].formula.arg
                if line.formula.arg in (formula.arg1, formula.arg2):
                    arg = formula

    def rule_axiom_1(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 1."""
        if line.formula != self.AXIOMS[1].formula:
            raise InvalidRule('not axiom 1')

    def rule_axiom_2(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 2."""
        if line.formula != self.AXIOMS[2].formula:
            raise InvalidRule('not axiom 2')

    def rule_axiom_3(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 3."""
        if line.formula != self.AXIOMS[3].formula:
            raise InvalidRule('not axiom 3')

    def rule_axiom_4(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 4."""
        if line.formula != self.AXIOMS[4].formula:
            raise InvalidRule('not axiom 4')

    def rule_axiom_5(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 5."""
        if line.formula != self.AXIOMS[5].formula:
            raise InvalidRule('not axiom 5')

    def rule_push(self, idx: int, line: Statement) -> None:
        """Validate the ``push`` fantasy rule."""
        if not isinstance(line.formula.arg, FantasyMarker):
            raise InvalidRule('cannot use push on actual formula')
        if line.formula.arg.rule != line.rule:
            raise InvalidRule('push rule used on pop statement')

    def rule_pop(self, idx: int, line: Statement) -> None:
        """Validate the ``pop`` fantasy rule."""
        if not isinstance(line.formula.arg, FantasyMarker):
            raise InvalidRule('cannot use pop on actual formula')
        if line.formula.arg.rule != line.rule:
            raise InvalidRule('pop rule used on push statement')
