"""
Validate TNT.
"""
#pylint: skip-file
from typing import Union, Optional
from .parser import TNTParser
from .leaves import Text, Fantasy, Statement, Logic, Compound, FantasyMarker

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

class InvalidFantasy(ProofMistake):
    """Incorrect use of fantasies."""
    pass

parser = TNTParser()

class TNTRunner:
    """Validate TNT."""

    AXIOMS = parser.parse("""
    [ 0 is not the successor of any natural number. ]
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
        if not isinstance(text, Text):
            raise TypeError(f'text must be Text, not {type(text).__name__!r}')
        if self.text is None:
            self.text = text
        try:
            for i, (lineno, line) in enumerate(text.items()):
                getattr(self, 'rule_' + line.rule.value\
                        .lower().replace(' ', '_').replace('-', '_'),
                        self.rule_invalid)(i, line)
        except ProofMistake as exc:
            exc.args = (f"line {line.lineno}: '{str(line)}' " + exc.args[0], *exc.args[1:])
            raise

    def find_fantasy(self, fantasy: Fantasy, obj) -> bool:
        """Returns True if ``obj`` has any parent Fantasy ``fantasy``."""
        if fantasy is None:
            return True # all objects have top level as parent
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

    def at_most_refs(self, line: Statement, count: int, rule: str) -> None:
        """Raise TooManyReferrals if there are too many referrals."""
        if len(line.referrals) > count:
            raise TooManyReferrals(f'too many referrals for {rule}: expected '
                                   f'at most {count}, got {len(line.referrals)}')

    def find_arg(self, idx: int, line: Statement,
                 cmp: type(lambda: None),
                 rule: str, argname: str) -> Statement:
        """Find an argument that matches the cmp function.
        cmp takes two arguments: the ``line`` passed into this function
        and a Statement being compared with it.
        If it returns True, that is the statement that is returned.
        """
        i = idx
        fantasy = line.fantasy
        while 1:
            i -= 1
            if self.raise_fantasy(i, fantasy, rule, argname):
                continue
            if cmp(line, self.text.vals[i]):
                return self.text.vals[i]

    def rule_invalid(self, idx: int, line: Statement) -> None:
        """How did you get here?"""
        #TODO: this is a print while not all rules are handled yet
        print(NotARule(f'No such rule: {line.rule.value!r}'))

    def rule_joining(self, idx: int, line: Statement) -> None:
        """Validate a joining."""
        # first off it has to be an and
        if not isinstance(line.formula.arg, Compound) \
               or line.formula.arg.operator is not Logic.AND:
            raise InvalidRule('formula is not an AND formula')
        arg1 = arg2 = False
        formula = line.formula.arg
        self.at_most_refs(line, 2, 'joining')
        if len(line.referrals) >= 1:
            form = self.text[line.referrals[0]].formula
            if form == formula.arg1:
                arg1 = True
            elif form == formula.arg2:
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
            if self.raise_fantasy(i, fantasy, 'joining',
                                  f"argument {'2' if arg1 else '1'}"):
                continue
            if not arg1 and self.text.vals[i].formula == line.formula.arg.arg1:
                arg1 = True
            if not arg2 and self.text.vals[i].formula == line.formula.arg.arg2:
                arg2 = True

    def rule_separation(self, idx: int, line: Statement) -> None:
        """Validate a separation."""
        arg = None
        self.at_most_refs(line, 1, 'separation')
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]].formula.arg
            if not isinstance(arg, Compound) \
                   or arg.operator is not Logic.AND:
                raise InvalidReferral('referral is not an AND formula')
            if line.formula.arg not in (arg.arg1, arg.arg2):
                raise InvalidReferral('referral does not have formula as operand')
        else:
            self.find_arg(idx, line, lambda line, stmt: (
                line.formula.arg in (stmt.formula.arg.arg1,
                                     stmt.formula.arg.arg2)
            ), 'separation', 'joined formula')

    def double_tilde(self, idx: int, line: Statement) -> None:
        """Validate the double-tilde rule."""
        self.at_most_refs(line, 1, 'double-tilde')
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]]
            if str(line.formula).replace('~~', '') \
                   != str(arg.formula).replace('~~', ''):
                raise InvalidReferral('change in formula does not only '
                                      'consist of adding or removing '
                                      'double-tildes')
        else:
            self.find_arg(idx, line, lambda line, stmt: (
                # if they are the same with all double-tildes removed,
                # then it follows the rule
                str(line.formula).replace('~~', '')
                == str(stmt.formula).replace('~~', '')
            ), 'double-tilde', 'previous well-formed string')

    def rule_fantasy_rule(self, idx: int, line: Statement) -> None:
        """Validate the fantasy rule."""
        self.at_most_refs(line, 0, 'fantasy rule')
        i = idx
        arg = None
        try:
            while arg is None:
                i -= 1
                if self.text.vals[i].fantasy is not line.fantasy:
                    if self.text.vals[i].fantasy is None:
                        continue
                    if self.text.vals[i].fantasy.fantasy is line.fantasy:
                        arg = self.text.vals[i]
                    elif self.find_fantasy(line.fantasy, self.text.vals[i]):
                        continue
                    else:
                        raise MissingArgument('fantasy rule missing '
                                              'corresponding fantasy')
        except IndexError:
            raise MissingArgument('fantasy rule missing corresponding fantasy')
        fant = arg.fantasy
        if fant.premise.formula != line.formula.arg.arg1:
            raise InvalidFantasy('premise of fantasy does not match '
                                 'condition of IMPLIES formula')
        if fant.outcome.formula != line.formula.arg.arg2:
            raise InvalidFantasy('outcome of fantasy does not match '
                                 'result of IMPLIES formula')

    def rule_carry(self, idx: int, line: Statement) -> None:
        """Validate the carry rule."""
        self.at_most_refs(line, 1, 'carry')
        # the parser is guaranteed to provide the single referral,
        # because the rule syntax requires it
        arg = self.text[line.referrals[0]]
        if not self.find_fantasy(arg.fantasy, line):
            raise InvalidReferral('carrying over from a fantasy '
                                  'that is not a direct parent')
        if arg.formula != line.formula:
            raise InvalidReferral('carried formula does not match '
                                  'formula being carried')

    def rule_detachment(self, idx: int, line: Statement) -> None:
        """Validate a detachment."""
        cond = expr = None
        formula = line.formula.arg
        self.at_most_refs(line, 2, 'detachment')
        if len(line.referrals) >= 1:
            form = self.text[line.referrals[0]].formula
            if isinstance(form.arg, Compound) \
                   and form.arg.operator is Logic.IMPLIES \
                   and form.arg.arg2 == formula:
                expr = self.text[line.referrals[0]]
            else:
                cond = self.text[line.referrals[0]] # assume it's the condition
        if len(line.referrals) == 2:
            form = self.text[line.referrals[1]].formula
            if (
                    not expr
                    and isinstance(form.arg, Compound)
                    and form.arg.operator is Logic.IMPLIES
                    and form.arg.arg2 == formula
                    # cond is already found, check it
                    and form.arg.arg1 == self.text[line.referrals[0]].formula
            ):
                expr = self.text[line.referrals[1]]
            elif not cond and form == self.text[line.referrals[0]] \
                     .formula.arg.arg1:
                cond = self.text[line.referrals[1]]
            else:
                raise InvalidReferral('invalid detachment')
        if not expr:
            expr = self.find_arg(idx, line, lambda line, stmt: (
                isinstance(stmt.formula.arg, Compound)
                and stmt.formula.arg.operator is Logic.IMPLIES
                and stmt.formula.arg.arg2 == line.formula
            ), 'detachment', 'formula to detach from')
        if not cond:
            cond = self.find_arg(idx, line, lambda line, stmt: (
                stmt.formula == expr.formula.arg.arg1
            ), 'detachment', 'condition to detach upon')

    # BIG TODO:
    # Implement some system of matching the x and y
    # in the three rules marking strings as interchangeable
    # because those strings can appear anywhere...
    # Then apply it to all three rules with just a pattern.

    def rule_axiom_1(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 1."""
        self.at_most_refs(line, 0, 'axiom 1')
        if line.formula != self.AXIOMS[1].formula:
            raise InvalidRule('not axiom 1')

    def rule_axiom_2(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 2."""
        self.at_most_refs(line, 0, 'axiom 2')
        if line.formula != self.AXIOMS[2].formula:
            raise InvalidRule('not axiom 2')

    def rule_axiom_3(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 3."""
        self.at_most_refs(line, 0, 'axiom 3')
        if line.formula != self.AXIOMS[3].formula:
            raise InvalidRule('not axiom 3')

    def rule_axiom_4(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 4."""
        self.at_most_refs(line, 0, 'axiom 4')
        if line.formula != self.AXIOMS[4].formula:
            raise InvalidRule('not axiom 4')

    def rule_axiom_5(self, idx: int, line: Statement) -> None:
        """Validate a statement as axiom 5."""
        self.at_most_refs(line, 0, 'axiom 5')
        if line.formula != self.AXIOMS[5].formula:
            raise InvalidRule('not axiom 5')

    def rule_push(self, idx: int, line: Statement) -> None:
        """Validate the ``push`` fantasy rule."""
        self.at_most_refs(line, 0, 'push')
        if not isinstance(line.formula.arg, FantasyMarker):
            raise InvalidRule('cannot use push on actual formula')
        if line.formula.arg.rule != line.rule:
            raise InvalidRule('push rule used on pop statement')

    def rule_pop(self, idx: int, line: Statement) -> None:
        """Validate the ``pop`` fantasy rule."""
        self.at_most_refs(line, 0, 'pop')
        if not isinstance(line.formula.arg, FantasyMarker):
            raise InvalidRule('cannot use pop on actual formula')
        if line.formula.arg.rule != line.rule:
            raise InvalidRule('pop rule used on push statement')
