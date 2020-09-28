"""
Validate TNT.
"""
#pylint: skip-file
import re
from difflib import SequenceMatcher
from typing import Union, Optional
from .parser import TNTParser
from .leaves import Text, Fantasy, Statement, Logic, Compound, FantasyMarker, \
     Quantifier

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
            try:
                if cmp(line, self.text.vals[i]):
                    return self.text.vals[i]
            except ProofMistake:
                continue

    def rule_invalid(self, idx: int, line: Statement) -> None:
        """How did you get here?"""
        #TODO: this is a print while not all rules are handled yet
        print(NotARule(f'No such rule: {line.rule.value!r}'))

    def rule_joining(self, idx: int, line: Statement) -> None:
        """If ``x`` and ``y`` are theorems, then ``<x∧y>`` is a theorem."""
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
        """If ``<x∧y>`` is a theorem, then both ``x`` and ``y`` are theorems."""
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

    def rule_double_tilde(self, idx: int, line: Statement) -> None:
        """The string ``~~`` can be deleted from any theorem.
        It can also be inserted into any theorem,
        provided that the resulting string is itself well-formed.
        """
        self.at_most_refs(line, 1, 'double-tilde')
        def referral_matches(line: Statement, arg: Statement) -> bool:
            return str(line.formula).replace('~~', '') \
                   == str(arg.formula).replace('~~', '')
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]]
            if not referral_matches(line, arg):
                raise InvalidReferral('change in formula does not only '
                                      'consist of adding or removing '
                                      'double-tildes')
        else:
            self.find_arg(idx, line, referral_matches, 'double-tilde',
                          'previous well-formed string')

    def rule_fantasy_rule(self, idx: int, line: Statement) -> None:
        """If ``y`` can be derived when ``x`` is assumed to be a theorem,
        then ``<x⊃y>`` is a theorem.
        """
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
        """Inside a fantasy, any theorem from the "reality" one level higher
        can be brought in and used.
        """
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
        """If ``x`` and ``<x⊃y>`` are both theorems, then ``y`` is a theorem."""
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

    def rule_specification(self, idx: int, line: Statement) -> None:
        """Suppose ``u`` is a variable which occurs inside the string ``x``.
        If the string ``∀u:x`` is a theorem, then so is ``x``, and so are any
        strings made from ``x`` by replacing ``u``, wherever it occurs, by one
        and the same term.
        (Restriction: The term which replaces ``u`` must not contain any
        variable that is quantified in ``x``.)
        """
        self.at_most_refs(line, 1, 'specification')
        def referral_matches(line: Statement, arg: Statement) -> bool:
            # find variables quantified in arg but not in line
            spec = set(arg.formula.quantified) - set(line.formula.quantified)
            if len(spec) != 1:
                raise InvalidReferral('cannot specify multiple variables '
                                      'simultaneously')
            spec = spec.pop()
            if arg.formula.quantified[spec] != Quantified.ALL:
                raise InvalidReferral('cannot specify existence quantifier')
            prev = str(arg.formula)
            after = str(line.formula)
            var = str(spec)
            # replace original var with invalid one to prevent SM from
            # breaking up the replacement with an "equal" operation
            # make sure the character counts match, though
            sm = SequenceMatcher(None, prev, after.replace(var, 'q' * len(var)))
            possibilities = set()
            for tag, i1, i2, j1, j2 in sm.get_opcodes():
                if tag == 'delete':
                    if i2 - i1 != len(var) + 2: # quantifiers are always 'Ax:'
                        raise InvalidReferral('non-quantifier deleted')
                    deleted = prev[i1:i2]
                    if deleted[0] != Quantifier.ALL.value:
                        raise InvalidReferral('invalid quantifier deleted')
                    if deleted[1:-1] != var:
                        raise InvalidReferral('quantifier for wrong '
                                              'variable deleted')
                    if deleted[-1] != ':':
                        raise InvalidReferral('how did you do this?')
                    prev = prev.replace(deleted, '', 1)
                if tag == 'replace':
                    repl = after[j1:j2]
                    # it might be a match if (a+b).replace(a, b) == (b+b)
                    if prev.replace(var, repl) == after:
                        possibilities.add(repl)
            if prev == after and not possibilities: # after deletion(s)
                return True
            # ambiguity is treated as failure
            if len(possibilities) != 1:
                raise InvalidReferral('ambiguous specification')
            repl = possibilities.pop()
            for var in line.formula.quantified:
                if str(var) in repl:
                    raise SpecifyingQuantifiedVariable(
                        'the term which replaces the specified variable '
                        'must not contain any variable that is quantified '
                        'in the resulting string'
                    )
            return True
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]]
            if not referral_matches(line, arg):
                raise InvalidReferral('invalid specification')
        else:
            self.find_arg(idx, line, referral_matches, 'specification',
                          'previous general statement')

    def rule_generalization(self, idx: int, line: Statement) -> None:
        """Suppose ``x`` is a theorem in which ``u``, a variable, occurs free.
        Then ``∀u:x` is a theorem.
        (Restriction: No generalization is allowed in a fantasy on any variable
        which appeared free in the fantasy's premise.)
        """
        self.at_most_refs(line, 1, 'generalization')
        def referral_matches(line: Statement, arg: Statement) -> bool:
            # find variables quantified in line but not in arg
            gen = set(line.formula.quantified) - set(arg.formula.quantified)
            if len(gen) != 1:
                raise InvalidReferral('cannot generalize multiple variables '
                                      'simultaneously')
            gen = gen.pop()
            if line.formula.quantified[gen] != Quantified.ALL:
                raise InvalidReferral('cannot generalize existence quantifier')
            prev = str(arg.formula)
            after = str(line.formula)
            var = str(gen)
            if var not in prev or var not in after:
                raise InvalidReferral('quantifying nonexistent variable')
            sm = SequenceMatcher(None, prev, after.replace(var, 'q' * len(var)))
            for tag, i1, i2, j1, j2 in sm.get_opcodes():
                if tag == 'insert':
                    if i2 - i1 != len(var) + 2: # quantifiers are always 'Ax:'
                        raise InvalidReferral('non-quantifier inserted')
                    inserted = after[j1:j2]
                    if inserted[0] != Quantifier.ALL.value:
                        raise InvalidReferral('invalid quantifier inserted')
                    if inserted[1:-1] != var:
                        raise InvalidReferral('quantifier for wrong '
                                              'variable inserted')
                    if inserted[-1] != ':':
                        raise InvalidReferral('how did you do this?')
                    if line.fantasy is not None:
                        premise = line.fantasy.premise
                        if gen in premise.formula.free:
                            raise GeneralizingFantasyVariable(
                                'no generalization is allowed in a fantasy on '
                                'any variable which appeared free in the '
                                'premise of the fantasy'
                            )
                    # generalization consists of a single insertion, so
                    # if all checks are passed then this is the one.
                    return True
            # if no insertions were made, no generalization was either
            return False
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]]
            if not referral_matches(line, arg):
                raise InvalidReferral('invalid generalization')
        else:
            self.find_arg(idx, line, referral_matches, 'generalization',
                          'previous statement with free variables')

    def rule_interchange(self, idx: int, line: Statement) -> None:
        """Suppose ``u`` is a variable. Then the strings ``∀u:~`` and ``~∃u:``
        are interchangeable anywhere inside any theorem.
        """
        self.at_most_refs(line, 1, 'interchange')
        def referral_matches(line: Statement, arg: Statement) -> bool:
            # replace all quantifiers with the same dummy invalid quantifier
            # for the sake of comparison
            return re.sub(
                '∀([a-e]′*):~|~∃([a-e]′*):',
                r'Q\1\2:', str(line.formula)
            ) == re.sub(
                '∀([a-e]′*):~|~∃([a-e]′*):',
                r'Q\1\2:', str(arg.formula)
            )
        if len(line.referrals) == 1:
            arg = self.text[line.referrals[0]]
            if not referral_matches(line, arg):
                raise InvalidReferral('invalid interchange')
        else:
            self.find_arg(idx, line, referral_matches, 'interchange',
                          'valid interchange source')

    def rule_existence(self, idx: int, line: Statement) -> None:
        """Suppose a term (which may contain variables as long as they are free)
        appears once, or multiply, in a theorem. Then any (or several, or all)
        of the appearances of the term may be replaced by a variable which
        otherwise does not occur in the theorem, and the corresponding
        existential quantifier must be placed in front.
        """
        self.at_most_refs(line, 1, 'existence')
        def referral_matches(line: Statement, arg: Statement) -> bool:
            # find variables quantified in line but not in arg
            # this is very similar to specification and generalization,
            # but has elements of both and neither
            exist = set(line.formula.quantified) - set(arg.formula.quantified)
            if len(exist) != 1:
                raise InvalidReferral('cannot assert existence of multiple '
                                      'variables simultaneously')
            # TODO: implement this

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
