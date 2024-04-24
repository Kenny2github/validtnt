"""
The leaves found on the parse tree.
"""
from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum
from collections import OrderedDict

__all__ = ['std', 'Quantifier', 'Logic', 'Operator', 'Rule',
           'PropositionalRule', 'TNTRule', 'FantasyRule', 'Rules',
           'Term', 'Numeral', 'Variable', 'Successed', 'MultiTerm', 'Formula',
           'Negated', 'Quantified', 'Compound', 'Wrapper', 'Statement',
           'FantasyMarker', 'Fantasy', 'Text']

# Symbols used

def std(symbol: str) -> str:
    """Normalize an alternate symbol to the standard one."""
    return {
        "'": '′',
        '*': '⋅',
        '.': '⋅',
        '!': '~',
        '&': '∧',
        '^': '∧',
        '|': '∨',
        'V': '∨',
        'v': '∨',
        '→': '⊃',
        ']': '⊃',
        'A': '∀',
        'E': '∃',
    }.get(symbol, symbol)

class Quantifier(Enum):
    """Quantifiers used before formulas."""
    EXISTS = '∃'
    ALL = '∀'

class Logic(Enum):
    """Logic operators used in compound formulas."""
    AND = '∧'
    OR = '∨'
    IMPLIES = '⊃'

class Operator(Enum):
    """Arithmetic operators used in terms."""
    ADD = '+'
    MULT = '⋅'

# Rules of the system

class Rule(Enum):
    """Rules of the system."""

class PropositionalRule(Rule):
    """Rules of Propositional Calculus."""
    JOINING = 'joining'
    SEPARATION = 'separation'
    DOUBLE_TILDE = 'double-tilde'
    FANTASY = 'fantasy rule'
    CARRY_OVER = 'carry'
    DETACHMENT = 'detachment'
    CONTAPOSITIVE = 'contrapositive'
    DE_MORGAN = 'De Morgan'
    SWITCHEROO = 'switcheroo'

class TNTRule(Rule):
    """Rules of TNT."""
    SPECIFICATION = 'specification'
    GENERALIZATION = 'generalization'
    INTERCHANGE = 'interchange'
    EXISTENCE = 'existence'
    SYMMETRY = 'symmetry'
    TRANSITIVITY = 'transitivity'
    ADD_S = 'add S'
    DROP_S = 'drop S'
    INDUCTION = 'induction'
    AXIOM1 = 'axiom 1'
    AXIOM2 = 'axiom 2'
    AXIOM3 = 'axiom 3'
    AXIOM4 = 'axiom 4'
    AXIOM5 = 'axiom 5'
    PREMISE = 'premise'

class FantasyRule(Rule):
    """Rules of fantasies."""
    PUSH = 'push'
    POP = 'pop'

Rules = PropositionalRule | TNTRule | FantasyRule

# Begin actual TNT syntax

class Term:
    """
    All Numerals and Variables are terms.
    """
    def __hash__(self) -> int:
        return hash(str(self))
    def __eq__(self, value: object) -> bool:
        return hash(self) == hash(value)

class Numeral(Term):
    """
    0 is a numeral.
    """
    _instance = None
    def __new__(cls):
        """There is only one 0."""
        if cls._instance is None:
            cls._instance = Term.__new__(cls)
        return cls._instance

    def __str__(self):
        return '0'

@dataclass(kw_only=False, eq=False)
class Variable(Term):
    """
    ``a`` is a variable.
    If we're not being austere, so are ``b``, ``c``, ``d``, and ``e``.
    A variable followed by a prime (``′`` or ``'``) is also a variable.
    """
    _instances = {}
    def __new__(cls, *, letter, primes=0):
        """Make each possible variable a singleton."""
        if letter not in 'abcde':
            raise ValueError('Only ``a``, ``b``, ``c``, ``d``, and ``e`` '
                             'are valid variable letters.')
        if (letter, primes) not in cls._instances:
            cls._instances[letter, primes] = Term.__new__(cls)
        return cls._instances[letter, primes]
    letter: str
    primes: int = 0
    def __str__(self):
        return self.letter + '′' * self.primes

@dataclass(kw_only=False, eq=False)
class Successed(Term):
    """A term preceded by ``S`` is also a term."""
    arg: Term
    successors: int = 1
    def __str__(self):
        return 'S' * self.successors + str(self.arg)

@dataclass(kw_only=False, eq=False)
class MultiTerm(Term):
    """If ``s`` and ``t`` are terms, then so are ``(s+t)`` and ``(s⋅t)``."""
    arg1: Term
    arg2: Term
    operator: Operator
    def __str__(self):
        return '({}{}{})'.format(
            str(self.arg1),
            self.operator.value,
            str(self.arg2),
        )

# The above rules tell how to make *parts* of well-formed formulas;
# the following rules tell how to make *complete* well-formed formulas.

@dataclass(kw_only=False, eq=False)
class Formula:
    """
    If ``s`` and ``t`` are terms, then ``s=t`` is an atom.
    """
    arg1: Term
    arg2: Term
    def __str__(self):
        return str(self.arg1) + '=' + str(self.arg2)
    def __hash__(self) -> int:
        return hash(str(self))
    def __eq__(self, value: object) -> bool:
        return hash(self) == hash(value)

@dataclass(kw_only=False, eq=False)
class Negated(Formula):
    """
    A well-formed formula preceded by a tilde is well-formed.
    """
    arg: Formula
    negations: int = 1
    arg1: None = field(default=None, init=False)
    arg2: None = field(default=None, init=False)
    def __str__(self):
        return '~' * self.negations + str(self.arg)

@dataclass(kw_only=False, eq=False)
class Quantified(Formula):
    """
    If ``u`` is a variable,
    and ``x`` is a well-formed formula in which ``u`` is free,
    then the following strings are well-formed formulas:
    ``∃u:x`` and ``∀u:x``
    """
    variable: Variable
    quantifier: Quantifier
    arg: Formula
    arg1: None = field(default=None, init=False)
    arg2: None = field(default=None, init=False)
    def __str__(self):
        return '{}{}:{}'.format(
            self.quantifier.value,
            str(self.variable),
            str(self.arg),
        )

@dataclass(kw_only=False, eq=False)
class Compound(Formula):
    """
    If ``x`` and ``y`` are well-formed formulas,
    and provided that no variable which is free in one is quantified
    in the other, then the following are all well-formed formulas:
    ``<x∧y>``, ``<x∨y>``, ``<x⊃y>``
    """
    arg1: Formula
    arg2: Formula
    operator: Logic
    def __str__(self):
        return '<{}{}{}>'.format(
            str(self.arg1),
            self.operator.value,
            str(self.arg2),
        )

def _recurse_vars(formula: Formula, quant: dict, free: set) -> None:
    """Compute which variables are quantified by recursing into each argument."""
    if isinstance(formula, (Negated, Quantified)):
        if isinstance(formula, Quantified):
            quant[formula.variable] = formula.quantifier
        _recurse_vars(formula.arg, quant, free)
    elif isinstance(formula, Compound):
        _recurse_vars(formula.arg1, quant, free)
        _recurse_vars(formula.arg2, quant, free)
    elif isinstance(formula, Variable):
        if formula not in quant:
            free.add(formula)

@dataclass(kw_only=False, eq=False)
class Wrapper(Formula):
    """Wrapper class to store variable quantification information."""
    arg1: None = field(default=None, init=False)
    arg2: None = field(default=None, init=False)
    arg: Formula
    free: set[Variable]
    quantified: dict # Variable: Quantifier

    def __init__(self, *, arg: Formula):
        """Find free variables and register them."""
        if not isinstance(arg, Formula):
            raise TypeError(f"'arg' is not 'Formula': {arg!r}")
        self.free = set()
        self.quantified = {}
        self.arg = arg
        _recurse_vars(self.arg, self.quantified, self.free)

    def __str__(self):
        return str(self.arg)

@dataclass(kw_only=False, eq=False)
class FantasyMarker(Formula):
    """Push or pop a fantasy."""
    rule: FantasyRule
    arg1: None = field(default=None, init=False)
    arg2: None = field(default=None, init=False)
    def __str__(self):
        if self.rule is FantasyRule.PUSH:
            return '['
        return ']'

# The above rules tell how to make complete formulas in TNT;
# the following rules will tell how to make complete statements.

@dataclass(kw_only=False, eq=False)
class Statement:
    """
    A statement consists of:
    1. An optional line number (to refer to in later steps).
       If not specified, the index of the line in the text is used.
       Behavior is undefined if automatic numbers and manual numbers are mixed;
       best practice is to use manual numbers, possibly BASIC-style.
    2. A well-formed formula
    3. A rule of Propositional Calculus proving the theoremhood of the formula
    4. Optional line number(s) to refer to as arguments of the rule.
       (Note: When carrying over from a fantasy, the line number in the rule
       specification is stored as the first (and only) referral.)
    Separated by spaces.
    """
    formula: Wrapper | FantasyMarker
    rule: Rule
    lineno: int | None = None
    referrals: list = field(default_factory=list)
    fantasy: Fantasy | None = None
    def __str__(self):
        result = ''
        # line number
        if self.lineno is not None:
            result += str(self.lineno) + ' '
        # indentation in fantasy
        count = 0
        fant = self.fantasy
        while fant is not None:
            count += 1
            fant = fant.fantasy
        result += '\t' * count
        # formula
        result += str(self.formula)
        if isinstance(self.formula, FantasyMarker):
            result += '\t'
        # rule
        refs = self.referrals
        if self.rule is PropositionalRule.CARRY_OVER:
            result += '\tcarry over line ' + str(self.referrals[0])
            # the first referral is part of the rule for this one
            refs = self.referrals[1:]
        else:
            result += '\t' + self.rule.value
        # referrals
        if refs:
            result += ' (line ' + ', '.join(map(str, refs)) + ')'
        return result

class Text(OrderedDict[int, Statement]):
    """Mapping of line numbers to TNT statements."""
    vals: list[Statement] = []
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.flash()
    def flash(self):
        """Store a list of items."""
        self.vals = list(self.values())
    def __str__(self):
        return '\n'.join(map(str, self.values()))

@dataclass(kw_only=False, eq=False)
class Fantasy:
    """
    A fantasy is a way of manufacturing theorems from nothing.
    A fantasy starts out with a premise P, which is a well-formed TNT string.
    This premise is assumed to be a theorem within the fantasy.
    Some derivation work is done from there,
    and a final theorem Q is arrived at at the end of the fantasy.
    Finally, <P⊃Q> is now truly a theorem, outside of the fantasy.
    """
    @property
    def premise(self) -> Statement:
        """The premise is the statement initially assumed to be true.
        It is the first statement within a fantasy.
        """
        for stmt in self.content.values():
            if stmt.rule == TNTRule.PREMISE:
                return stmt
        # if somehow no statement is explicitly marked as the premise,
        # return the first statement in the fantasy
        return list(self.content.values())[0]
    @property
    def outcome(self) -> Statement:
        """The outcome is the statement ultimately derived from the premise.
        It is what the premise implies, if it were true.
        It is the last statement within a fantasy.
        """
        # always last statement, no special rule for it
        return list(self.content.values())[-1]
    content: Text
    fantasy: Fantasy | None = None # fantasy can be inside a fantasy
    lineno: int | None = None

    def __str__(self):
        return str(self.content)
