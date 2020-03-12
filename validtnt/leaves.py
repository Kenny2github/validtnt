"""
The leaves found on the parse tree.
"""
from enum import Enum
from typing import Optional, Union

__all__ = ['std', 'Quantifier', 'Logic', 'Operator', 'Rule',
           'PropositionalRule', 'TNTRule', 'FantasyRule', 'Rules',
           'Term', 'Numeral', 'Variable', 'MultiTerm', 'Formula',
           'Negated', 'Quantified', 'Compound', 'Statement',
           'FantasyMarker']

# Symbols used

def std(symbol):
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
    pass

class PropositionalRule(Rule):
    """Rules of Propositional Calculus."""
    JOINING = 'joining'
    SEPARATION = 'separation'
    DOUBLE_TILDE = 'double-tilde'
    FANTASY = 'fantasy rule'
    CARRY_OVER = 'carry over line n'
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

class FantasyRule(Rule):
    """Rules of fantasies."""
    PREMISE = 'premise'
    PUSH = 'push'
    POP = 'pop'

Rules = (PropositionalRule, TNTRule, FantasyRule)

def _dataclass(cls: type) -> type:
    """Decorator to add kwarg-only __init__"""
    def __init__(self, **kwargs):
        cls = type(self)
        d = {}
        for c in cls.__mro__[::-1]:
            d.update(getattr(c, '__annotations__', {}))
        d = {k: v for k, v in d.items() if v is not None}
        for var, typ in d.items():
            if var not in kwargs and not hasattr(cls, var):
                raise TypeError(f'__init__() missing required keyword argument: {var!r}')
            elif var not in kwargs:
                kwargs[var] = getattr(cls, var)
            if getattr(typ, '__origin__', None) is Union:
                typ = typ.__args__
            if typ is not None and not isinstance(kwargs[var], typ):
                raise TypeError(f'{var!r} is not {typ.__name__!r}: {kwargs[var]!r}')
            setattr(self, var, kwargs[var])
    cls.__init__ = __init__
    return cls

# Begin actual TNT syntax

@_dataclass
class Term:
    """
    All Numerals and Variables are terms.
    A term preceded by ``S`` is also a term.
    """
    successors: int = 0

class Numeral(Term):
    """
    0 is a numeral.
    """
    pass

class Variable(Term):
    """
    ``a`` is a variable.
    If we're not being austere, so are ``b``, ``c``, ``d``, and ``e``.
    A variable followed by a prime (``′`` or ``'``) is also a variable.
    """
    letter: str
    primes: int = 0

class MultiTerm(Term):
    """If ``s`` and ``t`` are terms, then so are ``(s+t)`` and ``(s⋅t)``."""
    arg1: Term
    arg2: Term
    operator: Operator

# The above rules tell how to make *parts* of well-formed formulas;
# the following rules tell how to make *complete* well-formed formulas.

@_dataclass
class Formula:
    """
    If ``s`` and ``t`` are terms, then ``s=t`` is an atom.
    """
    arg1: Term
    arg2: Term

class Negated(Formula):
    """
    A well-formed formula preceded by a tilde is well-formed.
    """
    negations: int = 0
    arg: Formula
    arg1: None # de-registers arg1 as required arg
    arg2: None

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
    arg1: None
    arg2: None

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

# The above rules tell how to make complete formulas in TNT;
# the following rules will tell how to make complete statements.

@_dataclass
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
    lineno: Optional[int] = None
    formula: Formula
    rule: Rule
    referrals: list = []

class FantasyMarker(Formula):
    """Push or pop a fantasy."""
    rule: FantasyRule
