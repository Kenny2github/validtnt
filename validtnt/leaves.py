"""
The leaves found on the parse tree.
"""
from __future__ import annotations
from enum import Enum
from collections import OrderedDict
from typing import Optional, Union, Set, get_type_hints

__all__ = ['std', 'Quantifier', 'Logic', 'Operator', 'Rule',
           'PropositionalRule', 'TNTRule', 'FantasyRule', 'Rules',
           'Term', 'Numeral', 'Variable', 'MultiTerm', 'Formula',
           'Negated', 'Quantified', 'Compound', 'Statement',
           'FantasyMarker', 'Fantasy', 'Text']

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

Rules = (PropositionalRule, TNTRule, FantasyRule)

def _dataclass(cls: type) -> type:
    """Decorator to add kwarg-only __init__"""
    def __init__(self, **kwargs):
        cls = type(self)
        d = get_type_hints(cls)
        for var, typ in d.items():
            if typ in (None, type(None)):
                continue
            if var not in kwargs and not hasattr(cls, var):
                raise TypeError('__init__() missing required '
                                f'keyword argument: {var!r}')
            elif var not in kwargs:
                kwargs[var] = getattr(cls, var)
            if getattr(typ, '__origin__', None) is Union:
                typ = typ.__args__
            if typ is not None and not isinstance(kwargs[var], typ):
                raise TypeError(f'{var!r} is not {typ.__name__!r}: {kwargs[var]!r}')
            setattr(self, var, kwargs[var])
    def __repr__(self):
        ret = type(self).__name__
        ret += '('
        for k, v in self.__dict__.items():
            ret += f'{k!s}={v!r}, '
        if self.__dict__:
            ret = ret[:-2]
        ret += ')'
        return ret
    def __hash__(self):
        return hash(str(self))
    def __eq__(self, other):
        return hash(self) == hash(other)
    cls.__init__ = __init__
    cls.__repr__ = __repr__
    cls.__hash__ = __hash__
    cls.__eq__ = __eq__
    return cls

# Begin actual TNT syntax

@_dataclass
class Term:
    """
    All Numerals and Variables are terms.
    """
    def __init__(self):
        raise TypeError('This class can only be extended.')

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

class Successed(Term):
    """A term preceded by ``S`` is also a term."""
    successors: int = 1
    arg: Term
    def __str__(self):
        return 'S' * self.successors + str(self.arg)

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

@_dataclass
class Formula:
    """
    If ``s`` and ``t`` are terms, then ``s=t`` is an atom.
    """
    arg1: Term
    arg2: Term
    def __str__(self):
        return str(self.arg1) + '=' + str(self.arg2)

class Negated(Formula):
    """
    A well-formed formula preceded by a tilde is well-formed.
    """
    negations: int = 0
    arg: Formula
    arg1: None # de-registers arg1 as required arg
    arg2: None
    def __str__(self):
        return '~' * self.negations + str(self.arg)

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
    def __str__(self):
        return '{}{}:{}'.format(
            self.quantifier.value,
            str(self.variable),
            str(self.arg),
        )

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

def recurse_vars(formula, quant, free):
    if hasattr(formula, 'arg'):
        if isinstance(formula, Quantified):
            quant[formula.variable] = formula.quantifier
        recurse_vars(formula.arg, quant, free)
    elif hasattr(formula, 'arg1'):
        recurse_vars(formula.arg1, quant, free)
        recurse_vars(formula.arg2, quant, free)
    elif isinstance(formula, Variable):
        if formula not in quant:
            free.add(formula)

class Wrapper(Formula):
    """Wrapper class to store variable quantification information."""
    arg1: None
    arg2: None
    arg: Formula
    free: Set[Variable]
    quantified: dict # Variable: Quantifier

    def __init__(self, *, arg):
        """Find free variables and register them."""
        self.free = set()
        self.quantified = {}
        self.arg = arg
        recurse_vars(self.arg, self.quantified, self.free)

    def __str__(self):
        return str(self.arg)

class FantasyMarker(Formula):
    """Push or pop a fantasy."""
    rule: FantasyRule
    arg1: None
    arg2: None
    def __str__(self):
        if self.rule is FantasyRule.PUSH:
            return '['
        return ']'

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
    formula: Union[Wrapper, FantasyMarker]
    rule: Rule
    referrals: list = []
    fantasy: Optional[Fantasy] = None #pylint: disable=used-before-assignment
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

class Text(OrderedDict):
    """Mapping of line numbers to TNT statements."""
    vals: list = []
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.flash()
    def flash(self):
        """Store a list of items."""
        self.vals = list(self.values())
    def __str__(self):
        return '\n'.join(map(str, self.values()))

@_dataclass
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
    fantasy: Optional[Fantasy] = None # fantasy can be inside a fantasy
    lineno: Optional[int] = None

    def __str__(self):
        return str(self.content)
