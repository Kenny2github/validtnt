"""Test all of validtnt"""
import unittest
import validtnt
from validtnt import std

class TestLeaves(unittest.TestCase):
    """Test the leaves of the parser tree"""

    def test_std(self):
        """Ensure that std() normalizes correctly"""
        self.assertEqual(std('*'), std('.'), "product symbols don't match")
        self.assertEqual(std('&'), validtnt.Logic.AND.value, "wrong and symbol")
        self.assertEqual(std('&'), std('^'), "and symbols don't match")
        self.assertEqual(std('|'), validtnt.Logic.OR.value, "wrong or symbol")
        self.assertEqual(std('|'), std('V'), "or symbols don't match")
        self.assertEqual(std('V'), std('v'), "or symbols don't match")
        self.assertEqual(std('|'), std('v'), "or symbols don't match")
        self.assertEqual(std(']'), validtnt.Logic.IMPLIES.value,
                         "wrong implies symbol")
        self.assertEqual(std('→'), std(']'), "implies symbols don't match")
        self.assertEqual(std('A'), validtnt.Quantifier.ALL.value,
                         "wrong for-all symbol")
        self.assertEqual(std('E'), validtnt.Quantifier.EXISTS.value,
                         "wrong exists symbol")

    def test_terms(self):
        """Test Term and its subclasses"""
        zero = validtnt.Numeral()
        self.assertIsInstance(zero, validtnt.Term, "all numerals are terms")
        self.assertEqual(str(zero), '0', "numeral should be zero")
        self.assertIs(zero, validtnt.Numeral(), "there is only one zero")

        with self.assertRaises(TypeError, msg="letter should be str only"):
            validtnt.Variable(letter=1)
        with self.assertRaises(ValueError, msg="letter should be abcde only"):
            validtnt.Variable(letter='x')
        with self.assertRaises(TypeError, msg="primes should be int only"):
            validtnt.Variable(letter='a', primes='1')
        a = validtnt.Variable(letter='a')
        self.assertIsInstance(a, validtnt.Term, "all variables are terms")
        self.assertEqual(str(a), 'a', "this variable should be a")
        self.assertIs(a, validtnt.Variable(letter='a'),
                      "each possible variable should be a singleton")
        aprime = validtnt.Variable(letter='a', primes=2)
        self.assertIsInstance(aprime, validtnt.Term, "all variables are terms")
        self.assertEqual(str(aprime), 'a\N{PRIME}\N{PRIME}', "missing primes")
        self.assertIs(aprime, validtnt.Variable(letter='a', primes=2),
                      "each possible variable should be a singleton")

        with self.assertRaises(TypeError, msg="successors should be int only"):
            validtnt.Successed(successors='1', arg=zero)
        with self.assertRaises(TypeError, msg="arg should be term only"):
            validtnt.Successed(successors=1, arg=0)
        S0 = validtnt.Successed(successors=1, arg=zero)
        self.assertIsInstance(S0, validtnt.Term, "``S``term is also a term")
        self.assertEqual(str(S0), 'S0', "wrong representation of S0")
        # we've left the realm of singletons now
        SSa = validtnt.Successed(successors=2, arg=a)
        self.assertIsInstance(SSa, validtnt.Term,
                              "any term can follow any number of ``S``es")
        self.assertEqual(str(SSa), 'SSa', "wrong representation of SSa")

        with self.assertRaises(TypeError, msg="args should be terms only"):
            validtnt.MultiTerm(arg1='a', arg2=0, operator='+')
        aplusb = validtnt.MultiTerm(arg1=a, arg2=zero, operator=validtnt.Operator.ADD)
        self.assertIsInstance(aplusb, validtnt.Term, "multiterms are terms")
        self.assertEqual(str(aplusb), '(a+0)', "wrong representation of a+0")
        atimesb = validtnt.MultiTerm(arg1=a, arg2=zero, operator=validtnt.Operator.MULT)
        self.assertEqual(str(atimesb), '(a\N{DOT OPERATOR}0)',
                         "wrong representation of a*0")

    def test_formula(self):
        """Test Formula and its subclasses"""
        a = validtnt.Variable(letter='a')
        b = validtnt.Variable(letter='b')

        with self.assertRaises(TypeError, msg="args should be Terms only"):
            validtnt.Formula(arg1='a', arg2='b')
        with self.assertRaises(TypeError, msg="args shouldn't be Formulas"):
            validtnt.Formula(arg1=validtnt.Formula(arg1=a, arg2=b),
                             arg2=validtnt.Formula(arg1=b, arg2=a))
        aeqb = validtnt.Formula(arg1=a, arg2=b)
        self.assertEqual(str(aeqb), 'a=b',
                         "wrong representation for formula of a and b")

        with self.assertRaises(TypeError, msg="arg should be Formula only"):
            validtnt.Negated(arg='a=b')
        with self.assertRaises(TypeError, msg="arg shouldn't be Term"):
            validtnt.Negated(arg=a)
        notaeqb = validtnt.Negated(arg=aeqb)
        self.assertIsInstance(notaeqb, validtnt.Formula,
                              "negated formula is formula")
        self.assertEqual(str(notaeqb), '~a=b',
                         "wrong representation for not a=b")

        with self.assertRaises(TypeError, msg="this call should error"):
            validtnt.Quantifier(arg='a=b', variable='a', quantifier='A')
        allaeqb = validtnt.Quantified(arg=aeqb, variable=a,
                                      quantifier=validtnt.Quantifier.ALL)
        self.assertIsInstance(allaeqb, validtnt.Formula,
                              "quantified formula is formula")
        self.assertEqual(str(allaeqb), '\N{FOR ALL}a:a=b',
                         "wrong representation for all a=b")

        with self.assertRaises(TypeError, msg="args should be Formulas only"):
            validtnt.Compound(arg1=a, arg2=b, operator=validtnt.Operator.ADD)
        notandall = validtnt.Compound(arg1=notaeqb, arg2=allaeqb,
                                      operator=validtnt.Logic.AND)
        self.assertIsInstance(notandall, validtnt.Formula,
                              "compound formula is formula")
        self.assertEqual(str(notandall), '<~a=b\N{LOGICAL AND}\N{FOR ALL}a:a=b>',
                         "wrong representation for <~a=b&Aa:a=b>")

        with self.assertRaises(TypeError, msg="arg should be Formula only"):
            validtnt.Wrapper(arg=a)
        wrapped = validtnt.Wrapper(arg=notandall)
        self.assertIsInstance(wrapped, validtnt.Formula,
                              "wrapped formula is formula")
        self.assertEqual(str(wrapped), str(notandall), "wrapper shouldn't add")
        self.assertIs(wrapped.quantified.get(a, None), validtnt.Quantifier.ALL,
                      "variable a should be quantified ALL")
        self.assertIn(b, wrapped.free, "variable b should be free")

        with self.assertRaises(TypeError, msg="rule should be FantasyRule only"):
            validtnt.FantasyMarker(rule='push')
        push = validtnt.FantasyMarker(rule=validtnt.FantasyRule.PUSH)
        self.assertIsInstance(push, validtnt.Formula,
                              "fantasy marker is formula, technically")
        self.assertEqual(str(push), '[', "wrong representation for push")

    def test_statement(self):
        """Test the Statement class"""
        formula = validtnt.Formula(arg1=validtnt.Variable(letter='a'),
                                   arg2=validtnt.Variable(letter='b'))
        formula = validtnt.Wrapper(arg=formula)
        with self.assertRaises(TypeError, msg="lineno should be int? only"):
            validtnt.Statement(lineno='0', formula=formula,
                               rule=validtnt.PropositionalRule.DETACHMENT)
        with self.assertRaises(TypeError, msg="formula should be Formula only"):
            validtnt.Statement(formula=formula.arg.arg1, rule=validtnt.TNTRule.ADD_S)
        with self.assertRaises(TypeError, msg="rule should be Rule only"):
            validtnt.Statement(formula=formula, rule='add S')
        with self.assertRaises(TypeError, msg="referrals should be a list"):
            validtnt.Statement(formula=formula, rule=validtnt.FantasyRule.PUSH,
                               referrals='1, 2, 4')
        with self.assertRaises(TypeError, msg="fantasy should be a fantasy"):
            validtnt.Statement(formula=validtnt.FantasyMarker(
                rule=validtnt.FantasyRule.PUSH),
                               rule=validtnt.FantasyRule.PUSH,
                               fantasy=validtnt.Text())
        stmt = validtnt.Statement(lineno=1, formula=formula,
                                  rule=validtnt.TNTRule.ADD_S, referrals=[104],
                                  fantasy=validtnt.Fantasy(content=validtnt.Text()))
        self.assertEqual(str(stmt), '1 \ta=b\tadd S (line 104)',
                         "wrong stringification of statement")

    def test_fantasy(self):
        """Test the Fantasy class"""
        a = validtnt.Variable(letter='a')
        b = validtnt.Variable(letter='b')
        premise = validtnt.Formula(arg1=a, arg2=b)
        premise = validtnt.Wrapper(arg=premise)
        premise = validtnt.Statement(formula=premise, rule=validtnt.TNTRule.PREMISE)
        outcome = validtnt.Formula(arg1=b, arg2=a)
        outcome = validtnt.Wrapper(arg=outcome)
        outcome = validtnt.Statement(formula=outcome, rule=validtnt.TNTRule.SYMMETRY)
        text = validtnt.Text([(1, premise), (2, outcome)])
        fantasy = validtnt.Fantasy(content=text)
        self.assertIs(fantasy.premise, premise, "wrong premise")
        self.assertIs(fantasy.outcome, outcome, "wrong outcome")
        with self.assertRaises(TypeError, msg="content should be Text only"):
            validtnt.Fantasy(content='a=b\tpremise\nb=a\tsymmetry')
        with self.assertRaises(TypeError, msg="fantasy should be Fantasy only"):
            validtnt.Fantasy(content=text, fantasy=text)
        with self.assertRaises(TypeError, msg="lineno should be int? only"):
            validtnt.Fantasy(content=text, lineno='0')
        self.assertEqual(str(fantasy), str(fantasy.content),
                         "stringified fantasy should be stringified content")

class TestParser(unittest.TestCase):
    """Test the parser itself"""

    parser = validtnt.TNTParser()

    def test_whitespace(self):
        """Test skipping whitespace"""
        self.assertEqual(self.parser.whitespace(0, '  a'), 2,
                         "wrong amount of whitespace skipped")
        with self.assertRaises(AssertionError, msg="didn't error on missing WS"):
            self.parser.whitespace(0, 'a', True)
        # TODO: complete

if __name__ == '__main__':
    unittest.main()
