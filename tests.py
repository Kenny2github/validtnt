"""Test all of validtnt"""
import unittest
import validtnt
from validtnt import std
from validtnt.parser import recurse_vars
from validtnt.runner import TNTRunner

# pylint checks membership for all subclasses of parents, so this rule is
# triggered multiple times; the assertIsInstance already establishes it as
# a subclass, though, so the no-member check is wrong here.
# pylint: disable=no-member

a = validtnt.Variable(letter='a')
b = validtnt.Variable(letter='b')

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

        with self.assertRaises(ValueError, msg="letter should be abcde only"):
            validtnt.Variable(letter='x')
        vara = validtnt.Variable(letter='a')
        self.assertIsInstance(vara, validtnt.Term, "all variables are terms")
        self.assertEqual(str(vara), 'a', "this variable should be a")
        self.assertIs(vara, validtnt.Variable(letter='a'),
                      "each possible variable should be a singleton")
        aprime = validtnt.Variable(letter='a', primes=2)
        self.assertIsInstance(aprime, validtnt.Term, "all variables are terms")
        self.assertEqual(str(aprime), 'a\N{PRIME}\N{PRIME}', "missing primes")
        self.assertIs(aprime, validtnt.Variable(letter='a', primes=2),
                      "each possible variable should be a singleton")

        S0 = validtnt.Successed(successors=1, arg=zero)
        self.assertIsInstance(S0, validtnt.Term, "``S``term is also a term")
        self.assertEqual(str(S0), 'S0', "wrong representation of S0")
        # we've left the realm of singletons now
        SSa = validtnt.Successed(successors=2, arg=a)
        self.assertIsInstance(SSa, validtnt.Term,
                              "any term can follow any number of ``S``es")
        self.assertEqual(str(SSa), 'SSa', "wrong representation of SSa")

        aplusb = validtnt.MultiTerm(arg1=a, arg2=zero, operator=validtnt.Operator.ADD)
        self.assertIsInstance(aplusb, validtnt.Term, "multiterms are terms")
        self.assertEqual(str(aplusb), '(a+0)', "wrong representation of a+0")
        atimesb = validtnt.MultiTerm(arg1=a, arg2=zero, operator=validtnt.Operator.MULT)
        self.assertEqual(str(atimesb), '(a\N{DOT OPERATOR}0)',
                         "wrong representation of a*0")

    def test_formula(self):
        """Test Formula and its subclasses"""
        aeqb = validtnt.Formula(arg1=a, arg2=b)
        self.assertEqual(str(aeqb), 'a=b',
                         "wrong representation for formula of a and b")

        notaeqb = validtnt.Negated(arg=aeqb)
        self.assertIsInstance(notaeqb, validtnt.Formula,
                              "negated formula is formula")
        self.assertEqual(str(notaeqb), '~a=b',
                         "wrong representation for not a=b")

        allaeqb = validtnt.Quantified(arg=aeqb, variable=a,
                                      quantifier=validtnt.Quantifier.ALL)
        self.assertIsInstance(allaeqb, validtnt.Formula,
                              "quantified formula is formula")
        self.assertEqual(str(allaeqb), '\N{FOR ALL}a:a=b',
                         "wrong representation for all a=b")

        notandall = validtnt.Compound(arg1=notaeqb, arg2=allaeqb,
                                      operator=validtnt.Logic.AND)
        self.assertIsInstance(notandall, validtnt.Formula,
                              "compound formula is formula")
        self.assertEqual(str(notandall), '<~a=b\N{LOGICAL AND}\N{FOR ALL}a:a=b>',
                         "wrong representation for <~a=b&Aa:a=b>")

        push = validtnt.FantasyMarker(rule=validtnt.FantasyRule.PUSH)
        self.assertIsInstance(push, validtnt.Formula,
                              "fantasy marker is formula, technically")
        self.assertEqual(str(push), '[', "wrong representation for push")

    def test_statement(self):
        """Test the Statement class"""
        formula = validtnt.Formula(arg1=validtnt.Variable(letter='a'),
                                   arg2=validtnt.Variable(letter='b'))
        stmt = validtnt.Statement(lineno=1, formula=formula,
                                  rule=validtnt.TNTRule.ADD_S, referrals=[104],
                                  fantasy=validtnt.Fantasy(content=validtnt.Text()))
        self.assertEqual(str(stmt), '1 \ta=b\tadd S (line 104)',
                         "wrong stringification of statement")

    def test_fantasy(self):
        """Test the Fantasy class"""
        premise = validtnt.Formula(arg1=a, arg2=b)
        premise = validtnt.Statement(formula=premise, rule=validtnt.TNTRule.PREMISE)
        outcome = validtnt.Formula(arg1=b, arg2=a)
        outcome = validtnt.Statement(formula=outcome, rule=validtnt.TNTRule.SYMMETRY)
        text = validtnt.Text([(1, premise), (2, outcome)])
        fantasy = validtnt.Fantasy(content=text)
        self.assertIs(fantasy.premise, premise, "wrong premise")
        self.assertIs(fantasy.outcome, outcome, "wrong outcome")
        self.assertEqual(str(fantasy), str(fantasy.content),
                         "stringified fantasy should be stringified content")

class TestParser(unittest.TestCase):
    """Test the parser itself"""

    parser = validtnt.TNTParser()

    def test_recurse_vars(self):
        """Test quantified/free tracking"""
        aeqb = validtnt.Formula(arg1=a, arg2=b)
        notaeqb = validtnt.Negated(arg=aeqb)
        allaeqb = validtnt.Quantified(arg=aeqb, variable=a,
                                      quantifier=validtnt.Quantifier.ALL)
        notandall = validtnt.Compound(arg1=notaeqb, arg2=allaeqb,
                                      operator=validtnt.Logic.AND)
        quantified = {}
        free = set()
        recurse_vars(notandall, quantified, free)
        self.assertIs(quantified.get(a, None), validtnt.Quantifier.ALL,
                      "variable a should be quantified ALL")
        self.assertIn(b, free, "variable b should be free")

    def test_whitespace(self):
        """Test skipping whitespace"""
        self.assertEqual(self.parser.whitespace(0, '  a'), 2,
                         "wrong amount of whitespace skipped")
        with self.assertRaises(AssertionError, msg="didn't error on missing WS"):
            self.parser.whitespace(0, 'a', True)
        self.assertEqual(self.parser.whitespace(0, 'a'), 0,
                         "shouldn't have skipped any whitespace")

    def test_variable(self):
        """Test parsing variables"""
        end, var = self.parser.variable(0, "a'' ")
        self.assertEqual(end, 3, "wrong variable length")
        self.assertIsInstance(var, validtnt.Variable, "wrong var type")
        self.assertEqual(var.letter, 'a', "wrong variable name")
        self.assertEqual(var.primes, 2, "wrong number of primes")

    def test_term(self):
        """Test parsing generic terms"""
        # test successorship
        end, term = self.parser.term(0, 'SS0')
        self.assertEqual(end, 3, "wrong length consumed")
        self.assertIsInstance(term, validtnt.Successed, "SS0 not Successed")
        assert isinstance(term, validtnt.Successed)
        self.assertEqual(term.successors, 2, "wrong number of Ss")
        self.assertIs(term.arg, validtnt.Numeral(), "SS0 is SS of 0, not...")

        # test the singleton 0
        end, term = self.parser.term(0, '0')
        self.assertEqual(end, 1)
        self.assertIsInstance(term, validtnt.Numeral)
        self.assertIs(term, validtnt.Numeral())

        # test multiterms
        end, term = self.parser.term(0, '(0+0)')
        self.assertEqual(end, 5, "probably didn't consume ending parenthesis")
        self.assertIsInstance(term, validtnt.MultiTerm)
        assert isinstance(term, validtnt.MultiTerm)
        self.assertIs(term.arg1, validtnt.Numeral(), "wrong argument")
        self.assertIs(term.arg2, validtnt.Numeral())
        self.assertEqual(term.operator, validtnt.Operator.ADD)

        # test failed parsing
        with self.assertRaises(AssertionError, msg="division is not valid"):
            self.parser.term(0, '(a/b)')
        with self.assertRaises(AssertionError, msg="closing paren was ignored"):
            self.parser.term(0, '(a/b[')

    def test_rule(self):
        """Test parsing rules"""
        with self.assertRaises(AssertionError, msg="invalid rule"):
            self.parser.rule(0, 'invalid')
        end, rule, num = self.parser.rule(0, 'add S')
        self.assertEqual(end, 5, "probably halted at space")
        self.assertIs(rule, validtnt.TNTRule.ADD_S, "wrong rule parsed")
        self.assertIsNone(num, "add S takes no lineno")

        end, rule, num = self.parser.rule(0, 'carry over line 5')
        self.assertEqual(end, 17, "probably missed the number")
        self.assertIs(rule, validtnt.PropositionalRule.CARRY_OVER,
                      "matching special carry failed")
        self.assertEqual(num, 5)

    def test_refs(self):
        """Test referral parsing"""
        refs = list(self.parser.refs(0, '(lines 1, 3, and 90)'))
        for i in refs:
            self.assertIsInstance(i, int)
        self.assertListEqual(refs, [1, 3, 90])
        with self.assertRaises(AssertionError, msg="invalid line num"):
            list(self.parser.refs(0, '(lines 1 3 90)'))
        with self.assertRaises(AssertionError, msg="forgot closing paren"):
            list(self.parser.refs(0, '(lines 1 3 90 '))

    def test_fantasy_markers(self):
        """Test parsing fantasy markers: [ ]"""
        end, formula = self.parser.formula(0, '[')
        self.assertEqual(end, 1, "forgot to consume")
        self.assertIsInstance(formula, validtnt.FantasyMarker,
                              "fantasy marker not parsed")
        assert isinstance(formula, validtnt.FantasyMarker)
        self.assertIs(formula.rule, validtnt.FantasyRule.PUSH,
                      "probably swapped or something")
        end, formula = self.parser.formula(0, ']')
        self.assertEqual(end, 1)
        self.assertIsInstance(formula, validtnt.FantasyMarker)
        assert isinstance(formula, validtnt.FantasyMarker)
        self.assertIs(formula.rule, validtnt.FantasyRule.POP)

    def test_atoms(self):
        """Test base case: a=b"""
        with self.assertRaises(AssertionError, msg="two terms: illegal"):
            self.parser.formula(0, '(a+b)(a+b)=(a+b)')
        end, formula = self.parser.formula(0, 'a=b ')
        self.assertEqual(end, 3)
        self.assertIsInstance(formula, validtnt.Formula)
        self.assertIs(formula.arg1, a)
        self.assertIs(formula.arg2, b)

    def test_negations(self):
        """Test negations: ~a=b"""
        end, formula = self.parser.formula(0, '~~a=b ')
        self.assertEqual(end, 5)
        self.assertIsInstance(formula, validtnt.Negated)
        assert isinstance(formula, validtnt.Negated)
        self.assertEqual(formula.negations, 2, "possibly skipped one")
        self.assertIsInstance(formula.arg, validtnt.Formula)
        self.assertIs(formula.arg.arg1, a)
        self.assertIs(formula.arg.arg2, b)

    def test_quantifications(self):
        """Test quantifications: Au:x Eu:x"""
        end, formula = self.parser.formula(0, 'Aa:a=b ')
        self.assertEqual(end, 6)
        self.assertIsInstance(formula, validtnt.Quantified)
        assert isinstance(formula, validtnt.Quantified)
        self.assertIs(formula.quantifier, validtnt.Quantifier.ALL)
        self.assertIs(formula.variable, a)
        self.assertIsInstance(formula.arg, validtnt.Formula)
        self.assertIs(formula.arg.arg1, a)
        self.assertIs(formula.arg.arg2, b)
        with self.assertRaises(AssertionError, msg="colon should be checked"):
            self.parser.formula(0, 'Aa-a=b')

    def test_compounds(self):
        """Test compound formulas: <P&Q> <P|Q> <P]Q>"""
        with self.assertRaises(AssertionError, msg="closing should be checked"):
            self.parser.formula(0, '<a=b&b=a)')
        with self.assertRaises(AssertionError, msg="operator should be checked"):
            self.parser.formula(0, '<a=b/b=a>')
        with self.assertRaises(AssertionError,
                               msg="free vars in one arg "
                               "cannot be quantified in another"):
            self.parser.formula(0, '<Aa:a=b&Ab:b=a>')
        end, formula = self.parser.formula(0, '<a=b&b=a>')
        self.assertEqual(end, 9, "consume closing")
        self.assertIsInstance(formula, validtnt.Compound)
        assert isinstance(formula, validtnt.Compound)
        self.assertIs(formula.operator, validtnt.Logic.AND)
        self.assertIsInstance(formula.arg1, validtnt.Formula)
        self.assertIsInstance(formula.arg2, validtnt.Formula)
        self.assertIs(formula.arg1.arg1, a)
        self.assertIs(formula.arg1.arg2, b)
        self.assertIs(formula.arg2.arg1, b)
        self.assertIs(formula.arg2.arg2, a)

    def test_parse_line(self):
        """Test entire line parsing"""
        # get exceptional circumstances out of the way
        with self.assertRaises(TypeError, msg="can't operate on no text"):
            self.parser.parse_line(0, None)
        self.assertIsNone(self.parser.parse_line(0, '\t \n'), "empty line")
        with self.assertRaises(AssertionError, msg="cut-off string should error"):
            self.parser.parse_line(0, 'a=')

        stmt = self.parser.parse_line(1, '0 a=b premise')
        self.assertIsInstance(stmt, validtnt.Statement, "you had one job")
        assert isinstance(stmt, validtnt.Statement)
        self.assertEqual(stmt.lineno, 0, "wrong line number")
        self.assertIsInstance(stmt.formula, validtnt.Formula)
        assert isinstance(stmt.formula, validtnt.Formula)
        self.assertIs(stmt.formula.arg1, a)
        self.assertIs(stmt.formula.arg2, b)
        self.assertIs(stmt.rule, validtnt.TNTRule.PREMISE)
        self.assertFalse(bool(stmt.referrals))

        stmt = self.parser.parse_line(-1, 'a=b premise (line 3, 4, 9)')
        assert isinstance(stmt, validtnt.Statement)
        self.assertEqual(stmt.lineno, -1, "no line number should have been set")
        self.assertIsInstance(stmt.referrals, list)
        self.assertListEqual(stmt.referrals, [3, 4, 9])

    def test_parse(self):
        """Test whole text parsing"""
        with self.assertRaises(TypeError, msg="can't operate on no text"):
            self.parser.parse()
        self.assertIsInstance(self.parser.parse('a=b premise'), validtnt.Text)

        with self.assertRaises(validtnt.ParsingFailed,
                               msg="error on stack underflow") as cm:
            self.parser.parse('] pop')
        self.assertEqual(cm.exception.line, '] pop')
        with self.assertRaises(validtnt.ParsingFailed, msg="error on non-empty stack"):
            self.parser.parse('[ push')

        # actually test multiple lines
        text = self.parser.parse("""1 a=b premise
                                    2 b=a symmetry""")
        self.assertEqual(len(text), 2)
        self.assertEqual(str(text[1]).split(), '1 a=b premise'.split(),
                         "probably got the symmetry one instead")
        self.assertIs(text[2].rule, validtnt.TNTRule.SYMMETRY)

        text = self.parser.parse("""1 [ push
            2 a=b premise
        3 ] pop
        4 <a=b]a=b> fantasy rule""")
        self.assertEqual(len(text), 4, "fantasy markers probably not included")
        self.assertIsInstance(text[2].fantasy, validtnt.Fantasy)
        self.assertIsNone(text[4].fantasy)

class TestRunner(unittest.TestCase):
    """Test the runner. The "premise" rule is used to introduce arbitrary
    theorems as true for use by unit tests.
    """

    parser = validtnt.TNTParser()

    def test_find_fantasy(self):
        """Test finding a fantasy in its ancestors"""
        runner = validtnt.TNTRunner('')
        stmt = self.parser.parse_line(0, 'a=b premise')
        assert stmt is not None
        stmt.fantasy = validtnt.Fantasy(
            content=validtnt.Text([(stmt.lineno, stmt)]))
        stmt.fantasy.fantasy = validtnt.Fantasy(content=validtnt.Text())
        self.assertTrue(runner.find_fantasy(stmt.fantasy.fantasy, stmt))
        self.assertFalse(runner.find_fantasy(
            validtnt.Fantasy(content=validtnt.Text()), stmt))

    def test_raise_fantasy(self):
        """Test fantasy level checking"""
        runner = validtnt.TNTRunner('''a=b premise
                                    [ push
                                        b=a premise
                                        a=b carry over line 0
                                    ] pop
                                    <b=a]a=b> fantasy rule''')
        assert runner.text is not None
        self.assertTrue(runner.raise_fantasy(2, runner.text.vals[-1].fantasy,
                                             'none', 'none'),
                        "Statement from inner fantasy should be skipped")
        self.assertFalse(runner.raise_fantasy(2, runner.text[3].fantasy,
                                              'none', 'none'),
                         "Statement in same fantasy should be checked")
        with self.assertRaisesRegex(validtnt.MissingArgument,
                                    'rule missing corresponding arg',
                                    msg="Statement in outer fantasy raises"):
            runner.raise_fantasy(0, runner.text[2].fantasy, 'rule', 'arg')
        with self.assertRaisesRegex(validtnt.MissingArgument,
                                    'rule missing corresponding arg',
                                    msg="Out of range statement raises"):
            runner.raise_fantasy(-1, runner.text[2].fantasy, 'rule', 'arg')
        with self.assertRaisesRegex(validtnt.MissingArgument,
                                    'rule missing corresponding arg',
                                    msg="Out of range statement raises"):
            runner.raise_fantasy(10, runner.text[2].fantasy, 'rule', 'arg')

    def test_find_arg(self):
        """Test finding an argument that passes a cmp test"""
        runner = validtnt.TNTRunner('''a=b premise
                                    b=a symmetry''')
        assert runner.text is not None
        def _cmp(ln: validtnt.Statement, st: validtnt.Statement) -> bool:
            return (
                ln.formula.arg1 is st.formula.arg2
                and ln.formula.arg2 is st.formula.arg1
            )
        line = runner.find_arg(1, runner.text[1], _cmp, 'none', 'none')
        self.assertIsInstance(line, validtnt.Statement, 'uh oh stinky')
        self.assertEqual(str(line), '0 a=b\tpremise')

        with self.assertRaises(validtnt.MissingArgument,
                               msg="How did a constant-False cmp work?"):
            runner.find_arg(1, runner.text[1], lambda ln, st: False, '', '')

    def test_at_most_refs(self):
        """Test checking for too many referrals"""
        runner = validtnt.TNTRunner('')
        line = self.parser.parse_line(0, 'a=b premise')
        assert line is not None
        self.assertIsNone(runner.at_most_refs(line, 2, 'none'))
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="no raise?"):
            runner.at_most_refs(line, -1, 'rule')

    def test_get_arg(self):
        """Test getting a direct or indirect referral"""
        runner = validtnt.TNTRunner('''a=b premise
                                    b=a symmetry''')
        assert runner.text is not None
        self.assertIs(runner.get_arg(1, runner.text[1]), runner.text[0],
                      msg="indirect referral to previous line failed")
        runner = validtnt.TNTRunner('''a=b premise
                                    b=a symmetry (line 0)''')
        assert runner.text is not None
        self.assertIs(runner.get_arg(1, runner.text[1]), runner.text[0])
        with self.assertRaises(validtnt.MissingArgument,
                               msg="referral should not transcend fantasy"):
            runner = validtnt.TNTRunner('''0 a=b axiom 1
                                        1 [ push
                                        2   a=c premise
                                        3 ] pop''')
            assert runner.text is not None
            runner.get_arg(2, runner.text[2])

    def test_joining(self):
        """Test the joining rule"""
        runner = validtnt.TNTRunner('''a=b premise
                                    b=a premise
                                    <a=b&b=a> joining''')
        # if this fails, it will be an error
        assert runner.text is not None
        self.assertIsNone(runner.rule_joining(2, runner.text[2]))
        with self.assertRaises(validtnt.InvalidRule,
                               msg="wrong operator should raise"):
            # this check should be performed first
            runner = validtnt.TNTRunner('''<a=b|b=a> joining''')
            assert runner.text is not None
            runner.rule_joining(0, runner.text[0])
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="joining only allows 2 referrals"):
            runner = validtnt.TNTRunner('''<a=b&b=a> joining (lines 1,2,3)''')
            assert runner.text is not None
            runner.rule_joining(0, runner.text[0])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="directly wrong referral should raise"):
            runner = validtnt.TNTRunner('''a=b premise
                                        b=a premise
                                        <a=b&b=c> joining (lines 0, 1)''')
            assert runner.text is not None
            runner.rule_joining(2, runner.text[2])
        with self.assertRaises(validtnt.MissingArgument,
                               msg="indirectly referring prev wrongly raises"):
            runner = validtnt.TNTRunner('''
                [ Indirect referrals should not search infinitely far back. ]
                [ They should only look at the directly previous couple. ]
                1 a=b premise
                2 b=a premise
                3 c=d premise
                4 d=c premise
                5 <a=b&b=a> joining
            '''.strip())
            assert runner.text is not None
            runner.rule_joining(4, runner.text[5])
        runner = validtnt.TNTRunner('''
            [ Direct and indirect referrals should be combinable. ]
            1 a=b premise
            2 c=d premise
            3 b=a premise
            4 <a=b&b=a> joining (line 1)
        '''.strip())
        assert runner.text is not None
        self.assertIsNone(runner.rule_joining(3, runner.text[4]))

    def test_separation(self):
        """Test the separation rule"""
        runner = validtnt.TNTRunner('''<a=b&b=a> premise
                                    a=b separation''')
        assert runner.text is not None
        self.assertIsNone(runner.rule_separation(1, runner.text[1]))
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="separation only takes 1 ref"):
            runner = validtnt.TNTRunner('''a=b separation (lines 1,2)''')
            assert runner.text is not None
            runner.rule_separation(0, runner.text[0])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="wrong operator should raise"):
            runner = validtnt.TNTRunner('''<a=b|b=a> premise
                                        a=b separation''')
            assert runner.text is not None
            runner.rule_separation(1, runner.text[1])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="wrong referral should raise"):
            runner = validtnt.TNTRunner('''<a=b&b=a> premise
                                        a=c separation''')
            assert runner.text is not None
            runner.rule_separation(1, runner.text[1])

    def test_double_tilde(self):
        """Test the double-tilde rule"""
        runner = validtnt.TNTRunner('''~~<a=b&b=a> premise
                                    <a=b&~~b=a> double-tilde''')
        assert runner.text is not None
        self.assertIsNone(runner.rule_double_tilde(1, runner.text[1]))
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="double-tilde only takes 1 ref"):
            runner = validtnt.TNTRunner('''a=b double-tilde (lines 1, 2)''')
            assert runner.text is not None
            runner.rule_double_tilde(0, runner.text[0])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="wrong referral should raise"):
            runner = validtnt.TNTRunner('''~~<a=b&b=a> premise
                                        ~<a=b&b=a> double-tilde''')
            assert runner.text is not None
            runner.rule_double_tilde(1, runner.text[1])

    def test_fantasy_rule(self):
        """Test the fantasy rule"""
        runner = validtnt.TNTRunner('''0 [ push
                                    1     a=b premise
                                    2 ] pop
                                    3 <a=b]a=b> fantasy rule''')
        assert runner.text is not None
        self.assertIsNone(runner.rule_fantasy_rule(3, runner.text[3]))
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="fantasy rule takes no refs"):
            runner = validtnt.TNTRunner('''a=b fantasy rule (line 1)''')
            assert runner.text is not None
            runner.rule_fantasy_rule(0, runner.text[0])
        with self.assertRaises(validtnt.MissingArgument,
                               msg="fantasy rule needs a fantasy"):
            runner = validtnt.TNTRunner('''a=b fantasy rule''')
            assert runner.text is not None
            runner.rule_fantasy_rule(0, runner.text[0])
        with self.assertRaises(validtnt.MissingArgument,
                               msg="fantasy rule should not search "
                               "infinitely far back"):
            runner = validtnt.TNTRunner('''0 [ push
                                        1     a=b premise
                                        2 ] pop
                                        3 b=a symmetry
                                        4 <a=b]a=b> fantasy rule''')
            assert runner.text is not None
            runner.rule_fantasy_rule(4, runner.text[4])
        with self.assertRaises(validtnt.InvalidRule,
                               msg="at least if I'm not mistaken, "
                               "the only way for this to happen is if "
                               "the fantasy rule was used instead of "
                               "premise, which would have the push "
                               "as the previous statement"):
            runner = validtnt.TNTRunner('''0 [ push
                                        1     a=b fantasy rule
                                        2 ] pop''')
            assert runner.text is not None
            runner.rule_fantasy_rule(1, runner.text[1])
        with self.assertRaises(validtnt.InvalidFantasy,
                               msg="premise should match condition"):
            runner = validtnt.TNTRunner('''0 [ push
                                        1     a=b premise
                                        2 ] pop
                                        3 <a=c]a=b> fantasy rule''')
            assert runner.text is not None
            runner.rule_fantasy_rule(3, runner.text[3])
        with self.assertRaises(validtnt.InvalidFantasy,
                               msg="outcome should match result"):
            runner = validtnt.TNTRunner('''0 [ push
                                        1     a=b premise
                                        2 ] pop
                                        3 <a=b]a=c> fantasy rule''')
            assert runner.text is not None
            runner.rule_fantasy_rule(3, runner.text[3])

    def test_carry(self):
        """Test the carry over line n rule"""
        runner = validtnt.TNTRunner('''0 a=b premise
                                    1 [ push
                                    2     b=a premise
                                    3     a=b carry over line 0
                                    4 ] pop''')
        assert runner.text is not None
        self.assertIsNone(runner.rule_carry(3, runner.text[3]))
        with self.assertRaises(validtnt.TooManyReferrals,
                               msg="carry takes one parser-given ref"):
            runner = validtnt.TNTRunner('''a=b carry over line 0 (line 1)''')
            assert runner.text is not None
            runner.rule_carry(0, runner.text[0])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="carry comes from direct parent"):
            runner = validtnt.TNTRunner('''0 [ push
                                        1     a=b premise
                                        2 ] pop
                                        3 <a=b]a=b> fantasy rule
                                        4 [ push
                                        5     b=a premise
                                        6     [ push
                                        7         a=b carry over line 1
                                        8     ] pop
                                        9     <a=b]a=b> fantasy rule
                                        10 ] pop
                                        11 <b=a]<a=b]a=b>> fantasy rule''')
            assert runner.text is not None
            runner.rule_carry(7, runner.text[7])
        with self.assertRaises(validtnt.InvalidReferral,
                               msg="carry must be a copy"):
            runner = validtnt.TNTRunner('''0 a=b premise
                                        1 [ push
                                        2     b=a premise
                                        3     a=c carry over line 0
                                        4 ] pop
                                        5 <b=a]a=c> fantasy rule''')
            assert runner.text is not None
            runner.rule_carry(3, runner.text[3])

    def test_detachment(self):
        """Test the detachment rule"""
        runner = validtnt.TNTRunner('''0 a=b premise
                                    1 <a=b]b=a> premise
                                    2 b=a detachment''')
        assert runner.text is not None
        self.assertIsNone(runner.rule_detachment(2, runner.text[2]))

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        with open(sys.argv[1]) as f:
            runner = TNTRunner(f.read())
        runner.validate()
    else:
        unittest.main()
