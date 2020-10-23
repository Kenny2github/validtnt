# validtnt
Parse and validate [Typographic Number Theory](https://github.com/Kenny2github/language-tnt#summary-of-tnt) proofs.

## Examples
This proof passes:
```tnt
∀a:~Sa=0	axiom 1
∃b:∀a:~Sa=b	existence
<∀a:~Sa=0∧∃b:∀a:~Sa=b>	joining
```
because all rules of TNT were applied correctly.

This proof doesn't:
```tnt
∀a:(a+0)=a	axiom 2
∃b:∀a:(a+b)=a	existence
∃b:(b+b)=b	specification
```
because specification is not allowed when the term that specifies the variable contains any quantified variable.
