## Lambda BNF Definition:

<lambda> ::= <wrappedFunction> <lambda> | <wrappedFunction> | <function>

<wrappedFunction> ::= "(" <function> ")"

<function> ::= "λ" <variables> "." <application-term>

<application-term> ::= <item> | <item> <application-term>

<item> ::= <terms> | "(" <expression> ")"

// <item> ::= <terms> | "(" <expression> ")" | <expression> ????
// ==> "λb.λt.λf.btf"

<expression> ::= <application-term> | <function>

<variables> ::= <letter> <variables> | <letter>
<terms> ::= <letter> <terms> | <letter>

<letter> := "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
