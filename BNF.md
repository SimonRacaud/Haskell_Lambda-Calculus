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

## Logic clauses BNF

<xxx> ::= <logicExpr> <xxx> | <logicExpr>

<logicExpr> ::= <expr> <cond> | <expr>

<expr> ::= <operand> <cond> | <operand>

<wrapExpr> ::= '(' <expr> ')'

<operand> ::= <value> | <not> | <wrapExpr>

--

<cond> ::= <not> | <and> | <or>

<and> ::= <operand> "and" <operand>

<or> ::= <operand> "or" <operand>

<not> ::= "not" <operand>

<value> ::= "true" | "false"

###

<stmt> ::= <ifCond> | <expr>

<ifCond> ::= "if" <expr> "then" <stmt> "else" <stmt>

<expr> ::= <param> <duop> <expr> | <unop> <expr> | '(' <expr> ')' | <bool>

// To avoid an infinite loop for <duop> first parameter
<param> ::=  '(' <expr> ')' | <bool>

<bool> ::= "True" | "False"
<unop> ::= "not"
<duop> ::= "and" | "or"


>>> not (not True) and False
(Uno Not 
    (Duo And 
        (SubExpr 
            (Uno Not 
                (Var True)
            )
        ) 
        (Var False)
    )
)

>>> not ( not True) and False or True
(Uno Not 
    (Duo And 
        (SubExpr 
            (Uno Not (Var True))
        ) 
        (Duo Or 
            (Var False) 
            (Var True)
        )
    )
)