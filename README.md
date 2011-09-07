
# An ambiguous language called Nano #

nano it's a simple (trust me) language that proposes a new way to create
extensibility through what I call "permisive operators". A permisive operator can be
built using arbitrary characters and can be applied without leaving whitespace.

## Building Matematical Notation ##

nano has few built-in operations based on lambda-calculus. All these use normal
prefix notation. The following declarations declare the `+` and `-` operators with
appropiate precedences.

     let infixr 1 x + y = add x y
     let infixr 2 x - y = sub x y

By default operators are use prefix notations. There aren't negative lexicals in
nano. Nano is powerful enough, that we can just define a "-" unary operator for
writing negative lexicals.

    let 10 - x = sub 0 x

Nano is so simplistic that it doesn't even define expressions or grouping. You can
define them yourself by using closed operators.

    let closed ( x ) = x

All you need is recursion :)

    let rec suffix n ! = if (n==0) 1 (n * (n-1)!)

There's no even list sugar syntax, but you can create it!

    let infixr 2 x : y = cons x y
    let [] = empty
    let rec infixr l [ idx = if (idx == 0) (hd l) ((rest l)[(idx -1))
    let suffix x ] = x
    let 10 [ x = x
    let infixr x , y = x : y
    let suffix x ] = x : empty
    let rec infixr x ... y = if (x > y)
                                empty
                                (cons x ((x+1)...y))
    let infixl 4 x ? y = if x (cons y empty) (empty)
    let infixl 4 x : y = if (isNull x) y (hd x)

Since application doesn't require whitespace, is possible to have real hungarian
notation.

    let infixl x default y = if (isNull (fst x)) y (hd (fst x))
    let s x = if x "true" "false"
    let i x = if x 1 0

And closing operators can be used to have a mini-xml language

     let infixr x ; y = buildPair x y
     let closed <html> x </html> = buildPair "html" x
     let closed <head> x </head> = buildPair "body" x
     let closed <body> x </body> = buildPair "body" x
     let closed <div> x </div> = buildPair "div" x

