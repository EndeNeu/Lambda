# Lambda Expression Library #

This is a simply lambda expression parser which offers also the possibility to apply beta reduction operations. The library is fully functional, every operation applied returns always a new expression, there's no mutability.

* Create lambda expressions;
* Beta reduction with lambda expression as argument;
* Auto Beta reduction;
* Beta reduction to minimal term;
* Core lambda expressions (in expansion)
* Parse lambda expression form string.
* Takes into account Free Variables.

### Samples Definition ###

```
#!scala

// λx.x
val identity = EX(BV("x", AR("x")))

// λx.(x x)
val selfApply = EX(BV("x", AR("x"), AR("x")))
```

Every expression is wrapped in the EX constructor, it takes N lambda variables (bound variable or arguments).

### Samples Operation ###

```
#!scala

identity.toString // prints λx.x
identity.betaReduce() // returns a new lambda expression: λx.x
identity.betaReduce(AR("y")) // returns a new lambda expression with one argument: y

selfApply.toString // prints λx.(x x)
val newExp = selfApply.betaReduce(identity) // returns: λx.x λx.x
newExp.betaReduce() // returns: λx.x
```

### Parse from String (sperimental) ###

```
#!scala

LambdaParser.fromString("λx.(λy.(x y))").toString // prints λx.λy.(x y)
LambdaParser.fromString("λx.x") // returns the identity expression
```

### Known issues ###

To parse a lambda expression from a string the arguments of the bound variable must be wrapped in paenthesys if there's more than one argument:

```
#!scala

//this works, returns λx.λy.(x y)
LambdaParser.fromString("λx.(λy.(x y))") 

// this doesn't
LambdaParser.fromString("λx.λy.(x y)") 
```