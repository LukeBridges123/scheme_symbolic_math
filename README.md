# scheme_symbolic_math
The goal here is to build a Scheme program that can do calculus symbolically. The goal here is for it to be able to handle any derivative or integral found in standard undergraduate calculus, although that's probably quite far off. At present, it can handle:

Derivatives of single-variable functions including algebraic functions, the main trigonometric and inverse trigonometric functions, the exponential and natural log, and any product, composition, etc. of the above. Differentiation of exponentiations with arbitrary (numerical) bases is implemented, but for logarithms, only natural logs work. Possible "next steps" here include implementing some of the less important trig functions, logs with bases other than e, and perhaps some more obscure things like hyperbolic functions. A more ambitious goal would be implicit differentiation, but that would involve much more algebra than the program is currently able to handle.

Partial derivatives of functions R^n -> R, involving any of the functions and operations listed above. Possible next steps include derivatives of more general functions R^n -> R^m, as well as some vector calculus operations like gradient and divergence.

Generation of (finite) Taylor series for single-variable function that the program can differentiate. Possible next steps include Taylor series of multivariable functions, finding remainder terms, and implementing Taylor series as streams to allow for more easily working with Taylor series of arbitrary length.

Some very basic integration capabilities. Currently this just means finding indefinite and definite integrals of polynomials and a tiny handful of easy-to-integrate functions. Possible next steps include, uh, the rest of integral calculus.

