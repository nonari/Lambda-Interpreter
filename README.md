# Lambda calculus with extensions

Ocaml implementation of lambda calculus interpreter with extensions for `let in`, `if then else`, `succ`, `pred`, `iszero` and **natural numbers**

## How to use

 - Compilation: `make all`
 - Execution: `./lambda`
 - Use example: 
	 
	 - `$ let fix = L f.((L x.(f (L y.(x x) y))) (L x. f (L y.((x x) y)))) in let sumaux = L f.(L n.(L m.if iszero n then m else succ ((f pred n) m))) in let sum = fix sumaux in (sum 66) 33`
	 `= 99`