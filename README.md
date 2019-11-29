Compilation: make all
Execution: ./lambda
Use example: $ let fix = L f.((L x.(f (L y.(x x) y))) (L x. f (L y.((x x) y)))) in let sumaux = L f.(L n.(L m.if iszero n then m else succ ((f pred n) m))) in let sum = fix sumaux in (sum 66) 33

