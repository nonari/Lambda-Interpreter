# Lambda calculus with extensions

Ocaml implementation of lambda calculus interpreter with extensions for: 
- `let in`
- `letrec in`
- `if then else`
- `succ`
- `pred`
- `iszero`
- **Records**
- **Natural Numbers**
- **Context**

## How to use
- Compilation: `make all`
- Execution: `./lambda`
- Use examples: 
  - *Logical AND in pure lambda calculus*\
    `$ (L p.L q. p q p)(L t.L f.t)(L t.L f.t)`
  - *Sum two numbers with explicit FIX combinator*\
    `$ let fix = L f.(L x. f (L y. x x y)) (L x. f (L y. x x y)) in let sumaux = L f. (L n. (L m.if (iszero n) then m else succ (f (pred n) m))) in let sum = fix sumaux in sum 44 55`
  - *Sum two numbers with letrec*\
    `$ letrec sum = lambda n. lambda m. if iszero n then m else succ (sum (pred n) m) in sum 44 55`
  - *Project a record by label*\
    `$ {a=1, b=2}.b`

