# Rust Call Graph

Static analysis(use `syntex_syntax`) to generate Rust codes' callgraph.

- [Rust Call Graph](#rust-call-graph)
  - [Run Demo](#run-demo)
  - [Current Work](#current-work)
    - [Fixme](#fixme)
    - [Todolist](#todolist)
    - [Note](#note)
  - [Current Result](#current-result)
    - [Legend](#legend)


## Run Demo
```sh
$ ./run_demo.sh src/main.rs
input:  src/main.rs
dot:    output.dot
debug:  debug.log
output: src_main.svg
```
## Current Work
### Fixme
- [ ] Fail parsing `use crate::something;`
- [ ] Fail parsing `use std::io::{prelude::*, BufReader};`
- [ ] Derivation at the very least. eg 
    `fn func<F>(arg: F){ arg(p); }`, the correct result should be `F(p)` instead of `arg(p)`.
    

### Todolist
- [ ] UFCS
- [ ] Context of variable/type/crate
    - [ ] `let a:A;  a.f();` => `A::f()`
    - [x] ~~subgraph for type system~~
    - [ ] if `callee` couldn't be found inside `callers`, analysis it.
      - [ ] But how? 
- [ ] TraitRef 
- [ ] Generics
  - [ ] generic function
  - [ ] generic type
  - [ ] generic impl
- [ ] Macro analysis
- [x] ~~Ignorelist~~
  - [ ] Persistence
- [ ] Nested function
- [x] ~~Ret(Args..)~~
- [ ] MutiFile

### Note
- [ ] Declare `enum Enum{ E1(T); }` and then use it like `e = Enum::E1(t)`, should it be considered as a function call?


## Current Result
![](./README.svg)

### Legend
![](./LEGEND.svg)
