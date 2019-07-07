# Rust Call Graph

Static analysis(use `syntex_syntax`) to generate Rust codes' callgraph.


## Run Demo
```sh
$ ./run_demo.sh src/main.rs
input:  src/main.rs
dot:    output.dot
debug:  debug.log
output: src_main.svg
```

## Fixme
- [ ] Fail parsing `use crate::something;`
- [ ] Fail parsing `use std::io::{prelude::*, BufReader};`


## Todolist
- [ ]  UFCS
- [ ]  Context of variable/type/crate
    - [ ] `let a:A;  a.f();` => `A::f()`
    - [ ] subgraph for type system
- [ ]  Generics
  - [ ] generic function
  - [ ] generic type
  - [ ] generic impl
- [ ]  Macro analysis
- [ ]  Ignorelist


## Current Result
![Alt text](./README.svg)
