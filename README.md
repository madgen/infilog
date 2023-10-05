# infilog

A Datalog variant without range restriction. This gives us efficient
computation over infinite relations in finite time.

To run on a program:

```
stack exec -- infilog-exe <PATH>
```

See `test/cases` for examples and what they compute. You can add new programs
with `.inf` under that directory and run `stack test` to generate corresponding
`.exp` files.
