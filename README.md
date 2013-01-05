sudoku2SAT
==========

This application converts given Sudoku puzzle to SAT. SAT can be solved by any solver which can read input in [CNF](http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html) format.
Solvers:
* [Lingeling](http://fmv.jku.at/lingeling/)
* [zChaff](http://www.princeton.edu/~chaff/zchaff.html)

Sudoku should be entered in following format:

```
018000700
000300200
070000000
000071000
600000040
300000000
400500003
020080000
000000060
```

Where ``0`` means unknown value, and non-zero value means known value.

Program prints output on standard output, so if you want it in file ... use imagination :-)
