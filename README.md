# Setup and Running

Parse really simple datalog programs

        A(x, y) :- B(c, d)
        A(x, y) :- B(c, d), A(e, f)
        A(x, y) :- B(c, d), c > 1
        A(x, y) :- B(c, d), c * d > 1
        A(x, y) :- B(c, d), c + 1 > 1, d < 1
        A(x, sum(y)) :- B(c, d)


Files

* `pegs/datalog.pegjs` is the grammar file for the language.
   Running `pegjs ./pegs/datalog.pegjs` will generate a javascript parser file as `./pegs/sql.js`
* `src/ast.coffee` defines the AST nodes for the language.  It's a rough subset of SQL and interaction statements.

Getting started

* install stuff

        npm install .
        cp node_modules/pegjs/bin/pegjs .
        pip install fabric

* `fab` commands

        fab -l

* compile parser

        fab peg

* compile modules

        fab coffee

* run a test in `./tests/`

        fab parser
