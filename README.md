# Setup and Running

This folder contains code related to the core interaction language parser, AST, and execution engine.

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