parser = require './jsdatalogparser.js'
ast = require "./ast.js"
_ = require "underscore"
fs = require "fs"
vows = require "vows"
assert = require "assert"


suite = vows.describe("Parser Tests")
  .addBatch(
    "Simple":
      topic: -> parser.one "head(a, b) :- tail(b, c)"
      "parses": (q) ->
        assert.equal q.toSQL(), "head(a, b) :- tail(b, c)"

    "Join":
      topic: -> parser.one "head(a, b) :- A(b, c), B(d, c), C(b, a)"
      "parses": (q) ->
        assert.equal q.toSQL(), "head(a, b) :- A(b, c), B(d, c), C(b, a)" 

    "Filters":
      topic: -> parser.one "head(a, b) :- A(b, c), B(d, c), C(b, a), b > 0, c < 100, (a+b) = 2"
      "parses": (q) ->
        assert.equal q.toSQL(), "head(a, b) :- A(b, c), B(d, c), C(b, a), (b > 0.00), (c < 100.00), ((a + b) = 2.00)"

    "Aggregates":
      topic: -> parser.one "head(a, sum(c)) :- T(a, c), c > 10"
      "parses": (q) ->
        assert.equal q.toSQL(), "head(a, sum(c)) :- T(a, c), (c > 10.00)"

    "Program":
      topic: ->
        "
        T(x, y) :- readingssmall(x)
        scales(minx, maxx, miny, maxy) :- GB(readingssmall(), min(x),
        "
  )


saveDot = (q) ->
  fs.writeFile("./graph.dot", q.toDot(), (err) ->
    console.log err if err?
  )


suite.run()
