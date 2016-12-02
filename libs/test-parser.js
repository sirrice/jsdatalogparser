// Generated by CoffeeScript 1.10.0
(function() {
  var _, assert, ast, fs, parser, saveDot, suite, vows;

  parser = require('./jsdatalogparser.js');

  ast = require("./ast.js");

  _ = require("underscore");

  fs = require("fs");

  vows = require("vows");

  assert = require("assert");

  suite = vows.describe("Parser Tests").addBatch({
    "Simple": {
      topic: function() {
        return parser.one("head(a, b) :- tail(b, c)");
      },
      "parses": function(q) {
        return assert.equal(q.toSQL(), "head(a, b) :- tail(b, c);");
      }
    },
    "Join": {
      topic: function() {
        return parser.one("head(a, b) :- A(b, c), B(d, c), C(b, a)");
      },
      "parses": function(q) {
        return assert.equal(q.toSQL(), "head(a, b) :- A(b, c), B(d, c), C(b, a);");
      }
    },
    "Filters": {
      topic: function() {
        return parser.one("head(a, b) :- A(b, c), B(d, c), C(b, a), b > 0, c < 100, (a+b) = 2");
      },
      "parses": function(q) {
        return assert.equal(q.toSQL(), "head(a, b) :- A(b, c), B(d, c), C(b, a), (b > 0.00), (c < 100.00), ((a + b) = 2.00);");
      }
    },
    "Aggregates": {
      topic: function() {
        return parser.one("head(a, sum(c)) :- T(a, c), c > 10");
      },
      "parses": function(q) {
        return assert.equal(q.toSQL(), "head(a, sum(c)) :- T(a, c), (c > 10.00);");
      }
    },
    "Tuples": {
      topic: function() {
        return parser("T(1,2,3,4); G('a', 'b')");
      },
      parses: function(q) {
        assert.equal(q.toSQL(), "T(1, 2, 3, 4);\nG('a', 'b');");
        return console.log(q.toSQL());
      }
    },
    "Program": {
      topic: function() {
        return "T(x, y) :- readingssmall(x); scales(minx, maxx, miny, maxy) :- GB(readingssmall(), min(x))";
      }
    }
  });

  saveDot = function(q) {
    return fs.writeFile("./graph.dot", q.toDot(), function(err) {
      if (err != null) {
        return console.log(err);
      }
    });
  };

  suite.run();

}).call(this);
