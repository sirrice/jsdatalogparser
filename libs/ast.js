// Generated by CoffeeScript 1.10.0
(function() {
  var AggPredicate, Atom, BetweenExpr, ColExpr, DEBUG, Expr, ExternalTable, FuncExpr, Group, Having, LetUDF, LetUDFArg, Limit, Node, OrderBy, OrderByClause, ParamExpr, ParamVar, Predicate, Queries, QueryTable, SpecialExpr, Statement, Table, TableExpr, TableUDF, Tuple, UnaryExpr, ValExpr, _,
    indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; },
    slice = [].slice,
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  _ = require("underscore");

  _.append = function(arr, v) {
    arr = _.clone(arr);
    arr.push(v);
    return arr;
  };

  DEBUG = false;

  Node = (function() {
    Node.id = 1;

    function Node(type) {
      this.type = type;
      this.id = Node.id++;
    }

    Node.prototype.isType = function() {
      var ref;
      return ref = this.nodeType(), indexOf.call(arguments, ref) >= 0;
    };

    Node.prototype.isExpr = function() {
      var ref;
      return (ref = this.nodeType()) === "Expr" || ref === "SpecialExpr" || ref === "ColExpr" || ref === "ValExpr" || ref === "BetweenExpr" || ref === "UnaryExpr" || ref === "FuncExpr" || ref === "TableExpr";
    };

    Node.prototype.isTable = function() {
      var ref;
      return (ref = this.nodeType()) === "Table" || ref === "QueryTable";
    };

    Node.prototype.clone = function() {
      throw Error("not implemented");
    };

    Node.prototype.nodeType = function() {
      return this.constructor.name;
    };

    Node.prototype.descendents = function() {
      var types;
      types = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.allNodes(function(n) {
        return n.isType.apply(n, types);
      });
    };

    Node.prototype.children = function() {
      return [];
    };

    Node.prototype.sources = function() {
      return this.descendents("Table");
    };

    Node.prototype.variables = function() {
      return this.descendents("ColExpr");
    };

    Node.prototype.allNodes = function(f) {
      var func, ret;
      ret = [];
      func = function(node, path) {
        if (f(node)) {
          return ret.push(node);
        }
      };
      this.traverse(func);
      return _.uniq(ret);
    };

    Node.prototype.traverse = function(f, path) {
      var child, i, len, newpath, ref, results;
      if (path == null) {
        path = [];
      }
      f(this, path);
      newpath = _.append(path, this);
      ref = this.children();
      results = [];
      for (i = 0, len = ref.length; i < len; i++) {
        child = ref[i];
        if (child == null) {
          continue;
        }
        if (!_.isFunction(child.traverse)) {
          console.log("ERR: child node doesn't support traverse().  Printing child, this and path");
          console.log(child);
          console.log(this);
          console.log(path);
        }
        results.push(child.traverse(f, newpath));
      }
      return results;
    };

    Node.prototype.toSQL = function() {
      return "";
    };

    Node.prototype.toPrettySQL = function() {
      return this.toSQL();
    };

    Node.prototype.toString = function() {
      return this.toSQL();
    };

    Node.prototype.toJSString = function() {
      throw new Error("toJSString() not implemented by " + (this.toSQL()));
    };

    Node.prototype.toDot = function() {
      var f, output, s;
      output = [];
      output.push("\ndigraph DepGraph" + this.nprintcalls + " {");
      output.push("  labelloc=\"t\";");
      output.push("  label=\"graph" + this.nprintcalls + "\";");
      f = function(node, path) {
        var name, parent, pname;
        if (path.length > 0) {
          parent = _.last(path);
          pname = parent.nodeType();
          name = node.nodeType();
          return output.push("  " + pname + parent.id + " -> " + name + node.id);
        }
      };
      this.traverse(f);
      output.push("}\n");
      s = output.join("\n");
      return s;
    };

    return Node;

  })();

  Queries = (function(superClass) {
    extend(Queries, superClass);

    function Queries(queries) {
      this.queries = queries;
      Queries.__super__.constructor.apply(this, arguments);
    }

    Queries.prototype.clone = function() {
      return new Queries(_.map(this.queries, function(q) {
        return q.clone();
      }));
    };

    Queries.prototype.children = function() {
      return this.queries;
    };

    Queries.prototype.toSQL = function() {
      var sqls;
      sqls = _.map(this.queries, function(q) {
        return q.toSQL();
      });
      return "" + (sqls.join("\n"));
    };

    return Queries;

  })(Node);

  Statement = (function(superClass) {
    extend(Statement, superClass);

    function Statement(head1, tail1) {
      this.head = head1;
      this.tail = tail1;
    }

    Statement.prototype.children = function() {
      return _.flatten([this.head, this.tail]);
    };

    Statement.prototype.toSQL = function() {
      var head, tail;
      head = this.head.toSQL();
      tail = _.map(this.tail, function(t) {
        return t.toSQL();
      });
      return head + " :- " + (tail.join(", ")) + ";";
    };

    return Statement;

  })(Node);

  Atom = (function(superClass) {
    extend(Atom, superClass);

    function Atom() {
      return Atom.__super__.constructor.apply(this, arguments);
    }

    return Atom;

  })(Node);

  Tuple = (function(superClass) {
    extend(Tuple, superClass);

    function Tuple(tableName, vals1) {
      this.tableName = tableName;
      this.vals = vals1;
    }

    Tuple.prototype.toSQL = function() {
      var vals;
      vals = _.map(this.vals, function(v) {
        if (_.isString(v)) {
          return "'" + v + "'";
        } else {
          return v + "";
        }
      });
      return this.tableName + "(" + (vals.join(", ")) + ");";
    };

    return Tuple;

  })(Atom);

  Predicate = (function(superClass) {
    extend(Predicate, superClass);

    function Predicate(tableName, args1) {
      this.tableName = tableName;
      this.args = args1;
    }

    Predicate.prototype.toSQL = function() {
      return this.tableName + "(" + (this.args.join(", ")) + ")";
    };

    return Predicate;

  })(Atom);

  AggPredicate = (function(superClass) {
    extend(AggPredicate, superClass);

    function AggPredicate(tableName, args1) {
      this.tableName = tableName;
      this.args = args1;
      this.groups = _.filter(this.args, _.isString);
      this.aggs = _.reject(this.args, _.isString);
    }

    AggPredicate.prototype.children = function() {
      return this.aggs;
    };

    AggPredicate.prototype.toSQL = function() {
      var aggs, groups;
      groups = this.groups.join(", ");
      aggs = _.map(this.aggs, function(a) {
        return a.toSQL();
      }).join(", ");
      return this.tableName + "(" + groups + ", " + aggs + ")";
    };

    return AggPredicate;

  })(Atom);

  Expr = (function(superClass) {
    extend(Expr, superClass);

    function Expr(l1, op1, r1) {
      this.l = l1;
      this.op = op1 != null ? op1 : null;
      this.r = r1 != null ? r1 : null;
      Expr.__super__.constructor.apply(this, arguments);
    }

    Expr.prototype.clone = function() {
      var l, op, r;
      l = this.l.clone();
      op = this.op;
      r = null;
      if (this.r != null) {
        r = this.r.clone();
      }
      return new Expr(l, op, r);
    };

    Expr.prototype.children = function() {
      return _.compact([this.l, this.r]);
    };

    Expr.prototype.toSQL = function() {
      if (this.op != null) {
        if ((this.r != null) && this.r.isType("ColExpr", "ValExpr", "FuncExpr")) {
          return "(" + (this.l.toSQL()) + " " + this.op + " " + (this.r.toSQL()) + ")";
        } else {
          return "(" + (this.l.toSQL()) + " " + this.op + " (" + (this.r.toSQL()) + "))";
        }
      } else {
        return this.l.toSQL();
      }
    };

    Expr.prototype.toJSString = function() {
      var op;
      if (this.op != null) {
        op = this.op;
        if (op === "=") {
          op = "==";
        }
        if (op === "AND") {
          op = "&&";
        }
        return "(" + (this.l.toJSString()) + " " + op + " (" + (this.r.toJSString()) + "))";
      } else {
        return this.l.toJSString();
      }
    };

    return Expr;

  })(Atom);

  SpecialExpr = (function(superClass) {
    extend(SpecialExpr, superClass);

    function SpecialExpr(v1, table) {
      this.v = v1;
      this.table = table != null ? table : null;
      this.tableName = null;
      if (this.table != null) {
        this.tableName = this.table.name;
      }
      SpecialExpr.__super__.constructor.apply(this, arguments);
    }

    SpecialExpr.prototype.clone = function() {
      return new SpecialExpr(this.v, this.table);
    };

    SpecialExpr.prototype.toSQL = function() {
      var prefix;
      prefix = "";
      if (this.tableName != null) {
        prefix = this.tableName + ".";
      }
      return "" + prefix + this.v;
    };

    SpecialExpr.prototype.toJSString = function() {
      if (this.tableName != null) {
        return "" + this.tableName;
      } else {
        throw new Error("SpecialExpr doesn't support toJSString: " + this.v);
      }
    };

    return SpecialExpr;

  })(Expr);

  BetweenExpr = (function(superClass) {
    extend(BetweenExpr, superClass);

    function BetweenExpr(v1, op1, minv1, maxv1) {
      this.v = v1;
      this.op = op1;
      this.minv = minv1;
      this.maxv = maxv1;
      BetweenExpr.__super__.constructor.apply(this, arguments);
    }

    BetweenExpr.prototype.clone = function() {
      var maxv, minv, v;
      v = this.v;
      if (this.v.clone != null) {
        v = this.v.clone();
      }
      minv = this.minv;
      if (this.minv.clone != null) {
        minv = this.minv.clone();
      }
      maxv = this.maxv;
      if (this.maxv.clone != null) {
        maxv = this.maxv.clone();
      }
      return new BetweenExpr(v, this.op, minv, maxv);
    };

    BetweenExpr.prototype.children = function() {
      return _.compact([this.v, this.minv, this.maxv]);
    };

    BetweenExpr.prototype.toSQL = function() {
      return (this.v.toSQL()) + " " + this.op + " " + (this.minv.toSQL()) + " AND " + (this.maxv.toSQL());
    };

    BetweenExpr.prototype.toJSString = function() {
      var ret;
      ret = ["(" + (this.v.toJSString()) + " >= " + (this.minv.toJSString()) + ")", "(" + (this.v.toJSString()) + " < " + (this.maxv.toJSString()) + ")"];
      ret = ret.join(" && ");
      if (this.op === "NOT BETWEEN") {
        ret = "!(" + ret + ")";
      }
      return ret;
    };

    return BetweenExpr;

  })(Expr);

  UnaryExpr = (function(superClass) {
    extend(UnaryExpr, superClass);

    function UnaryExpr(op1, expr) {
      this.op = op1;
      this.expr = expr;
      UnaryExpr.__super__.constructor.apply(this, arguments);
    }

    UnaryExpr.prototype.clone = function() {
      return new UnaryExpr(this.op, this.expr.clone());
    };

    UnaryExpr.prototype.children = function() {
      return [this.expr];
    };

    UnaryExpr.prototype.toSQL = function() {
      return this.op + " " + (this.expr.toSQL());
    };

    UnaryExpr.prototype.toJSString = function() {
      if (this.op === "NOT") {
        return "!(" + (this.expr.toJSString()) + ")";
      } else if (this.op === "NOT EXISTS") {
        return "!_.isEmpty(" + (this.expr.toJSString()) + ")";
      } else {
        return "" + this.op + (this.expr.toJSString());
      }
    };

    return UnaryExpr;

  })(Expr);

  FuncExpr = (function(superClass) {
    extend(FuncExpr, superClass);

    function FuncExpr(fname, exprs1) {
      this.fname = fname;
      this.exprs = exprs1;
      this.exprs = _.compact(_.flatten([this.exprs]));
      FuncExpr.__super__.constructor.apply(this, arguments);
    }

    FuncExpr.prototype.children = function() {
      return this.exprs;
    };

    FuncExpr.prototype.clone = function() {
      return new FuncExpr(this.fname, _.map(this.exprs, function(e) {
        return e.clone();
      }));
    };

    FuncExpr.prototype.isSQLFunc = function() {
      var ref;
      return (ref = this.fname) === "abs" || ref === "max" || ref === "min";
    };

    FuncExpr.prototype.toSQL = function() {
      var args;
      args = this.exprs.map(function(e) {
        return e.toSQL();
      }).join(",");
      return this.fname + "(" + args + ")";
    };

    FuncExpr.prototype.toJSString = function() {
      var args, f;
      f = (function() {
        switch (this.fname) {
          case "abs":
            return "Math.abs";
          case "max":
            return "Math.max";
          case "min":
            return "Math.min";
          default:
            return this.fname;
        }
      }).call(this);
      args = this.exprs.map(function(e) {
        return e.toJSString();
      }).join(",");
      return f + "(" + args + ")";
    };

    return FuncExpr;

  })(Expr);

  ColExpr = (function(superClass) {
    extend(ColExpr, superClass);

    function ColExpr(col, table) {
      this.col = col;
      this.table = table != null ? table : null;
      this.tableName = null;
      if (this.table != null) {
        this.tableName = this.table.name;
      }
      ColExpr.__super__.constructor.apply(this, arguments);
    }

    ColExpr.prototype.children = function() {
      return _.compact([this.table]);
    };

    ColExpr.prototype.clone = function() {
      return new ColExpr(this.col, this.table);
    };

    ColExpr.prototype.toSQL = function() {
      var prefix;
      prefix = "";
      if (this.tableName != null) {
        prefix = this.tableName + ".";
      }
      return "" + prefix + this.col;
    };

    ColExpr.prototype.toJSString = function() {
      return this.toSQL();
    };

    return ColExpr;

  })(Expr);

  TableExpr = (function(superClass) {
    extend(TableExpr, superClass);

    function TableExpr(table) {
      this.table = table;
      if (this.table == null) {
        throw new Error("TableExpr got null table");
      }
      this.tableName = null;
      if (this.table != null) {
        this.tableName = this.table.name;
      }
      TableExpr.__super__.constructor.apply(this, arguments);
    }

    TableExpr.prototype.clone = function() {
      return new TableExpr(this.table.clone());
    };

    TableExpr.prototype.toSQL = function() {
      return this.table.toSQL();
    };

    TableExpr.prototype.toJSString = function() {
      return this.tableName;
    };

    return TableExpr;

  })(Expr);

  ParamVar = (function(superClass) {
    extend(ParamVar, superClass);

    function ParamVar(name1, val) {
      this.name = name1;
      this.val = val != null ? val : null;
    }

    ParamVar.prototype.children = function() {
      return _.compact([this.val]);
    };

    ParamVar.prototype.clone = function() {
      return new ParamVar(this.name);
    };

    ParamVar.prototype.toSQL = function() {
      if (this.val != null) {
        return this.val.toSQL();
      }
      return "$" + this.name;
    };

    return ParamVar;

  })(Node);

  ParamExpr = (function(superClass) {
    extend(ParamExpr, superClass);

    function ParamExpr(expr, _default, params) {
      this.expr = expr;
      this["default"] = _default != null ? _default : null;
      this.params = params != null ? params : {};
      ParamExpr.__super__.constructor.apply(this, arguments);
    }

    ParamExpr.prototype.getVars = function() {
      return this.expr.descendents("ParamVar");
    };

    ParamExpr.prototype.areParamsFixed = function() {
      return _.all(this.getVars(), function(v) {
        return v.val != null;
      });
    };

    ParamExpr.prototype.getParams = function() {
      return this.getVars();
    };

    ParamExpr.prototype.getParamNames = function() {
      return _.pluck(this.getVars(), "name");
    };

    ParamExpr.prototype.setParams = function(params) {
      var i, len, pv, ref, results;
      this.params = params;
      ref = this.getVars();
      results = [];
      for (i = 0, len = ref.length; i < len; i++) {
        pv = ref[i];
        if (pv.name in this.params) {
          results.push(pv.val = this.params[pv.name]);
        } else {
          results.push(void 0);
        }
      }
      return results;
    };

    ParamExpr.prototype.children = function() {
      return _.compact([this["default"], this.expr]);
    };

    ParamExpr.prototype.clone = function() {
      var args;
      args = _.map([this.expr, this["default"]], function(v) {
        return (v != null) && v.clone() || null;
      });
      args.push(_.clone(this.params));
      return (function(func, args, ctor) {
        ctor.prototype = func.prototype;
        var child = new ctor, result = func.apply(child, args);
        return Object(result) === result ? result : child;
      })(ParamExpr, args, function(){});
    };

    ParamExpr.prototype.toSQL = function() {
      if (this.areParamsFixed()) {
        return this.expr.toSQL();
      }
      if (this["default"] != null) {
        if (this["default"].isType("SpecialExpr") && this["default"].v === null) {
          return null;
        }
        return this["default"].toSQL();
      }
      return null;
    };

    return ParamExpr;

  })(Expr);

  ValExpr = (function(superClass) {
    extend(ValExpr, superClass);

    function ValExpr(v1) {
      this.v = v1;
      ValExpr.__super__.constructor.apply(this, arguments);
    }

    ValExpr.prototype.children = function() {
      return [];
    };

    ValExpr.prototype.clone = function() {
      return new ValExpr(this.v);
    };

    ValExpr.prototype.toSQL = function() {
      if (_.isString(this.v)) {
        return "'" + this.v + "'";
      }
      if ((this.v != null) && _.isNumber(this.v)) {
        return this.v.toFixed(2);
      }
      return "" + this.v;
    };

    ValExpr.prototype.toJSString = function() {
      return this.toSQL();
    };

    return ValExpr;

  })(Expr);

  Table = (function(superClass) {
    extend(Table, superClass);

    function Table(name1, alias) {
      this.name = name1;
      this.alias = alias != null ? alias : null;
      this.isDefaultAlias = false;
      if (this.alias == null) {
        if (this.alias == null) {
          this.alias = this.name;
        }
        this.isDefaultAlias = true;
      }
      Table.__super__.constructor.apply(this, arguments);
    }

    Table.prototype.clone = function() {
      return new Table(this.name, this.alias);
    };

    Table.prototype.isExternalTable = function() {
      return false;
    };

    Table.prototype.toString = function(printAlias) {
      if (printAlias == null) {
        printAlias = true;
      }
      return this.name;
    };

    Table.prototype.toSQL = function(printAlias) {
      if (printAlias == null) {
        printAlias = true;
      }
      return this.name;
    };

    return Table;

  })(Node);

  ExternalTable = (function(superClass) {
    extend(ExternalTable, superClass);

    function ExternalTable(interactionName, name1) {
      this.interactionName = interactionName;
      this.name = name1;
      ExternalTable.__super__.constructor.apply(this, arguments);
    }

    ExternalTable.prototype.clone = function() {
      return new ExternalTable(this.interactionName, this.name);
    };

    ExternalTable.prototype.isExternalTable = function() {
      return true;
    };

    ExternalTable.prototype.toString = function() {
      return this.toSQL();
    };

    ExternalTable.prototype.toSQL = function() {
      return this.interactionName + "." + this.name;
    };

    return ExternalTable;

  })(Node);

  QueryTable = (function(superClass) {
    extend(QueryTable, superClass);

    function QueryTable(query, alias) {
      this.query = query;
      this.alias = alias != null ? alias : null;
      if (this.alias == null) {
        throw new Error("subquery needs to have an alias!  " + this.query);
      }
      QueryTable.__super__.constructor.apply(this, arguments);
    }

    QueryTable.prototype.clone = function() {
      return new QueryTable(this.query.clone(), this.alias);
    };

    QueryTable.prototype.children = function() {
      return [this.query];
    };

    QueryTable.prototype.toSQL = function() {
      return this.query.toSQL();
    };

    return QueryTable;

  })(Node);

  TableUDF = (function(superClass) {
    extend(TableUDF, superClass);

    function TableUDF(fname, exprs1, alias) {
      var ref;
      this.fname = fname;
      this.exprs = exprs1;
      this.alias = alias != null ? alias : null;
      if (this.alias == null) {
        throw new Error("UDF in FROM clause needs to have an alias! " + this.fname);
      }
      if ((ref = this.fname) === "abs" || ref === "min" || ref === "max") {
        throw new Error("SQL func should not be in FROM clause");
      }
      this.type = "TableUDF";
      this.exprs = _.compact(_.flatten([this.exprs]));
      TableUDF.__super__.constructor.apply(this, arguments);
    }

    TableUDF.prototype.clone = function() {
      return new TableUDF(this.fname, _.map(this.exprs, function(e) {
        return e.clone();
      }), this.alias);
    };

    TableUDF.prototype.traverse = function(f, path) {
      var newpath;
      if (path == null) {
        path = [];
      }
      f(this, path);
      newpath = _.append(path, this);
      return _.each(this.exprs, function(e) {
        return e.traverse(f, newpath);
      });
    };

    TableUDF.prototype.toSQL = function() {
      var args;
      args = this.exprs.map(function(e) {
        return e.toSQL();
      }).join(",");
      return this.fname + "(" + args + ")";
    };

    TableUDF.prototype.toJSString = function() {
      var args, f;
      f = this.fname;
      args = this.exprs.map(function(e) {
        return e.toJSString();
      }).join(",");
      return f + "(" + args + ")";
    };

    return TableUDF;

  })(Node);

  LetUDF = (function(superClass) {
    extend(LetUDF, superClass);

    function LetUDF(fname, args1, input, render_or_compute, source) {
      this.fname = fname;
      this.args = args1;
      this.input = input;
      this.render_or_compute = render_or_compute;
      this.source = source;
      this.type = "LetUDF";
      this.args = _.compact(_.flatten([this.args]));
      LetUDF.__super__.constructor.apply(this, arguments);
    }

    LetUDF.prototype.clone = function() {
      return new LetUDF(this.fname, _.map(this.exprs, function(e) {
        return e.clone();
      }), this.alias);
    };

    LetUDF.prototype.traverse = function(f, path) {
      var arg, i, len, newpath, ref, results;
      if (path == null) {
        path = [];
      }
      f(this, path);
      newpath = _.append(path, this);
      ref = this.args;
      results = [];
      for (i = 0, len = ref.length; i < len; i++) {
        arg = ref[i];
        results.push(arg.traverse(f, newpath));
      }
      return results;
    };

    LetUDF.prototype.clone = function() {
      var args;
      args = this.args.map(function(arg) {
        return arg.clone();
      });
      return new LetUDF(this.fname, args, this.exists);
    };

    LetUDF.prototype.toSQL = function() {
      return "";
    };

    LetUDF.prototype.schema = function() {
      return this.args.map(function(arg) {
        return arg.schema();
      });
    };

    return LetUDF;

  })(Node);

  LetUDFArg = (function(superClass) {
    extend(LetUDFArg, superClass);

    function LetUDFArg(name1, type) {
      this.name = name1;
      this.type = type;
      LetUDFArg.__super__.constructor.apply(this, arguments);
    }

    LetUDFArg.prototype.clone = function() {
      return new LetUDFArg(this.name, this.type);
    };

    LetUDFArg.prototype.toSQL = function() {
      return this.name + " " + this.type;
    };

    LetUDFArg.prototype.schema = function() {
      return {
        alias: this.name,
        type: this.type
      };
    };

    return LetUDFArg;

  })(Node);

  Group = (function(superClass) {
    extend(Group, superClass);

    function Group(groupinglist, having1) {
      this.groupinglist = groupinglist;
      this.having = having1;
      Group.__super__.constructor.apply(this, arguments);
    }

    Group.prototype.clone = function() {
      var having;
      having = null;
      if (this.having != null) {
        having = this.having.clone();
      }
      return new Group(this.groupinglist.map(function(g) {
        return g.clone();
      }, having));
    };

    Group.prototype.children = function() {
      return _.union(this.groupinglist, _.compact([this.having]));
    };

    Group.prototype.toSQL = function() {
      var grouping;
      grouping = _.compact(this.groupinglist.map(function(g) {
        return g.toSQL();
      }));
      if ((this.having != null) && this.having.children().length > 0) {
        return (grouping.join(", ")) + " HAVING " + (this.having.toSQL());
      } else {
        return grouping.join(", ");
      }
    };

    return Group;

  })(Node);

  Having = (function(superClass) {
    extend(Having, superClass);

    function Having(exprs1) {
      this.exprs = exprs1 != null ? exprs1 : [];
      this.exprs = _.compact(_.flatten([this.exprs]));
      Having.__super__.constructor.apply(this, arguments);
    }

    Having.prototype.children = function() {
      return this.exprs;
    };

    Having.prototype.clone = function() {
      var exprs;
      exprs = _.map(this.exprs, function(e) {
        return e.clone();
      });
      return new Having(exprs);
    };

    Having.prototype.toSQL = function() {
      return _.map(this.exprs, function(e) {
        return e.toSQL();
      }).join(", ");
    };

    return Having;

  })(Node);

  OrderBy = (function(superClass) {
    extend(OrderBy, superClass);

    function OrderBy(exprs1) {
      this.exprs = exprs1 != null ? exprs1 : [];
      this.exprs = _.compact(_.flatten([this.exprs]));
      OrderBy.__super__.constructor.apply(this, arguments);
    }

    OrderBy.prototype.children = function() {
      return this.exprs;
    };

    OrderBy.prototype.clone = function() {
      return new OrderBy(this.exprs.map(function(e) {
        return e.clone();
      }));
    };

    OrderBy.prototype.toSQL = function() {
      return this.exprs.map(function(e) {
        return e.toSQL();
      }).join(", ");
    };

    return OrderBy;

  })(Node);

  OrderByClause = (function(superClass) {
    extend(OrderByClause, superClass);

    function OrderByClause(expr, asc1) {
      this.expr = expr;
      this.asc = asc1 != null ? asc1 : true;
      OrderByClause.__super__.constructor.apply(this, arguments);
    }

    OrderByClause.prototype.clone = function() {
      return new OrderByClause(this.expr.clone(), this.asc);
    };

    OrderByClause.prototype.children = function() {
      return [this.expr];
    };

    OrderByClause.prototype.toSQL = function() {
      var asc;
      asc = this.asc ? "ASC" : "DESC";
      return (this.expr.toSQL()) + " " + asc;
    };

    return OrderByClause;

  })(Node);

  Limit = (function(superClass) {
    extend(Limit, superClass);

    function Limit(expr, offset1) {
      this.expr = expr;
      this.offset = offset1;
      Limit.__super__.constructor.apply(this, arguments);
    }

    Limit.prototype.clone = function() {
      var offset;
      offset = null;
      if (this.offset != null) {
        offset = this.offset.clone();
      }
      return new Limit(this.limit.clone(), offset);
    };

    Limit.prototype.children = function() {
      return _.compact([this.expr, this.offset]);
    };

    Limit.prototype.toSQL = function() {
      if (this.offset != null) {
        return (this.expr.toSQL()) + " OFFSET " + (this.offset.toSQL());
      } else {
        return this.expr.toSQL();
      }
    };

    return Limit;

  })(Expr);

  module.exports = {
    Queries: Queries,
    Statement: Statement,
    Atom: Atom,
    Tuple: Tuple,
    Predicate: Predicate,
    AggPredicate: AggPredicate,
    Table: Table,
    ExternalTable: ExternalTable,
    QueryTable: QueryTable,
    Expr: Expr,
    SpecialExpr: SpecialExpr,
    BetweenExpr: BetweenExpr,
    UnaryExpr: UnaryExpr,
    FuncExpr: FuncExpr,
    ColExpr: ColExpr,
    TableExpr: TableExpr,
    ParamVar: ParamVar,
    ParamExpr: ParamExpr,
    ValExpr: ValExpr,
    Group: Group,
    Having: Having,
    OrderBy: OrderBy,
    OrderByClause: OrderByClause,
    Limit: Limit
  };

}).call(this);
