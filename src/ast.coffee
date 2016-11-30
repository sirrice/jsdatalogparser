_ = require "underscore"
_.append = (arr, v) ->
  arr = _.clone(arr)
  arr.push v
  arr

DEBUG = no




class Node
  @id: 1
  constructor: (@type) ->
    @id = Node.id++

  isType:  -> @nodeType() in arguments
  isExpr: -> @nodeType() in [ "Expr", "SpecialExpr", "ColExpr", "ValExpr",
                              "BetweenExpr", "UnaryExpr", "FuncExpr", "TableExpr"]
  isTable: -> @nodeType() in ["Table", "QueryTable"]

  clone: -> throw Error "not implemented"

  nodeType: -> @constructor.name

  descendents: (types...) ->
    @allNodes (n) -> n.isType types...

  children: -> []

  # returns names of tables that Node depends on
  sources: -> @descendents "Table"

  variables: -> @descendents "ColExpr"

  # @param f given a node, returns true if it should be returned
  allNodes: (f) ->
    ret = []
    func = (node, path) ->
      ret.push node if f node

    @traverse func
    _.uniq ret

  traverse: (f, path=[]) -> 
    f @, path
    newpath = _.append path, @
    for child in @children()
      continue unless child?
      unless _.isFunction child.traverse
        console.log "ERR: child node doesn't support traverse().  Printing child, this and path"
        console.log child
        console.log @
        console.log path
      child.traverse f, newpath

  # what is run by a dbms
  toSQL: -> ""

  # readable formatting
  toPrettySQL: -> @toSQL()

  # in the DEVIL syntax
  toString: -> @toSQL()

  # subset of AST nodes can be translated into JS
  toJSString: ->
    throw new Error "toJSString() not implemented by #{@toSQL()}"

  toDot: ->
    output = []
    output.push "\ndigraph DepGraph#{@nprintcalls} {"
    output.push "  labelloc=\"t\";"
    output.push "  label=\"graph#{@nprintcalls}\";"

    f = (node, path) ->
      if path.length > 0
        parent = _.last(path)
        pname = parent.nodeType()
        name = node.nodeType()
        output.push "  #{pname}#{parent.id} -> #{name}#{node.id}"
    @traverse f

    output.push "}\n"
    s = output.join "\n"
    s




class Queries extends Node
  constructor: (@queries) ->
    super

  clone: ->
    new Queries(_.map @queries, (q) -> q.clone())

  children: -> @queries

  toSQL: ->
    sqls = _.map @queries, (q) -> q.toSQL()
    "#{sqls.join("\n")}"

class Statement extends Node
  constructor: (@head, @tail) ->
  children: -> _.flatten([@head, @tail])
  toSQL: ->
    head = @head.toSQL()
    tail = _.map @tail, (t) -> t.toSQL()
    "#{head} :- #{tail.join ", "}"

class Atom extends Node

class Predicate extends Atom
  constructor: (@tableName, @args) ->
  toSQL: ->
    "#{@tableName}(#{@args.join ", "})"

class AggPredicate extends Atom
  constructor: (@tableName, @args) ->
    @groups = _.filter @args, _.isString
    @aggs = _.reject @args, _.isString
  children: -> @aggs
  toSQL: ->
    groups = @groups.join ", "
    aggs = _.map(@aggs, (a) -> a.toSQL()).join ", "
    "#{@tableName}(#{groups}, #{aggs})"

class Expr extends Atom
  constructor: (@l, @op=null, @r=null) ->
    super

  clone: ->
    l = @l.clone()
    op = @op
    r = null
    r = @r.clone() if @r?
    new Expr l, op, r

  children: -> _.compact [@l, @r]

  toSQL: ->
    if @op?
      if @r? and @r.isType("ColExpr", "ValExpr", "FuncExpr")
        "(#{@l.toSQL()} #{@op} #{@r.toSQL()})"
      else
        "(#{@l.toSQL()} #{@op} (#{@r.toSQL()}))"
    else
      @l.toSQL()

  toJSString: ->
    if @op?
      op = @op
      op = "==" if op == "="
      op = "&&" if op == "AND"
      "(#{@l.toJSString()} #{op} (#{@r.toJSString()}))"
    else
      @l.toJSString()






# for strings such as * that should not be
# treated as literals
class SpecialExpr extends Expr
  constructor: (@v, @table=null) ->
    @tableName = null
    @tableName = @table.name if @table?
    super
  clone: -> new SpecialExpr @v, @table
  toSQL: ->
    prefix = ""
    prefix = "#{@tableName}." if @tableName?
    "#{prefix}#{@v}"
  toJSString: ->
    if @tableName?
      "#{@tableName}"
    else
      throw new Error("SpecialExpr doesn't support toJSString: #{@v}")

# For Event queries, checking if value is between two expressions
class BetweenExpr extends Expr
  constructor: (@v, @op, @minv, @maxv) ->
    super

  clone: -> 
    v = @v
    v = @v.clone() if @v.clone?
    minv = @minv
    minv = @minv.clone() if @minv.clone?
    maxv = @maxv
    maxv = @maxv.clone() if @maxv.clone?
    new BetweenExpr v, @op, minv, maxv

  children: -> _.compact [@v, @minv, @maxv]

  toSQL: ->
    "#{@v.toSQL()} #{@op} #{@minv.toSQL()} AND #{@maxv.toSQL()}"

  toJSString: ->
    ret = [
      "(#{@v.toJSString()} >= #{@minv.toJSString()})"
      "(#{@v.toJSString()} < #{@maxv.toJSString()})"
    ]
    ret = ret.join " && "
    if @op == "NOT BETWEEN"
      ret = "!(#{ret})"
    ret

class UnaryExpr extends Expr
  constructor: (@op, @expr) ->
    super
  clone: -> new UnaryExpr @op, @expr.clone()
  children: -> [@expr]
  toSQL: ->
    "#{@op} #{@expr.toSQL()}"
  toJSString: ->
    if @op == "NOT"
      "!(#{@expr.toJSString()})"
    else if @op == "NOT EXISTS"
      "!_.isEmpty(#{@expr.toJSString()})"
    else
      "#{@op}#{@expr.toJSString()}"

class FuncExpr extends Expr
  constructor: (@fname, @exprs) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: -> new FuncExpr @fname, _.map(@exprs, (e)->e.clone())

  isSQLFunc: -> @fname in ["abs", "max", "min"]

  toSQL: ->
    args = @exprs.map((e)->e.toSQL()).join ","
    "#{@fname}(#{args})"

  toJSString: ->
    f = switch @fname
      when "abs" then "Math.abs"
      when "max" then "Math.max"
      when "min" then "Math.min"
      else @fname
    args = @exprs.map((e)->e.toJSString()).join ","
    "#{f}(#{args})"



class ColExpr extends Expr
  constructor: (@col, @table=null) ->
    @tableName = null
    @tableName = @table.name if @table?
    super

  children: -> _.compact [@table]

  clone: -> new ColExpr @col, @table
  toSQL: ->
    prefix = ""
    prefix = "#{@tableName}." if @tableName?
    "#{prefix}#{@col}"

  toJSString: -> @toSQL()

class TableExpr extends Expr
  constructor: (@table) ->
    unless @table?
      throw new Error "TableExpr got null table"
    @tableName = null
    @tableName = @table.name if @table?
    super

  clone: -> new TableExpr @table.clone()
  toSQL: -> @table.toSQL()
  toJSString: -> @tableName

class ParamVar extends Node
  constructor: (@name, @val=null) ->

  children: -> _.compact [@val]
  clone: -> new ParamVar @name
  toSQL: -> 
    return @val.toSQL() if @val?
    "$#{@name}"

class ParamExpr extends Expr
  constructor: (@expr, @default=null, @params={}) ->
    super

  getVars: -> @expr.descendents "ParamVar"
  areParamsFixed: -> _.all(@getVars(), (v) -> v.val?)
  getParams: -> @getVars()
  getParamNames: -> _.pluck @getVars(), "name"
  setParams: (@params) ->
    for pv in @getVars()
      if pv.name of @params
        pv.val = @params[pv.name]

  children: -> _.compact [@default, @expr]

  clone: -> 
    args = _.map [@expr, @default], (v) -> v? and v.clone() or null
    args.push _.clone(@params)
    new ParamExpr args...

  toSQL: -> 
    return @expr.toSQL() if @areParamsFixed()
    if @default?
      if @default.isType("SpecialExpr") and @default.v is null
        return null
      return @default.toSQL() 
    return null

class ValExpr extends Expr
  constructor: (@v) ->
    super

  children: -> []
  clone: -> new ValExpr @v
  toSQL: ->
    return "'#{@v}'" if _.isString @v
    return @v.toFixed(2) if @v? and _.isNumber @v
    "#{@v}"
  toJSString: -> @toSQL()




##############################################
#
# Operators that don't really get any love
#
##############################################





class Table extends Node
  constructor: (@name, @alias=null) ->
    @isDefaultAlias = no
    unless @alias?
      @alias = @name unless @alias?
      @isDefaultAlias = yes
    super

  clone: ->
    new Table @name, @alias

  isExternalTable: -> no

  toString: (printAlias=yes) ->
    return @name 


  toSQL: (printAlias=yes) ->
    return @name 

class ExternalTable extends Node
  constructor: (@interactionName, @name) ->
    super

  clone: -> new ExternalTable @interactionName, @name

  isExternalTable: -> yes

  toString: -> @toSQL()

  toSQL: ->
    "#{@interactionName}.#{@name}"


class QueryTable extends Node
  constructor: (@query, @alias=null) ->
    unless @alias?
      throw new Error "subquery needs to have an alias!  #{@query}"
    super

  clone: ->
    new QueryTable @query.clone(), @alias

  children: -> [@query]

  # the FROM clause will add the "AS alias" clause
  toSQL: ->
    @query.toSQL()


class TableUDF extends Node
  constructor: (@fname, @exprs, @alias=null) ->
    unless @alias?
      throw new Error "UDF in FROM clause needs to have an alias! #{@fname}"
    unless @fname not in ["abs", "min", "max"]
      throw new Error "SQL func should not be in FROM clause"
    @type = "TableUDF"
    @exprs = _.compact _.flatten [@exprs]
    super

  clone: ->
    new TableUDF @fname, _.map(@exprs, (e)->e.clone()), @alias

  traverse: (f, path=[]) ->
    f @, path
    newpath = _.append path, @
    _.each @exprs, (e) -> e.traverse f, newpath

  toSQL: ->
    args = @exprs.map((e)->e.toSQL()).join ","
    "#{@fname}(#{args})"

  toJSString: ->
    f = @fname
    args = @exprs.map((e)->e.toJSString()).join ","
    "#{f}(#{args})"


class LetUDF extends Node
  constructor: (@fname, @args, @input, @render_or_compute, @source) ->
     @type = "LetUDF"
     @args = _.compact _.flatten [@args]
     super

  clone: ->
    new LetUDF @fname, _.map(@exprs, (e)->e.clone()), @alias

  traverse: (f, path=[]) ->
    f @, path
    newpath = _.append path, @
    for arg in @args
      arg.traverse f, newpath

  clone: ->
    args = @args.map (arg) -> arg.clone()
    new LetUDF @fname, args, @exists


  toSQL: -> ""

  schema: ->
    @args.map (arg) -> arg.schema()

class LetUDFArg extends Node
  constructor: (@name, @type) ->
    super
  clone: -> new LetUDFArg @name, @type
  toSQL: -> "#{@name} #{@type}"
  schema: ->
    alias: @name
    type: @type





class Group extends Node
  # @param gorupinglist list of Expr
  # @param having a single HAVING expression Expr object (not a list)
  #
  constructor: (@groupinglist, @having) ->
    super

  clone: ->
    having = null
    having = @having.clone() if @having?
    new Group(
      @groupinglist.map (g) -> g.clone(),
      having
    )

  children: -> _.union @groupinglist, _.compact([@having])

  toSQL: ->
    grouping = _.compact(@groupinglist.map (g) -> g.toSQL())
    if @having? and @having.children().length > 0
      "#{grouping.join ", "} HAVING #{@having.toSQL()}"
    else
      grouping.join ", "


class Having extends Node
  constructor: (@exprs=[]) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: ->
    exprs = _.map @exprs, (e) -> e.clone()
    new Having exprs

  toSQL: ->
    _.map(@exprs, (e) -> e.toSQL()).join ", "

class OrderBy extends Node
  constructor: (@exprs=[]) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: -> new OrderBy( @exprs.map (e) -> e.clone())

  toSQL: -> @exprs.map((e) -> e.toSQL()).join ", "

class OrderByClause extends Node
  constructor: (@expr, @asc=true) ->
    super


  clone: -> new OrderByClause @expr.clone(), @asc

  children: -> [@expr]

  toSQL: ->
    asc = if @asc then "ASC" else "DESC"
    "#{@expr.toSQL()} #{asc}"

class Limit extends Expr
  constructor: (@expr, @offset) ->
    super

  clone: ->
    offset = null
    offset = @offset.clone() if @offset?
    new Limit @limit.clone(), offset

  # old traverse() impl used to have the following line:
  #
  #     @expr.traverse f, path
  #
  # instead of 
  #
  #     @expr.traverse f, _.append(path, @)
  #
  # Not sure if that was a bug or not...
  children: -> _.compact [@expr, @offset]

  toSQL: ->
    if @offset?
      "#{@expr.toSQL()} OFFSET #{@offset.toSQL()}"
    else
      @expr.toSQL()


##############################################
#
# Function Query
#
##############################################

class FunctionQuery extends Node
  constructor: (@fname, @tableOrQuery) ->
    super
    unless @tableOrQuery?
      throw new Error "FunctionQuery cannot have empty argument"

  clone: ->
    new FunctionQuery @fname, @tableOrQuery.clone()

  children: -> [@tableOrQuery]

  toSQL: ->
    "#{@fname}(#{@tableOrQuery.toSQL()})"



#
# compute query's schema by recursively expanding all * clauses
# XXX: passes over queries to expand STAR project clauses
#
# @param queryName name of query to compute schema for
# @param nameToQueries dictionary mapping query names (e.g., in FROM clauses) to the Query AST objects
#
schema = (() ->
  get_schema = (queryName, nameToQueries, seen={}) ->
    return [] if queryName of seen
    seen[queryName] = yes

    query = nameToQueries[queryName]
    schema = query.schema()
    stars = _.filter schema, (s) -> s.type == "star"
    rest = _.reject schema, (s) -> s.type == "star"
    return rest unless stars.length

    # expand the schemas for SELECT * clauses
    starSchemas = for star in stars
      if star.table?
        get_schema star.table.name, nameToQueries, seen
      else
        sources = query.sources()
        unless sources.length == 1
          throw new Error("* project clause must be qualified with table name if >1 table source")
        get_schema sources[0].name, nameToQueries, seen

    tmp = null
    for starSchema in starSchemas
      tmp = starSchema unless tmp?
      isConsistent = _.chain(tmp)
        .zip(starSchema)
        .all((p) -> p[0].type == p[1].type)
        .value()
      unless isConsistent
        throw new Error "Inconsistent schemas: #{JSON.stringify tmp}
          ::: #{JSON.stringify starSchema}"

    rest.concat tmp
)()



module.exports =
  Queries           : Queries
  Statement         : Statement
  Predicate         : Predicate
  AggPredicate      : AggPredicate
  Table             : Table
  ExternalTable     : ExternalTable
  QueryTable        : QueryTable
  Expr              : Expr
  SpecialExpr       : SpecialExpr
  BetweenExpr       : BetweenExpr
  UnaryExpr         : UnaryExpr
  FuncExpr          : FuncExpr
  ColExpr           : ColExpr
  TableExpr         : TableExpr
  ParamVar          : ParamVar
  ParamExpr         : ParamExpr
  ValExpr           : ValExpr
  Group             : Group
  Having            : Having
  OrderBy           : OrderBy
  OrderByClause     : OrderByClause
  Limit             : Limit
  FunctionQuery     : FunctionQuery
  schema            : schema
