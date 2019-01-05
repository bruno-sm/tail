%{
  open Ast
%}

%token <int> INT_NUMBER
%token <float> REAL_NUMBER
%token <string> NAME
%token <string> STRING
%token <string> ATOM
%token SEQUENCE
%token NEWLINE
%token OPEN_PARENTHESES
%token CLOSE_PARENTHESES
%token LAMBDA
%token COLON
%token BLOCK_BEGIN
%token BLOCK_END
%token ASSIGN
%token COMMA
%token SPACE
/* Dummy token for give more precedence to commas declaring a tuple type */
%token TYPE_COMMA
%token IF
%token THEN
%token ELIF
%token ELSE
%token UNION
%token INTERSECTION
%token COMPLEMENT
%token INT_TYPE
%token REAL_TYPE
%token STRING_TYPE
%token ATOM_TYPE
%token <string> SPECIFIC_ATOM_TYPE
%token LIST_TYPE
%token MATRIX_TYPE
%token MATRIX_SEPARATOR
%token <int> UNIVERSE_TYPE
%token UNKNOWN_TYPE
%token ARROW
%token OPEN_LIST
%token CLOSE_LIST
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token UNKNOWN_TOKEN
%token EOF

%nonassoc ELSE
%left SEQUENCE
%left NEWLINE
%right ASSIGN
%nonassoc COLON
%left COMMA
%left UNION
%left INTERSECTION
%right ARROW
%nonassoc COMPLEMENT
%left TYPE_COMMA
%nonassoc OPEN_PARENTHESES

%start parse
%type <Ast.expression> parse
%%

parse:
  | NEWLINE?; e = sequence; EOF  { e }
;


sequence:
  | e = basic_expression; sequence_op; s = sequence
    { match s with
      | Sequence l -> Sequence (e::l)
      | _ -> Sequence []
    }

  | e1 = basic_expression; sequence_op?
    { Sequence [e1] }
;


%inline sequence_op:
  | SEQUENCE  {}
  | NEWLINE {}


basic_expression:
  | OPEN_PARENTHESES; s = sequence; CLOSE_PARENTHESES
    { Parentheses(s) }

  | b = block
    { b }

  | v = variable
    { v }

  | l = lambda
    { l }

  | f = function_call
    { f }

  | d = declaration
    { d }

  | a = assignment
    { a }

  | i = if_expression
    { i }

  | t = type_constructor
    { t }

  | t = type_expression
    { Type(t) }
;


block:
  | BLOCK_BEGIN; s = sequence; BLOCK_END
    { Block(s) }
;


lambda:
  | LAMBDA; v = NAME; COLON; t = type_expression; ASSIGN; e = basic_expression
    { Lambda(v, t, e) }

  | LAMBDA; v = NAME; ASSIGN; e = basic_expression
    { Lambda(v, Unknown, e) }
;


function_call:
  | f = basic_expression; OPEN_PARENTHESES; arg = sequence; CLOSE_PARENTHESES
    { FunctionCall(f, arg) }
;


declaration:
  | n = NAME; COLON; t = type_expression
    { Declaration(n, t) }
;


assignment:
  | n = NAME; ASSIGN; e = basic_expression
    { Assignment(n, e) }
;


variable:
  | n = NAME
    { Variable n }
;


if_expression:
  | IF; cond = sequence; THEN; do_if = sequence; do_elif = elif_expressions; do_else = else_expression
    { If(cond, do_if, do_elif, do_else) }
;


elif_expressions:
  | ELIF; cond = sequence; THEN; do_if = sequence; elifs = elif_expressions
    { Elif(cond, do_if)::elifs }

  | epsilon
    { [] }
;


else_expression:
  | ELSE; do_else = basic_expression
    { Else(do_else) }
;


type_expression:
  | t1 = type_expression; UNION; t2 = type_expression
    { Union(t1, t2) }

  | t1 = type_expression; INTERSECTION; t2 = type_expression
    { Intersection(t1, t2) }

  | COMPLEMENT; t = type_expression
    { Complement(t) }

  | t1 = type_expression; COMMA; t2 = type_expression %prec TYPE_COMMA
    { match t2 with
        | Tuple(types) -> Tuple(t1::types)
        | _ -> Tuple([t1; t2])
    }

  | ATOM_TYPE
    { Atom }

  | n = SPECIFIC_ATOM_TYPE
    { SpecificAtom(n) }

  | INT_TYPE
    { Int }

  | REAL_TYPE
    { Real }

  | STRING_TYPE
    { String }

  | LIST_TYPE
    { List(Unknown) }

  | LIST_TYPE; OPEN_BRACKET; t = type_expression; CLOSE_BRACKET
    { List(t) }

  | MATRIX_TYPE
    { Matrix(Unknown) }

  | MATRIX_TYPE; OPEN_BRACKET; t = type_expression; CLOSE_BRACKET
    { Matrix(t) }

  | u = UNIVERSE_TYPE
    { Universe(u) }

  | source = type_expression; ARROW; target = type_expression
    { Arrow(source, target) }

  | UNKNOWN_TYPE
    { Unknown }
;


type_constructor:
  | c = number_constructor
    { c }

  | c = string_constructor
    { c }

  | c = atom_constructor
    { c }

  | c = tuple_constructor
    { c }

  | c = list_constructor
    { c }

  | c = matrix_constructor
    { c }
;


number_constructor:
  | n = INT_NUMBER
    { IntLiteral(n) }

  | n = REAL_NUMBER
    { RealLiteral(n) }
;


string_constructor:
  | s = STRING
    { StringLiteral(s) }
;


atom_constructor:
  | n = ATOM
    { AtomLiteral(n) }
;


tuple_constructor:
  | t = tuple_constructor; COMMA; e = basic_expression
    { match t with
        | TupleLiteral(expressions) -> TupleLiteral(e::expressions)
        | _ -> TupleLiteral([e]) }

  | e1 = basic_expression; COMMA; e2 = basic_expression
    { TupleLiteral([e1; e2]) }
;


list_constructor:
  | OPEN_LIST; l = element_list; CLOSE_LIST
    { ListLiteral(l) }

  | OPEN_LIST; CLOSE_LIST
    { ListLiteral([]) }
;


matrix_constructor:
  | OPEN_BRACKET; m = matrix_elements; CLOSE_BRACKET
    { MatrixLiteral(m) }

  | OPEN_BRACKET; CLOSE_BRACKET
    { MatrixLiteral([]) }
;


matrix_elements:
  | l = element_list; MATRIX_SEPARATOR; option(MATRIX_SEPARATOR); m = matrix_elements
    { l::m }

  | l = element_list
    { [l] }
;


element_list:
  | e = basic_expression; SPACE; l = element_list
    { e::l }

  | e = basic_expression
    { [e] }
;


epsilon:
		{}
;
