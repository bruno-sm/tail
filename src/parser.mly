%token <int> INT_NUMBER
%token <float> REAL_NUMBER
%token <string> NAME
%token <string> STRING
%token TRUE
%token FALSE
%token SEQUENCE
%token OPEN_PARENTHESES
%token CLOSE_PARENTHESES
%token LAMBDA
%token COLON
%token BLOCK_BEGIN
%token BLOCK_END
%token ASSIGN
%token COMMA
%token IF
%token ELIF
%token ELSE
%token UNION
%token INTERSECTION
%token COMPLEMENT
%token INT_TYPE
%token REAL_TYPE
%token BOOLEAN_TYPE
%token STRING_TYPE
%token LIST_TYPE
%token MATRIX_TYPE
%token UNKNOWN_TYPE
%token OPEN_LIST
%token CLOSE_LIST
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token SPACE

%start <int> parse
%%

parse:
  | e = expression  { 1 }
;


expression:
  | e1 = expression; SEQUENCE; e2 = expression
    {}

  | OPEN_PARENTHESES; expression; CLOSE_PARENTHESES
    {}

  | block
    {}

  | lambda
    {}

  | function_call
    {}

  | declaration
    {}

  | assignment
    {}

  | if_expression
    {}

  | type_constructor
    {}

  | type_expression
    {}
;


block:
  | BLOCK_BEGIN; e = expression; BLOCK_END
    {}
;


lambda:
  | LAMBDA; n = name; COLON; t = type_expression; ASSIGN; expression
    {}

  | LAMBDA; n = name; ASSIGN; expression
    {}
;


function_call:
  | n = name; OPEN_PARENTHESES; e = expression; CLOSE_PARENTHESES
    {}
;


declaration:
  | n = name; COLON; t = type_expression
    {}
;


assignment:
  | n = name; ASSIGN;  e = expression
    {}
;


name:
  | n = NAME
    {}
;


if_expression:
  | IF; cond = expression; do_if = expression; do_elif = elif_expression; do_else = else_expression
    {}
;


elif_expression:
  | ELIF; cond = expression; do_if = expression; do_elif = elif_expression
    {}

  | epsilon
    {}
;


else_expression:
  | ELSE; do_else = expression
    {}
  | epsilon
    {}
;


type_expression:
  | t1 = type_expression; UNION; t2 = type_expression
    {}

  | t1 = type_expression; INTERSECTION; t2 = type_expression
    {}

  | COMPLEMENT; t = type_expression
    {}

  | t1 = type_expression; COMMA; t2 = type_expression
    {}

  | INT_TYPE
    {}

  | REAL_TYPE
    {}

  | BOOLEAN_TYPE
    {}

  | STRING_TYPE
    {}

  | LIST_TYPE
    {}

  | LIST_TYPE; OPEN_BRACKET; t = type_expression; CLOSE_BRACKET
    {}

  | MATRIX_TYPE
    {}

  | MATRIX_TYPE; OPEN_BRACKET; t = type_expression; CLOSE_BRACKET
    {}

  | UNKNOWN_TYPE
    {}
;


type_constructor:
  | c = number_constructor
    {}

  | c = bool_constructor
    {}

  | c = string_constructor
    {}

  | c = atom_constructor
    {}

  | c = tuple_constructor
    {}

  | c = list_constructor
    {}

  | c = matrix_constructor
    {}
;


number_constructor:
  | n = INT_NUMBER
    {}

  | n = REAL_NUMBER
    {}
;


bool_constructor:
  | TRUE
    {}

  | FALSE
    {}
;


string_constructor:
  | s = STRING
    {}
;


atom_constructor:
  | COLON; n = name
    {}
;


tuple_constructor:
  | e = expression; COMMA; t = tuple_constructor
    {}

  | e = expression
    {}
;


list_constructor:
  | OPEN_LIST; l = element_list; CLOSE_LIST
    {}

  | OPEN_LIST; CLOSE_LIST
    {}
;


matrix_constructor:
  | OPEN_BRACKET; m = matrix_elements; CLOSE_BRACKET
    {}

  | OPEN_BRACKET; CLOSE_BRACKET
    {}
;


matrix_elements:
  | l = element_list; SEQUENCE; m = matrix_elements
    {}

  | l = element_list
    {}
;


element_list:
  | e = expression; SPACE; l = element_list
    {}

  | e = expression
    {}
;


epsilon:
		{}
;
