%{
open Type
%}

%token <string> STRING IDENT
%token EOF
%token LPAREN RPAREN
%token STAR
%token ARROW

%nonassoc STAR
%right ARROW

%start type_expr
%type <Type.t> type_expr

%%

type_expr:
| atom                                     { Atom(Module_env.Path.empty, $1) }
| LPAREN type_expr RPAREN                  { $2 }
| type_expr_no_star STAR type_expr_no_star { Tuple2 ($1, $3) }
| type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star
  { Tuple3 ($1, $3, $5) }
| type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star
  { Tuple4 ($1, $3, $5, $7) }
| type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star STAR type_expr_no_star
  { Tuple5 ($1, $3, $5, $7, $9) }
| type_expr ARROW type_expr { Arrow (Nolabel, $1, $3) }
| type_expr_no_star atom    { Apply ($1, $2) }
;

/* [type_expr_no_star] exists to disambiguate the grammar. E.g. without it 'a * b * c' is ambiguous to
    be parsed both [(a*b) * c] and as a triple [a * b *c].
*/
type_expr_no_star:
| atom                       { Atom(Module_env.Path.empty, $1) }
| LPAREN type_expr RPAREN    { $2 }
| type_expr_no_star atom     { Apply ($1, $2) }

atom:
| IDENT                      { $1 }
;

%%
