{
open Type_parser
exception Error of string

let newline lexbuf = Lexing.new_line lexbuf
let error fmt =
  Printf.kprintf (fun msg -> raise (Error msg)) fmt
}

let eol = '\r'? '\n'
let space  = [' ' '\t']
let alpha  = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let ichar  = alpha | digit | ['_']
let ident  = alpha ichar*

rule token = parse
| space  { token lexbuf }
| eol    { newline lexbuf; token lexbuf }
| "("    { LPAREN }
| ")"    { RPAREN }
| "*"    { STAR }
| "->"   { ARROW }
| ident  { IDENT (Lexing.lexeme lexbuf) }
| eof    { EOF }
| _      { let token = Lexing.lexeme lexbuf in
           error "'%s' is not a valid token" token }
