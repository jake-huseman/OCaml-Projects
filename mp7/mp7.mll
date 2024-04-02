{
open Common;;

let int_of_binary b_str =
   let rec aux acc i =
      if i < 0 then acc
      else aux (acc + (if b_str.[i] = '1' then 1 lsl (String.length b_str - i - 1) else 0)) (i - 1)
   in aux 0 (String.length b_str - 1)
}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let upper = ['A'-'Z']
let underscore = '_'
let prime = '\''
let letter = lowercase | upper
let identifier_char = letter | digit | underscore | prime
let binary_digit = ['0'-'1']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let numeric = digit+
let binary = "0b" binary_digit+
let hex = "0x" hex_digit+
let float_num = digit+ '.' digit* | digit+ ('.' digit*)? ('e' ['+' '-']? digit+)?


rule token = parse
   | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
   | "(*"                 { comment 1 lexbuf }
   | "//" [^ '\n']* '\n'  { token lexbuf }
   | "\""                 { string_literal "" lexbuf }
   | eof             { EOF } 

(* your rules go here *)
   | "true"           { TRUE }
   | "false"          { FALSE }
   | "()"             { UNIT }
   | "let"            { LET }
   | "rec"            { REC }
   | "and"            { AND }
   | "end"            { END }
   | "in"             { IN }
   | "if"             { IF }
   | "then"           { THEN }
   | "else"           { ELSE }
   | "fun"            { FUN }
   | "mod"            { MOD }
   | "raise"          { RAISE }
   | "try"            { TRY }
   | "with"           { WITH }
   | "not"            { NOT }
   | lowercase (identifier_char)* as id { IDENT id }
   | "0b" binary_digit+ as b { INT (int_of_binary (String.sub b 2 (String.length b - 2))) }
   | "0x" hex_digit+ as h    { INT (int_of_string h) }
   | digit+ as n             { INT (int_of_string n) }
   | float_num as f          { FLOAT (float_of_string f) }
   | "+"              { PLUS }
   | "-"              { MINUS }
   | "*"              { TIMES }
   | "/"              { DIV }
   | "+."             { DPLUS }
   | "-."             { DMINUS }
   | "*."             { DTIMES }
   | "/."             { DDIV }
   | "^"              { CARAT }
   | "<"              { LT }
   | ">"              { GT }
   | "<="             { LEQ }
   | ">="             { GEQ }
   | "="              { EQUALS }
   | "<>"             { NEQ }
   | "|"              { PIPE }
   | "->"             { ARROW }
   | ";"              { SEMI }
   | ";;"             { DSEMI }
   | "::"             { DCOLON }
   | "@"              { AT }
   | "[]"             { NIL }
   | "&&"             { LOGICALAND }
   | "||"             { LOGICALOR }
   | "["              { LBRAC }
   | "]"              { RBRAC }
   | "("              { LPAREN }
   | ")"              { RPAREN }
   | ","              { COMMA }
   | "_"              { UNDERSCORE }

and string_literal acc = parse
   | "\""                        { STRING acc }
   | "\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] as d {
      let code = int_of_string (String.sub d 1 3) in
      if code >= 0 && code <= 255 then
         string_literal (acc ^ String.make 1 (char_of_int code)) lexbuf
      else
         raise (Failure "escape sequence out of range")
   }
   | "\\" (['\\' '\"' 't' 'n' 'r' 'b' ' '] as c) {
      let escaped_char = match c with
         | '\\' -> "\\"
         | '"'  -> "\""
         | 't'  -> "\t"
         | 'n'  -> "\n"
         | 'r'  -> "\r"
         | 'b'  -> "\b"
         | ' '  -> " "
         | _    -> failwith "unrecognized escape sequence"
      in string_literal (acc ^ escaped_char) lexbuf
   }
   | "\\" _                      { raise (Failure "illegal escape sequence") }
   | "\\" '\n' [' ' '\t']*       { string_literal acc lexbuf }
   | [^ '"' '\\']+ as str        { string_literal (acc ^ str) lexbuf }
   | eof                         { raise (Failure "unterminated string") }
   | _                           { string_literal acc lexbuf }

and comment depth = parse
   | "(*"           { comment (depth + 1) lexbuf }
   | "*)"           { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
   | eof            { raise (Failure "unmatched open comment") }
   | _              { comment depth lexbuf }

{(* do not modify this function: *)
let lextest s = token (Lexing.from_string s)

let get_all_tokens s =
   let b = Lexing.from_string (s^"\n") in
   let rec g () = 
   match token b with EOF -> []
   | t -> t :: g () in
   g ()

let try_get_all_tokens s =
   try (Some (get_all_tokens s), true)
   with Failure "unmatched open comment" -> (None, true)
   | Failure "unmatched closed comment" -> (None, false)
 }

