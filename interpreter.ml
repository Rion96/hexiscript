type token =
  | NAME of string
  | FUN of string | ENDFUN
  | INT of int | TONUM
  | FLOAT of float
  | BOOL of bool
  | CHAR of char | TOCHAR
  | STR of string | TOSTR
  | INCHAN of in_channel | OPENIN | CLOSE | CATCH | THROW
  | OUTCHAN of out_channel | OPENOUT | READ | WRITE
  | ARRAY of token array | ARR | DREF | FLOOR | CEIL
  | LET | PRINT | PRINTLN | POSTFIX_INCR | POSTFIX_DECR
  | IF | ELSE | ENDIF | WHILE | ENDWHILE | FOR | ENDFOR
  | NEWLINE | ERROR | LIST of token list | SEQ | RETURN
  | UNOP_MINUS | NOT | POW | PREFIX_INCR | PREFIX_DECR
  | PLUS | MINUS | MULT | DIV | MOD | SCAN | LEN | INCLUDE | EXP
  | START | END | BR_START | BR_END | BREAK | RAND | FREE | SQRT
  | AND | OR | GREATER | LESS | GEQ | LEQ | NEQ | EQ | LN | LOG | ABS
  | B_AND | B_OR | B_XOR | B_NOT | B_SHIFTL | B_LSHIFTR | B_ASHIFTR
  | SIN | COS | TAN | ASIN | ACOS | ATAN | SINH | COSH | TANH

exception InvalidToken of (token * string)
exception InvalidFunction of string
exception InvalidFilename of string

let rec string_of_token (tok : token) = (
  let print_list (l : token list) = (
    let rec loop l buf = (
      match l with
      | [hd] -> (buf ^ (string_of_token hd))
      | hd :: tl -> loop tl (buf ^ (string_of_token hd) ^ ", ")
      | [] -> buf
    ) in
    ("[" ^ (loop l "") ^ "]")
  ) in
  match tok with
  | NAME s        -> ("NAME " ^ s)
  | FUN s         -> ("FUN " ^ s)
  | ENDFUN        -> "ENDFUN"
  | INT i         -> ("INT " ^ string_of_int i)
  | TONUM         -> "TONUM"
  | FLOAT f       -> ("FLOAT " ^ string_of_float f)
  | BOOL b        -> ("BOOL " ^ string_of_bool b)
  | CHAR c        -> ("CHAR " ^ String.make 1 c)
  | TOCHAR        -> "TOCHAR"
  | STR s         -> ("STRING " ^ s)
  | TOSTR         -> "TOSTR"
  | INCHAN _      -> ("INCHAN")
  | OPENIN        -> "OPENIN"
  | CLOSE         -> "CLOSE"
  | CATCH         -> "CATCH"
  | THROW         -> "THROW"
  | OUTCHAN _     -> "OUTCHAN" 
  | OPENOUT       -> "OPENOUT"
  | READ          -> "READ"
  | WRITE         -> "WRITE"
  | ARRAY _       -> "ARRAY"
  | ARR           -> "ARR"
  | DREF          -> "DREF"
  | FLOOR         -> "FLOOR"
  | CEIL          -> "CEIL"
  | LET           -> "LET"
  | PRINT         -> "PRINT"
  | PRINTLN       -> "PRINTLN"
  | POSTFIX_INCR  -> "POSTFIX_INCR"
  | POSTFIX_DECR  -> "POSTFIX_DECR"
  | IF            -> "IF"
  | ELSE          -> "ELSE"
  | ENDIF         -> "ENDIF"
  | WHILE         -> "WHILE"
  | ENDWHILE      -> "ENDWHILE"
  | FOR           -> "FOR"
  | ENDFOR        -> "ENDFOR"
  | NEWLINE       -> "NEWLINE"
  | ERROR         -> "ERROR"
  | LIST l        -> ("LIST " ^ (print_list l))
  | SEQ           -> "SEQ"
  | RETURN        -> "RETURN"
  | UNOP_MINUS    -> "UNOP_MINUS"
  | NOT           -> "NOT"
  | POW           -> "POW"
  | PREFIX_INCR   -> "PREFIX_INCR"
  | PREFIX_DECR   -> "PREFIX_DECR"
  | PLUS          -> "PLUS"
  | MINUS         -> "MINUS"
  | MULT          -> "MULT"
  | DIV           -> "DIV"
  | MOD           -> "MOD"
  | SCAN          -> "SCAN"
  | LEN           -> "LEN"
  | INCLUDE       -> "INCLUDE"
  | START         -> "START"
  | END           -> "END"
  | BR_START      -> "BR_START"
  | BR_END        -> "BR_END"
  | BREAK         -> "BREAK"
  | RAND          -> "RAND"
  | AND           -> "AND"
  | OR            -> "OR"
  | GREATER       -> "GREATER"
  | LESS          -> "LESS"
  | GEQ           -> "GEQ"
  | LEQ           -> "LEQ"
  | NEQ           -> "NEQ"
  | EQ            -> "EQ"
  | B_AND         -> "B_AND"
  | B_OR          -> "B_OR"
  | B_XOR         -> "B_XOR"
  | B_NOT         -> "B_NOT"
  | B_SHIFTL      -> "B_SHIFTL"
  | B_LSHIFTR     -> "B_LSHIFTR"
  | B_ASHIFTR     -> "B_ASHIFTR"
  | SIN           -> "SIN"
  | COS           -> "COS"
  | TAN           -> "TAN"
  | ASIN          -> "ASIN"
  | ACOS          -> "ACOS"
  | ATAN          -> "ATAN"
  | LN            -> "LN"
  | LOG           -> "LOG"
  | SQRT          -> "SQRT"
  | ABS           -> "ABS"
  | EXP           -> "EXP"
  | FREE          -> "FREE"
  | SINH          -> "SINH"
  | COSH          -> "COSH"
  | TANH          -> "TANH"
)

let tokenizer (stack : char list) = (
  let sep (c : char) = (
    match c with
    | ' ' | '\n' | ';' | '\t' | '!' 
    | '(' | '[' | '+' | '-' | '~' 
    | '|' | '&' | '>' | '<' | '^' -> true
    | _ -> false
  ) in
  let postfix (c : char) = (
    match c with
    | 'A'..'Z' | 'a'..'z' | ']' -> true
    | _ -> false
  ) in
  let rec ignore_token = (
    function
    | NEWLINE :: stack -> NEWLINE :: stack
    | hd :: tl -> ignore_token tl
    | [] -> []
  ) in
  let rec num_token (buffer : string) = (
    function
    | hd :: tl when hd = 'b' || hd = 'x' || hd = 'o' || hd = '.'
                 || hd >= 'A' && hd <= 'F'
                 || hd >= 'a' && hd <= 'f'
                 || hd >= '0' && hd <= '9' ->
      num_token ((String.make 1 hd) ^ buffer) tl
    | stack -> 
      try
        INT (int_of_string buffer), stack
      with exn ->
        try
          FLOAT (float_of_string buffer), stack
        with exn ->
          ERROR, stack
  ) in
  let char_token = (
    function
    | '\\' :: '\\' :: '\'' :: stack -> CHAR '\\', stack
    | '\"' :: '\\' :: '\'' :: stack -> CHAR '\"', stack
    | '\'' :: '\\' :: '\'' :: stack -> CHAR '\'', stack
    | 'n' :: '\\' :: '\'' :: stack -> CHAR '\n', stack
    | 'r' :: '\\' :: '\'' :: stack -> CHAR '\r', stack
    | 't' :: '\\' :: '\'' :: stack -> CHAR '\t', stack
    | 'b' :: '\\' :: '\'' :: stack -> CHAR '\b', stack
    | c :: '\'' :: stack -> CHAR c, stack
    | stack -> ERROR, stack
  ) in
  let rec str_token (buffer : string) = (
    function
    | '\\' :: '\\' :: tl -> str_token ((String.make 1 '\\') ^ buffer) tl
    | '\"' :: '\\' :: tl -> str_token ((String.make 1 '\"') ^ buffer) tl
    | '\'' :: '\\' :: tl -> str_token ((String.make 1 '\'') ^ buffer) tl
    | 'n' :: '\\' :: tl -> str_token ((String.make 1 '\n') ^ buffer) tl
    | 'r' :: '\\' :: tl -> str_token ((String.make 1 '\r') ^ buffer) tl
    | 't' :: '\\' :: tl -> str_token ((String.make 1 '\t') ^ buffer) tl
    | 'b' :: '\\' :: tl -> str_token ((String.make 1 '\b') ^ buffer) tl
    | '"' :: stack -> STR buffer, stack
    | hd :: tl -> str_token ((String.make 1 hd) ^ buffer) tl
    | [] -> ERROR, []
  ) in
  let rec name_token (buffer : string) (input : char list) = (
    match input with
    | c :: _ when sep c -> NAME buffer, input
    | 'x' :: '0' :: tl ->
      num_token ("0x" ^ buffer) tl
    | '_' :: ('A'..'Z' as c) :: tl
    | '_' :: ('a'..'z' as c) :: tl ->
      name_token ((String.make 1 c) ^ "_" ^ buffer) tl
    | ('A'..'Z' as hd) :: tl
    | ('a'..'z' as hd) :: tl ->
      name_token ((String.make 1 hd) ^ buffer) tl
    | _ :: stack -> ERROR, stack
    | [] -> ERROR, []
  ) in
  let rec main_parser (buffer : token list) = (
    function
    | ';' :: stack ->
      main_parser (SEQ :: buffer) stack
    | '\n' :: stack ->
      main_parser (NEWLINE :: buffer) stack
    | ']' :: stack ->
      main_parser (BR_END :: buffer) stack
    | '[' :: stack ->
      main_parser (BR_START :: buffer) stack
    | ')' :: stack ->
      main_parser (END :: buffer) stack
    | '+' :: '+' :: [] -> (PREFIX_INCR :: buffer)
    | '+' :: '+' :: c :: tl when sep c ->
      main_parser (PREFIX_INCR :: buffer) (c :: tl)
    | '+' :: '+' :: c :: tl when postfix c ->
      main_parser (POSTFIX_INCR :: buffer) (c :: tl)
    | '(' :: stack
    | '+' :: '(' :: stack ->
      main_parser (START :: buffer) stack
    | '-' :: '-' :: [] -> (PREFIX_DECR :: buffer)
    | '-' :: '-' :: c :: tl when sep c ->
      main_parser (PREFIX_DECR :: buffer) (c :: tl)
    | '-' :: '-' :: c :: tl when postfix c ->
      main_parser (POSTFIX_DECR :: buffer) (c :: tl)
    | '-' :: '(' :: stack ->
      main_parser (START :: UNOP_MINUS :: buffer) stack
    | '-' :: stack ->
      main_parser (MINUS :: buffer) stack
    | '+' :: stack ->
      main_parser (PLUS :: buffer) stack
    | '*' :: stack ->
      main_parser (MULT :: buffer) stack
    | '/' :: stack ->
      main_parser (DIV :: buffer) stack
    | '%' :: stack ->
      main_parser (MOD :: buffer) stack
    | '^' :: '^' :: stack ->
      main_parser (POW :: buffer) stack
    | '^' :: stack ->
      main_parser (B_XOR :: buffer) stack
    | '!' :: stack ->
      main_parser (NOT :: buffer) stack
    | '~' :: stack ->
      main_parser (B_NOT :: buffer) stack
    | '|' :: '|' :: stack ->
      main_parser (OR :: buffer) stack
    | '|' :: stack ->
      main_parser (B_OR :: buffer) stack
    | '&' :: '&' :: stack ->
      main_parser (AND :: buffer) stack
    | '&' :: stack ->
      main_parser (B_AND :: buffer) stack
    | '=' :: '>' :: stack ->
      main_parser (GEQ :: buffer) stack
    | '=' :: '<' :: stack ->
      main_parser (LEQ :: buffer) stack
    | '=' :: '!' :: stack ->
      main_parser (NEQ :: buffer) stack
    | '=' :: stack ->
      main_parser (EQ :: buffer) stack
    | '<' :: '<' :: stack ->
      main_parser (B_SHIFTL :: buffer) stack
    | '<' :: stack ->
      main_parser (LESS :: buffer) stack
    | '>' :: '>' :: '>' :: stack ->
      main_parser (B_ASHIFTR :: buffer) stack
    | '>' :: '>' :: stack ->
      main_parser (B_LSHIFTR :: buffer) stack
    | '>' :: stack ->
      main_parser (GREATER :: buffer) stack
    | '#' :: stack ->
      main_parser (ignore_token buffer) stack
    | '\t' :: stack
    | ' ' :: stack ->
      main_parser buffer stack
    | '"' :: stack ->
      let tok, stack = str_token "" stack in
      main_parser (tok :: buffer) stack
    (* println *)
    | 'n' :: 'l' :: 't' :: 'n' :: 'i' :: 'r' :: 'p' :: [] -> (PRINTLN :: buffer)
    | 'n' :: 'l' :: 't' :: 'n' :: 'i' :: 'r' :: 'p' :: c :: tl when sep c ->
      main_parser (PRINTLN :: buffer) (c :: tl)
    (* print *)
    | 't' :: 'n' :: 'i' :: 'r' :: 'p' :: [] -> (PRINT :: buffer)
    | 't' :: 'n' :: 'i' :: 'r' :: 'p' :: c :: tl when sep c ->
      main_parser (PRINT :: buffer) (c :: tl)
    (* let *)
    | 't' :: 'e' :: 'l' :: [] -> (LET :: buffer)
    | 't' :: 'e' :: 'l' :: c :: tl when sep c ->
      main_parser (LET :: buffer) (c :: tl)
    (* true *)
    | 'e' :: 'u' :: 'r' :: 't' :: [] -> (BOOL true :: buffer)
    | 'e' :: 'u' :: 'r' :: 't' :: c :: tl when sep c ->
      main_parser (BOOL true :: buffer) (c :: tl)
    (* false *)
    | 'e' :: 's' :: 'l' :: 'a' :: 'f' :: [] -> (BOOL false :: buffer)
    | 'e' :: 's' :: 'l' :: 'a' :: 'f' :: c :: tl when sep c ->
      main_parser (BOOL false :: buffer) (c :: tl)
    (* endif *)
    | 'f' :: 'i' :: 'd' :: 'n' :: 'e' :: [] -> (ENDIF :: buffer)
    | 'f' :: 'i' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      main_parser (ENDIF :: buffer) (c :: tl)
    (* if *)
    | 'f' :: 'i' :: [] -> (IF :: buffer)
    | 'f' :: 'i' :: c :: tl when sep c ->
      main_parser (IF :: buffer) (c :: tl)
    (* else *)
    | 'e' :: 's' :: 'l' :: 'e' :: [] -> (ELSE :: buffer)
    | 'e' :: 's' :: 'l' :: 'e' :: c :: tl when sep c ->
      main_parser (ELSE :: buffer) (c :: tl)
    (* endwhile *)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: 'd' :: 'n' :: 'e' :: [] -> (ENDWHILE :: buffer)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      main_parser (ENDWHILE :: buffer) (c :: tl)
    (* while *)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: [] -> (WHILE :: buffer)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: c :: tl when sep c ->
      main_parser (WHILE :: buffer) (c :: tl) 
    (* endfun *)
    | 'n' :: 'u' :: 'f' :: 'd' :: 'n' :: 'e' :: [] -> (ENDFUN :: buffer)
    | 'n' :: 'u' :: 'f' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      main_parser (ENDFUN :: buffer) (c :: tl)
    (* fun *)
    | 'n' :: 'u' :: 'f' :: [] -> (FUN "_DEF_" :: buffer)
    | 'n' :: 'u' :: 'f' :: c :: tl when sep c ->
      main_parser (FUN "_DEF_" :: buffer) (c :: tl)
    (* return *)
    | 'n' :: 'r' :: 'u' :: 't' :: 'e' :: 'r' :: [] -> (RETURN :: buffer)
    | 'n' :: 'r' :: 'u' :: 't' :: 'e' :: 'r' :: c :: tl when sep c ->
      main_parser (RETURN :: buffer) (c :: tl)
    (* arr *)
    | 'r' :: 'r' :: 'a' :: [] -> (ARR :: buffer)
    | 'r' :: 'r' :: 'a' :: c :: tl when sep c ->
      main_parser (ARR :: buffer) (c :: tl)
    (* endfor *)
    | 'r' :: 'o' :: 'f' :: 'd' :: 'n' :: 'e' :: [] -> (ENDFOR :: buffer)
    | 'r' :: 'o' :: 'f' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      main_parser (ENDFOR :: buffer) (c :: tl)
    (* for *)
    | 'r' :: 'o' :: 'f' :: [] -> (FOR :: buffer)
    | 'r' :: 'o' :: 'f' :: c :: tl when sep c ->
      main_parser (FOR :: buffer) (c :: tl)
    (* break *)
    | 'k' :: 'a' :: 'e' :: 'r' :: 'b' :: [] -> (BREAK :: buffer)
    | 'k' :: 'a' :: 'e' :: 'r' :: 'b' :: c :: tl when sep c ->
      main_parser (BREAK :: buffer) (c :: tl)
    (* scan *)
    | 'n' :: 'a' :: 'c' :: 's' :: [] -> (SCAN :: buffer)
    | 'n' :: 'a' :: 'c' :: 's' :: c :: tl when sep c ->
      main_parser (SCAN :: buffer) (c :: tl)
    (* len *)
    | 'n' :: 'e' :: 'l' :: [] -> (LEN :: buffer)
    | 'n' :: 'e' :: 'l' :: c :: tl when sep c ->
      main_parser (LEN :: buffer) (c :: tl)
    (* rand *)
    | 'd' :: 'n' :: 'a' :: 'r' :: [] -> (RAND :: buffer)
    | 'd' :: 'n' :: 'a' :: 'r' :: c :: tl when sep c ->
      main_parser (RAND :: buffer) (c :: tl)
    (* floor *)
    | 'r' :: 'o' :: 'o' :: 'l' :: 'f' :: [] -> (FLOOR :: buffer)
    | 'r' :: 'o' :: 'o' :: 'l' :: 'f' :: c :: tl when sep c ->
      main_parser (FLOOR :: buffer) (c :: tl)
    (* ceil *)
    | 'l' :: 'i' :: 'e' :: 'c' :: [] -> (CEIL :: buffer)
    | 'l' :: 'i' :: 'e' :: 'c' :: c :: tl when sep c ->
      main_parser (CEIL :: buffer) (c :: tl)
    (* include *)
    | 'e' :: 'd' :: 'u' :: 'l' :: 'c' :: 'n' :: 'i' :: [] -> (INCLUDE :: buffer)
    | 'e' :: 'd' :: 'u' :: 'l' :: 'c' :: 'n' :: 'i' :: c :: tl when sep c ->
      main_parser (INCLUDE :: buffer) (c :: tl)
    (* tonum *)
    | 'm' :: 'u' :: 'n' :: 'o' :: 't' :: [] -> (TONUM :: buffer)
    | 'm' :: 'u' :: 'n' :: 'o' :: 't' :: c :: tl when sep c ->
      main_parser (TONUM :: buffer) (c :: tl)
    (* openin *)
    | 'n' :: 'i' :: 'n' :: 'e' :: 'p' :: 'o' :: [] -> (OPENIN :: buffer)
    | 'n' :: 'i' :: 'n' :: 'e' :: 'p' :: 'o' :: c :: tl when sep c ->
      main_parser (OPENIN :: buffer) (c :: tl)
    (* openout *)
    | 't' :: 'u' :: 'o' :: 'n' :: 'e' :: 'p' :: 'o' :: [] -> (OPENOUT :: buffer)
    | 't' :: 'u' :: 'o' :: 'n' :: 'e' :: 'p' :: 'o' :: c :: tl when sep c ->
      main_parser (OPENOUT :: buffer) (c :: tl)
    (* close *)
    | 'e' :: 's' :: 'o' :: 'l' :: 'c' :: [] -> (CLOSE :: buffer)
    | 'e' :: 's' :: 'o' :: 'l' :: 'c' :: c :: tl when sep c ->
      main_parser (CLOSE :: buffer) (c :: tl)
    (* read *)
    | 'd' :: 'a' :: 'e' :: 'r' :: [] -> (READ :: buffer)
    | 'd' :: 'a' :: 'e' :: 'r' :: c :: tl when sep c ->
      main_parser (READ :: buffer) (c :: tl)
    (* write *)
    | 'e' :: 't' :: 'i' :: 'r' :: 'w' :: [] -> (WRITE :: buffer)
    | 'e' :: 't' :: 'i' :: 'r' :: 'w' :: c :: tl when sep c ->
      main_parser (WRITE :: buffer) (c :: tl)
    (* throw *)
    | 'w' :: 'o' :: 'r' :: 'h' :: 't' :: [] -> (THROW :: buffer)
    | 'w' :: 'o' :: 'r' :: 'h' :: 't' :: c :: tl when sep c ->
      main_parser (THROW :: buffer) (c :: tl)
    (* catch *)
    | 'h' :: 'c' :: 't' :: 'a' :: 'c' :: [] -> (CATCH :: buffer)
    | 'h' :: 'c' :: 't' :: 'a' :: 'c' :: c :: tl when sep c ->
      main_parser (CATCH :: buffer) (c :: tl)
    (* tochar *)
    | 'r' :: 'a' :: 'h' :: 'c' :: 'o' :: 't' :: [] -> (TOCHAR :: buffer)
    | 'r' :: 'a' :: 'h' :: 'c' :: 'o' :: 't' :: c :: tl when sep c ->
      main_parser (TOCHAR :: buffer) (c :: tl)
    (* tostr *)
    | 'r' :: 't' :: 's' :: 'o' :: 't' :: [] -> (TOSTR :: buffer)
    | 'r' :: 't' :: 's' :: 'o' :: 't' :: c :: tl when sep c ->
      main_parser (TOSTR :: buffer) (c :: tl)
    (* sinh *)
    | 'h' :: 'n' :: 'i' :: 's' :: [] -> (SINH :: buffer)
    | 'h' :: 'n' :: 'i' :: 's' :: c :: tl when sep c ->
      main_parser (SINH :: buffer) (c :: tl)
    (* asin *)
    | 'n' :: 'i' :: 's' :: 'a' :: [] -> (ASIN :: buffer)
    | 'n' :: 'i' :: 's' :: 'a' :: c :: tl when sep c ->
      main_parser (ASIN :: buffer) (c :: tl)
    (* sin *)
    | 'n' :: 'i' :: 's' :: [] -> (SIN :: buffer)
    | 'n' :: 'i' :: 's' :: c :: tl when sep c ->
      main_parser (SIN :: buffer) (c :: tl)
    (* cosh *)
    | 'h' :: 's' :: 'o' :: 'c' :: [] -> (COSH :: buffer)
    | 'h' :: 's' :: 'o' :: 'c' :: c :: tl when sep c ->
      main_parser (COSH :: buffer) (c :: tl)
    (* acos *)
    | 's' :: 'o' :: 'c' :: 'a' :: [] -> (ACOS :: buffer)
    | 's' :: 'o' :: 'c' :: 'a' :: c :: tl when sep c ->
      main_parser (ACOS :: buffer) (c :: tl)
    (* cos *)
    | 's' :: 'o' :: 'c' :: [] -> (COS :: buffer)
    | 's' :: 'o' :: 'c' :: c :: tl when sep c ->
      main_parser (COS :: buffer) (c :: tl)
    (* tanh *)
    | 'h' :: 'n' :: 'a' :: 't' :: [] -> (TANH :: buffer)
    | 'h' :: 'n' :: 'a' :: 't' :: c :: tl when sep c ->
      main_parser (TANH :: buffer) (c :: tl)
    (* atan *)
    | 'n' :: 'a' :: 't' :: 'a' :: [] -> (ATAN :: buffer)
    | 'n' :: 'a' :: 't' :: 'a' :: c :: tl when sep c ->
      main_parser (ATAN :: buffer) (c :: tl)
    (* tan *)
    | 'n' :: 'a' :: 't' :: [] -> (TAN :: buffer)
    | 'n' :: 'a' :: 't' :: c :: tl when sep c ->
      main_parser (TAN :: buffer) (c :: tl)
    (* abs *)
    | 's' :: 'b' :: 'a' :: [] -> (ABS :: buffer)
    | 's' :: 'b' :: 'a' :: c :: tl when sep c ->
      main_parser (ABS :: buffer) (c :: tl)
    (* sqrt *)
    | 't' :: 'r' :: 'q' :: 's' :: [] -> (SQRT :: buffer)
    | 't' :: 'r' :: 'q' :: 's' :: c :: tl when sep c ->
      main_parser (SQRT :: buffer) (c :: tl)
    (* log *)
    | 'g' :: 'o' :: 'l' :: [] -> (LOG :: buffer)
    | 'g' :: 'o' :: 'l' :: c :: tl when sep c ->
      main_parser (LOG :: buffer) (c :: tl)
    (* ln *)
    | 'n' :: 'l' :: [] -> (LN :: buffer)
    | 'n' :: 'l' :: c :: tl when sep c ->
      main_parser (LN :: buffer) (c :: tl)
    (* exp *)
    | 'p' :: 'x' :: 'e' :: [] -> (EXP :: buffer)
    | 'p' :: 'x' :: 'e' :: c :: tl when sep c ->
      main_parser (EXP :: buffer) (c :: tl)
    (* free *)
    | 'e' :: 'e' :: 'r' :: 'f' :: [] -> (FREE :: buffer)
    | 'e' :: 'e' :: 'r' :: 'f' :: c :: tl when sep c ->
      main_parser (FREE :: buffer) (c :: tl)
    | '\'' :: tl ->
      let tok, stack = char_token tl in
      main_parser (tok :: buffer) stack
    | '.' :: c :: stack when c >= '0' && c <= '9' ->
      let tok, stack = num_token "" ('.' :: c :: stack) in
      main_parser (tok :: buffer) stack
    | c :: stack when c >= '0' && c <= '9' ->
      let tok, stack = num_token "" (c :: stack) in
      main_parser (tok :: buffer) stack
    | ('a'..'z' as c) :: stack
    | ('A'..'Z' as c) :: stack ->
      let tok, stack = name_token "" (c :: stack) in
      main_parser (tok :: buffer) stack
    | _ :: stack ->
      main_parser (ERROR :: buffer) stack
    | [] -> 
      buffer
  ) in
  main_parser [] stack
)

let char_stack (f : in_channel) = (
  let rec loop stack = (
    match input_char f with
    | c -> loop (c :: stack)
    | exception End_of_file -> close_in f; stack
  ) in
  loop []
)

let interpreter (tokens : token list) = (
  let rec iterate (input : token list)
                  (vars : (string * token) list)
                  (funs : (string * (int * string list * token list)) list) = (
    let rec rpn (input : token list) (stack : token list) (output : token list) = (
      let rec precedence (op : token) (stack : token list) (output : token list) = (
        let higher_order (op : token) (stack_op : token) = (
          let get_lvl (op : token) = (
            match op with
            | FUN _ | POSTFIX_DECR | POSTFIX_INCR -> 0
            | LEN | RAND | UNOP_MINUS | OPENIN | CLOSE | READ
            | NOT | FLOOR | CEIL | TONUM | OPENOUT | CATCH | SQRT
            | POW | PREFIX_DECR | PREFIX_INCR | B_NOT | ABS
            | TOSTR | TOCHAR | LOG | LN | ASIN | ACOS | ATAN
            | SINH | COSH | TANH | SIN | COS | TAN | EXP -> 1
            | MULT | DIV | MOD -> 2
            | PLUS | MINUS -> 3
            | B_SHIFTL | B_ASHIFTR | B_LSHIFTR -> 4
            | LESS | LEQ | GREATER | GEQ -> 5
            | NEQ | EQ -> 6
            | B_AND -> 7
            | B_XOR -> 8
            | B_OR -> 9
            | AND -> 10
            | OR -> 11
            | LET | PRINT | PRINTLN | RETURN
            | SCAN | WRITE | THROW | FREE -> 12
            | tok -> raise (InvalidToken (tok, "at higher_order"))
          ) in
          let op, stack_op = get_lvl op, get_lvl stack_op in
          op < stack_op
        ) in
        match stack with
        | [] ->
          [op], output
        | stack_op :: tl -> (
          match op, stack_op with
          (* Special cases with parentheses and brackets *)
          | BR_END, BR_START ->
            tl, (DREF :: output)
          | END, START ->
            tl, output
          | BR_END, _
          | END, _ ->
            precedence op tl (stack_op :: output)
          | BR_START, _
          | START, _ 
          | _, BR_START
          | _, START ->
            (op :: stack), output
          | op, stack_op ->
            if higher_order op stack_op then
              (op :: stack), output
            else
              precedence op tl (stack_op :: output)
        )
      ) in
      match input with
      | NEWLINE :: _
      | SEQ :: _
      | [] -> (
        match stack with
        | [] -> (List.rev output), input
        | hd :: tl -> rpn input tl (hd :: output)
      )
      | NAME n :: tl when List.mem_assoc n funs ->
        rpn tl (FUN n :: stack) output
      | NAME _ :: tl
      | STR _ :: tl
      | INT _ :: tl 
      | FLOAT _ :: tl
      | BOOL _ :: tl 
      | CHAR _ :: tl 
      | ARR :: tl ->
        let elem = List.hd input in
        rpn tl stack (elem :: output)
      | op :: tl ->
        let stack, output = precedence op stack output in
        rpn tl stack output
    ) in
    let strip_if (stack : token list) = (
      let rec loop (stack : token list) (cnt : int) = (
        match stack with
        | IF :: stack -> loop stack (cnt + 1)
        | ELSE :: stack when cnt = 1 -> stack
        | ENDIF :: stack when cnt = 1 -> stack
        | ENDIF :: stack -> loop stack (cnt - 1)
        | _ :: stack -> loop stack cnt
        | [] -> []
      ) in
      loop stack 1
    ) in
    let strip_else (stack : token list) = (
      let rec loop (stack : token list) (buffer : token list) (cnt : int) = (
        if cnt > 0 then
          match stack with
          | IF :: tl -> loop tl buffer (cnt + 1)
          | ENDIF :: tl -> loop tl buffer (cnt - 1)
          | _ :: tl -> loop tl buffer cnt
          | [] -> []
        else
          match buffer with
          | hd :: tl -> loop (hd :: stack) tl cnt
          | [] -> stack
      ) in
      let rec store_if (input : token list) (buffer : token list) (cnt : int) = (
        match input with
        | IF :: tl -> store_if tl (IF :: buffer) (cnt + 1)
        | ELSE :: tl when cnt = 1 -> loop tl buffer 1
        | ENDIF :: tl when cnt = 1 -> stack
        | ENDIF :: tl -> store_if tl (ENDIF :: buffer) (cnt - 1)
        | hd :: tl -> store_if tl (hd :: buffer) cnt
        | [] -> []
      ) in
      store_if stack [] 1
    ) in
    let copy_while (stack : token list) = (
      let rec strip_while (stack : token list) (buffer : token list) (cnt : int) = (
        match stack with
        | WHILE :: tl -> strip_while tl (WHILE :: buffer) (cnt + 1)
        | ENDWHILE :: tl when cnt = 1 -> List.rev buffer, tl
        | ENDWHILE :: tl -> strip_while tl (ENDWHILE :: buffer) (cnt - 1)
        | hd :: tl -> strip_while tl (hd :: buffer) cnt
        | [] -> raise (InvalidToken (LIST [], "at copy_while"))
      ) in
      strip_while stack [] 1
    ) in
    let copy_for (stack : token list) = (
      let rec strip_for (stack : token list) (buffer : token list) (cnt : int) = (
        match stack with
        | FOR :: tl -> strip_for tl (FOR :: buffer) (cnt + 1)
        | ENDFOR :: tl when cnt = 1 -> List.rev buffer, tl
        | ENDFOR :: tl -> strip_for tl (ENDFOR :: buffer) (cnt - 1)
        | hd :: tl -> strip_for tl (hd :: buffer) cnt
        | [] -> raise (InvalidToken (LIST [], "at copy_for"))
      ) in
      strip_for stack [] 1
    ) in
    let store_fun (input : token list) = (
      let rec parse_args (input : token list) (output : string list) (counter : int) = (
        match input with
        | NEWLINE :: input
        | SEQ :: input ->
          input, counter, output
        | NAME n :: input ->
          parse_args input (n :: output) (counter + 1)
        | _ -> raise (InvalidFunction "at parse_args")
      ) in 
      let rec skip_fun (input : token list) (output : token list) (counter : int) = (
        match input with
        | FUN "_DEF_" :: tl ->
          skip_fun tl (List.hd input :: output) (counter + 1)
        | ENDFUN :: tl when counter = 1 ->
          tl, List.rev output
        | ENDFUN :: tl ->
          skip_fun tl (List.hd input :: output) (counter - 1)
        | hd :: tl ->
          skip_fun tl (hd :: output) counter
        | [] ->
          raise (InvalidFunction "at skip_fun")
      ) in
      let (input, argc, argv : token list * int * string list) = parse_args input [] 0 in
      let (output, sequence : token list * token list) = skip_fun input [] 1 in
      (argc, argv, sequence), output
    ) in
    let rec eval_rpn (input : token list) (vars : (string * token) list) (stack : token list) = (
      let dref (stack : token list) (n : int) = (
        let rec loop (stack : token list) (buffer : token list) (index : int) = (
          if index < n then
            match stack with
            | INT i :: ARRAY a :: stack ->
              let elem = a.(i) in
              loop stack (elem :: buffer) (index + 1)
            | NAME a :: stack ->
              let elem = try List.assoc a vars
              with Not_found -> raise (InvalidToken (NAME a, "at dref")) in
              loop stack (elem :: buffer) (index + 1)
            | hd :: tl ->
              loop tl (hd :: buffer) (index + 1)
            | stack -> raise (InvalidToken (LIST stack, "at dref"))
          else
            match buffer with
            | hd :: tl ->
              loop (hd :: stack) tl index
            | [] -> stack
        ) in
        loop stack [] 0
      ) in
      match input with
      | NAME _ :: tl
      | STR _ :: tl
      | INT _ :: tl 
      | FLOAT _ :: tl
      | BOOL _ :: tl 
      | CHAR _ :: tl 
      | ARR :: tl ->
        eval_rpn tl vars ((List.hd input) :: stack)
      | op :: input -> (
        match op with
        | LET -> (
          let stack = dref stack 1 in
          match stack with
          | v :: NAME n :: stack ->
            eval_rpn input ((n, v) :: (List.remove_assoc n vars)) (v :: stack)
          | v :: INT i :: ARRAY a :: stack ->
            a.(i) <- v; eval_rpn input vars (v :: stack)
          | INT size :: ARR :: NAME n :: stack ->
            let arr = ARRAY (Array.make size (INT 0)) in
            eval_rpn input ((n, arr) :: (List.remove_assoc n vars)) (arr :: stack)
          | INT size :: ARR :: INT i :: ARRAY a :: stack ->
            let arr = ARRAY (Array.make size (INT 0)) in
            a.(i) <- arr; eval_rpn input vars (arr :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LET"))
        )
        | SCAN -> (
          match stack with
          | NAME n :: NAME t :: stack -> (
            match t with
            | "int" ->
              let v = read_int () in
              eval_rpn input ((n, INT v) :: (List.remove_assoc n vars)) stack
            | "str" ->
              let v = read_line () in
              eval_rpn input ((n, STR v) :: (List.remove_assoc n vars)) stack
            | "float" ->
              let v = Scanf.scanf "%f" (fun v -> v) in
              eval_rpn input ((n, FLOAT v) :: (List.remove_assoc n vars)) stack
            | _ -> raise (InvalidToken (NAME t, "at SCAN"))
          )
          | INT i :: ARRAY a :: NAME t :: stack -> (
            match t with
            | "int" ->
              let v = read_int () in
              a.(i) <- INT v; eval_rpn input vars stack
            | "str" ->
              let v = read_line () in
              a.(i) <- STR v; eval_rpn input vars stack
            | "float" ->
              let v = Scanf.scanf "%f" (fun v -> v) in
              a.(i) <- FLOAT v; eval_rpn input vars stack
            | _ -> raise (InvalidToken (NAME t, "at SCAN"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at SCAN"))
        )
        | PREFIX_INCR -> (
          match stack with
          | INT i :: ARRAY a :: stack -> (
            let v = a.(i) in
            match v with
            | INT j ->
              let v = INT (j + 1) in
              a.(i) <- v;
              eval_rpn input vars (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_INCR (array)"))
          )
          | NAME n :: stack -> (
            let v = try List.assoc n vars
            with Not_found -> raise (InvalidToken (NAME n, "at PREFIX_INCR")) in
            match v with
            | INT i ->
              let v = INT (i + 1) in
              eval_rpn input ((n, v) :: (List.remove_assoc n vars)) (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_INCR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at PREFIX_INCR"))
        )
        | PREFIX_DECR -> (
          match stack with
          | INT i :: ARRAY a :: stack -> (
            let v = a.(i) in
              match v with
              | INT j ->
                let v = INT (j - 1) in
                a.(i) <- v; eval_rpn input vars (v :: stack)
              | tok -> raise (InvalidToken (tok, "at PREFIX_DECR (array)"))
          )
          | NAME n :: stack -> (
            let v = try List.assoc n vars
            with Not_found -> raise (InvalidToken (NAME n, "at PREFIX_DECR")) in
            match v with
            | INT i ->
              let v = INT (i - 1) in
              eval_rpn input ((n, v) :: (List.remove_assoc n vars)) (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_DECR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at PREFIX_DECR"))
        )
        | POSTFIX_INCR -> (
          match stack with
          | INT i :: ARRAY a :: stack -> (
            let v = a.(i) in
              match v with
              | INT j ->
                let v = INT (j + 1) in
                a.(i) <- v; eval_rpn input vars (INT j :: stack)
              | tok -> raise (InvalidToken (tok, "at POSTFIX_INCR (array)"))
          )
          | NAME n :: stack -> (
            let v = try List.assoc n vars
            with Not_found -> raise (InvalidToken (NAME n, "at POSTFIX_INCR")) in
            match v with
            | INT i ->
              let v = INT (i + 1) in
              eval_rpn input ((n, v) :: (List.remove_assoc n vars)) (INT i :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_INCR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at POSTFIX_INCR"))
        )
        | POSTFIX_DECR -> (
          match stack with
          | INT i :: ARRAY a :: stack -> (
            let v = a.(i) in
              match v with
              | INT j ->
                let v = INT (j - 1) in
                a.(i) <- v; eval_rpn input vars (INT j :: stack)
              | tok -> raise (InvalidToken (tok, "at POSTFIX_DECR (array)"))
          )
          | NAME n :: stack -> (
            let v = try List.assoc n vars
            with Not_found -> raise (InvalidToken (NAME n, "at POSTFIX_DECR")) in
            match v with
            | INT i ->
              let v = INT (i - 1) in
              eval_rpn input ((n, v) :: (List.remove_assoc n vars)) (INT i :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_DECR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at POSTFIX_DECR"))
        )
        | DREF -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: INT j :: ARRAY a :: stack -> (
            let elem = a.(j) in
            match elem with
            | ARRAY _ ->
              eval_rpn input vars (INT i :: elem :: stack)
            | STR s ->
              eval_rpn input vars (CHAR s.[i] :: stack)
            | tok -> raise (InvalidToken (tok, "at DREF"))
          )
          | INT i :: NAME n :: stack -> (
            let v = try List.assoc n vars
            with Not_found -> raise (InvalidToken (NAME n, "at DREF")) in
            match v with
            | ARRAY a ->
              eval_rpn input vars (INT i :: ARRAY a :: stack)
            | STR s ->
              eval_rpn input vars (CHAR s.[i] :: stack)
            | tok -> raise (InvalidToken (tok, "at DREF"))
          )
          | INT i :: STR s :: stack ->
            eval_rpn input vars (CHAR s.[i] :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at DREF"))
        )
        | FUN n -> (
          let argc, args, sequence = try List.assoc n funs 
          with Not_found -> raise (InvalidToken (FUN n, "at FUN")) in
          let stack = dref stack argc in
          let rec assign (args : string list) (stack : token list) (vars : (string * token) list) = (
            match args, stack with
            | n :: tl, v :: stack ->
              assign tl stack ((n, v) :: vars)
            | [], _ -> vars, stack
            | _ -> raise (InvalidFunction "at assign")
          ) in
          let fn_vars, stack = assign args stack [] in
          let res_vars = iterate sequence fn_vars funs in
          if List.mem_assoc "_RETURN_" res_vars then
            eval_rpn input vars ((List.assoc "_RETURN_" res_vars) :: stack)
          else
            eval_rpn input vars stack
        )
        | PRINT -> (
          let stack = dref stack 1 in
          match stack with
          | STR a :: stack ->
            print_string a; eval_rpn input vars stack
          | INT a :: stack ->
            print_int a; eval_rpn input vars stack
          | FLOAT a :: stack ->
            print_float a; eval_rpn input vars stack
          | BOOL a :: stack ->
            if a then
              print_string "true"
            else
              print_string "false";
            eval_rpn input vars stack
          | CHAR a :: stack ->
            print_char a; eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at PRINT"))
        )
        | PRINTLN -> (
          let stack = dref stack 1 in
          match stack with
          | STR a :: stack ->
            print_endline a; eval_rpn input vars stack
          | INT a :: stack ->
            print_int a; print_newline ();
            eval_rpn input vars stack
          | FLOAT a :: stack ->
            print_float a; print_newline ();
            eval_rpn input vars stack
          | BOOL a :: stack ->
            if a then
              print_endline "true"
            else
              print_endline "false";
            eval_rpn input vars stack
          | CHAR a :: stack ->
            print_char a; print_newline ();
            eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at PRINTLN"))
        )
        | PLUS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a + b)) :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (a +. b) :: stack))
          | FLOAT f :: INT i :: stack
          | INT i :: FLOAT f :: stack ->
            let i = float_of_int i in
            eval_rpn input vars ((FLOAT (i +. f) :: stack))
          | STR b :: STR a :: stack ->
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | CHAR b :: CHAR a :: stack ->
            let c = Char.code a + Char.code b in
            eval_rpn input vars (INT c :: stack)
          | STR b :: INT a :: stack ->
            let a = string_of_int a in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | INT b :: STR a :: stack ->
            let b = string_of_int b in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | STR b :: FLOAT a :: stack ->
            let a = string_of_float a in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | FLOAT b :: STR a :: stack ->
            let b = string_of_float b in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | STR b :: CHAR a :: stack ->
            let a = String.make 1 a in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | CHAR b :: STR a :: stack ->
            let b = String.make 1 b in
            eval_rpn input vars ((STR (a ^ b)) :: stack)
          | INT i :: CHAR c :: stack
          | CHAR c :: INT i :: stack ->
            let c = i + Char.code c in
            eval_rpn input vars (INT c :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at PLUS"))
        )
        | MINUS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a - b)) :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (a -. b) :: stack))
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars ((FLOAT (a -. b) :: stack))
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars ((FLOAT (a -. b) :: stack))
          | CHAR b :: INT a :: stack ->
            let b = Char.code b in
            eval_rpn input vars ((INT (a - b)) :: stack)
          | INT b :: CHAR a :: stack ->
            let a = Char.code a in
            eval_rpn input vars ((INT (a - b)) :: stack)
          | CHAR b :: CHAR a :: stack ->
            let c = Char.code a - Char.code b in
            eval_rpn input vars (INT c :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at MINUS"))
        )
        | UNOP_MINUS -> (
          let stack = dref stack 1 in
          match stack with
          | INT a :: stack ->
            eval_rpn input vars ((INT (-a)) :: stack)
          | FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (-.a) :: stack))
          | stack -> raise (InvalidToken (LIST stack, "at UNOP_MINUS"))
        )
        | MULT -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a * b)) :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (a *. b) :: stack))
          | FLOAT f :: INT i :: stack
          | INT i :: FLOAT f :: stack ->
            let i = float_of_int i in
            eval_rpn input vars ((FLOAT (i *. f) :: stack))
          | stack -> raise (InvalidToken (LIST stack, "at MULT"))
        )
        | DIV -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a / b)) :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (a /. b) :: stack))
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars ((FLOAT (a /. b) :: stack))
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars ((FLOAT (a /. b) :: stack))
          | stack -> raise (InvalidToken (LIST stack, "at DIV"))
        )
        | MOD -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a mod b)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at MOD"))
        )
        | LESS -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a < b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars (BOOL (a < b) :: stack)
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars (BOOL (a < b) :: stack)
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars (BOOL (a < b) :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a < b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LESS"))
        )
        | GREATER -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a > b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars (BOOL (a > b) :: stack)
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars (BOOL (a > b) :: stack)
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars (BOOL (a > b) :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a > b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at GREATER"))
        )
        | LEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a <= b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars (BOOL (a <= b) :: stack)
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars (BOOL (a <= b) :: stack)
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars (BOOL (a <= b) :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a <= b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LEQ"))
        )
        | GEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a >= b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars (BOOL (a >= b) :: stack)
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars (BOOL (a >= b) :: stack)
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars (BOOL (a >= b) :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a >= b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at GEQ"))
        )
        | NEQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a <> b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((BOOL (a <> b) :: stack))
          | FLOAT f :: INT i :: stack
          | INT i :: FLOAT f :: stack ->
            let i = float_of_int i in
            eval_rpn input vars ((BOOL (i <> f) :: stack))
          | STR b :: STR a :: stack ->
            let v = BOOL (a <> b) in
            eval_rpn input vars (v :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a <> b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at NEQ"))
        )
        | EQ -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let v = BOOL (a = b) in
            eval_rpn input vars (v :: stack)
          | FLOAT b :: FLOAT a :: stack ->
            eval_rpn input vars ((BOOL (a = b) :: stack))
          | FLOAT f :: INT i :: stack
          | INT i :: FLOAT f :: stack ->
            let i = float_of_int i in
            eval_rpn input vars ((BOOL (i = f) :: stack))
          | STR b :: STR a :: stack ->
            let v = BOOL (a = b) in
            eval_rpn input vars (v :: stack)
          | CHAR b :: CHAR a :: stack ->
            eval_rpn input vars (BOOL (a = b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at EQ"))
        )
        | AND -> (
          let stack = dref stack 2 in
          match stack with
          | BOOL b :: BOOL a :: stack ->
            let v = BOOL (a && b) in
            eval_rpn input vars (v :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at AND"))
        )
        | OR -> (
          let stack = dref stack 2 in
          match stack with
          | BOOL b :: BOOL a :: stack ->
            let v = BOOL (a || b) in
            eval_rpn input vars (v :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OR"))
        )
        | NOT -> (
          let stack = dref stack 1 in
          match stack with
          | BOOL a :: stack ->
            let v = BOOL (not a) in
            eval_rpn input vars (v :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at NOT"))
        )
        | POW -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            let b = float_of_int b in
            let a = float_of_int a in
            eval_rpn input vars ((FLOAT (a ** b)) :: stack)
          | FLOAT b :: INT a :: stack ->
            let a = float_of_int a in
            eval_rpn input vars ((FLOAT (a ** b)) :: stack)
          | INT b :: FLOAT a :: stack ->
            let b = float_of_int b in
            eval_rpn input vars ((FLOAT (a ** b)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at POW"))
        )
        | LEN -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack ->
            eval_rpn input vars ((INT (String.length s)) :: stack)
          | ARRAY a :: stack ->
            eval_rpn input vars ((INT (Array.length a)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LEN"))
        )
        | RAND -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT (Random.int i) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at RAND"))
        )
        | FLOOR -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT i :: stack)
          | FLOAT f :: stack ->
            eval_rpn input vars (INT (int_of_float (floor f)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at FLOOR"))
        )
        | CEIL -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT i :: stack)
          | FLOAT f :: stack ->
            eval_rpn input vars (INT (int_of_float (ceil f)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at CEIL"))
        )
        | TONUM -> (
          let stack = dref stack 1 in
          match stack with
          | INT _ :: _ | FLOAT _ :: _ ->
            eval_rpn input vars stack
          | CHAR c :: stack ->
            eval_rpn input vars (INT (Char.code c) :: stack)
          | STR s :: stack ->
            let num = try INT (int_of_string s)
            with e -> try FLOAT (float_of_string s)
            with e -> raise (InvalidToken (STR s, "at TONUM")) in
            eval_rpn input vars (num :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TONUM"))
        )
        | OPENIN -> (
          let stack = dref stack 1 in
          match stack with
          | STR n :: stack ->
          let file = try open_in n
          with e -> raise (InvalidFilename n) in
          eval_rpn input vars (INCHAN file :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OPENIN"))
        )
        | OPENOUT -> (
          let stack = dref stack 1 in
          match stack with
          | STR n :: stack ->
          let file = try open_out n
          with e -> raise (InvalidFilename n) in
          eval_rpn input vars (OUTCHAN file :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OPENOUT"))
        )
        | CLOSE -> (
          let stack = dref stack 1 in
          match stack with
          | INCHAN c :: stack ->
            close_in c; eval_rpn input vars stack
          | OUTCHAN c :: stack ->
            close_out c; eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at CLOSE"))
        )
        | CATCH -> (
          let stack = dref stack 1 in
          match stack with
          | ERROR :: stack -> eval_rpn input vars (BOOL true :: stack)
          | _ :: stack -> eval_rpn input vars (BOOL false :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at CATCH"))
        )
        | READ -> (
          let stack = dref stack 1 in
          match stack with
          | INCHAN c :: NAME t :: stack -> (
            match t with
            | "str" -> (
              match input_line c with
              | s -> (eval_rpn [@tailcall]) input vars (STR s :: stack)
              | exception e -> (eval_rpn [@tailcall]) input vars (ERROR :: stack)
            )
            | "char" -> (
              match input_char c with
              | c -> (eval_rpn [@tailcall]) input vars (CHAR c :: stack)
              | exception e -> (eval_rpn [@tailcall]) input vars (ERROR :: stack)
            )
            | _ -> raise (InvalidToken (NAME t, "at READ"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at READ"))
        )
        | WRITE -> (
          let stack = dref stack 2 in
          match stack with
          | OUTCHAN c :: STR s :: stack ->
            output_string c s; eval_rpn input vars stack
          | OUTCHAN c :: CHAR ch :: stack ->
            output_char c ch; eval_rpn input vars stack
          | OUTCHAN c :: INT i :: stack ->
            output_string c (string_of_int i); eval_rpn input vars stack
          | OUTCHAN c :: FLOAT f :: stack ->
            output_string c (string_of_float f); eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at WRITE"))
        )
        | THROW -> (
          raise (InvalidToken (LIST stack, "at THROW"))
        )
        | TOCHAR -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (CHAR (Char.chr i) :: stack)
          | STR s :: stack ->
            eval_rpn input vars (CHAR (s.[0]) :: stack)
          | CHAR _ :: _ ->
            eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at TOCHAR"))
        )
        | TOSTR -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (STR (string_of_int i) :: stack)
          | FLOAT f :: stack ->
            eval_rpn input vars (STR (string_of_float f) :: stack)
          | CHAR c :: stack ->
            eval_rpn input vars (STR (String.make 1 c) :: stack)
          | STR _ :: _ ->
            eval_rpn input vars stack
          | BOOL b :: stack ->
            let s = if b then "true" else "false" in
            eval_rpn input vars (STR s :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TOSTR"))
        )
        | B_AND -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a land b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_AND"))
        )
        | B_OR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lor b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_OR"))
        )
        | B_XOR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lxor b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_XOR"))
        )
        | B_SHIFTL -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lsl b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_SHIFTL"))
        )
        | B_LSHIFTR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a asr b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_LSHIFTR"))
        )
        | B_ASHIFTR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lsr b) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_ASHIFTR"))
        )
        | B_NOT -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT (lnot i) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_NOT"))
        )
        | SINH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sinh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sinh (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SINH"))
        )
        | ASIN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (asin f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (asin (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ASIN"))
        )
        | SIN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sin f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sin (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SIN"))
        )
        | COSH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (cosh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (cosh (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at COSH"))
        )
        | ACOS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (acos f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (acos (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ACOS"))
        )
        | COS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (cos f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (cos (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at COS"))
        )
        | TANH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (tanh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (tanh (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TANH"))
        )
        | ATAN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (atan f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (atan (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ATAN"))
        )
        | TAN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (tan f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (tan (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TAN"))
        )
        | ABS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (abs_float f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (INT (abs i) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ABS"))
        )
        | SQRT -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sqrt f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sqrt (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SQRT"))
        )
        | LOG -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (log10 f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (log10 (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LOG"))
        )
        | LN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (log f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (log (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LN"))
        )
        | EXP -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (exp f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (exp (float_of_int i)) :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at EXP"))
        )
        | FREE -> (
          match stack with
          | NAME n :: stack ->
            eval_rpn input (List.remove_assoc n vars) stack
          | stack -> raise (InvalidToken (LIST stack, "at FREE"))
        )
        | op -> raise (InvalidToken (op, "at eval_rpn"))
      )
      | [] -> vars
    ) in  
    match input with
    | [] -> vars
    | SEQ :: input
    | NEWLINE :: input -> iterate input vars funs
    | INCLUDE :: STR n :: input ->
      let file = try open_in n
      with e -> raise (InvalidFilename n) in
      let tokens = char_stack file |> tokenizer in
      iterate (tokens @ input) vars funs
    | IF :: input -> (
      let expr, input = rpn (LET :: NAME "_EVAL_" :: input) [] [] in
      let vars = eval_rpn expr vars [] in
      let ret = try List.assoc "_EVAL_" vars
      with Not_found -> raise (InvalidToken (NAME "_EVAL_", "at IF")) in
      match ret with
      | BOOL true -> iterate (strip_else input) vars funs
      | BOOL false -> iterate (strip_if input) vars funs
      | eval -> raise (InvalidToken (eval, "at IF"))
    )
    | RETURN :: input -> (
      let expr, _ = rpn (LET :: NAME "_RETURN_" :: input) [] [] in
      let vars = eval_rpn expr vars [] in
      vars
    )
    | BREAK :: input -> (
      (("_BREAK_", BREAK) :: (List.remove_assoc "_BREAK_" vars))
    )
    | WHILE :: tl -> (
      let cond, stack = rpn (LET :: NAME "_EVAL_" :: tl) [] [] in
      let copy, stack = copy_while stack in
      let rec loop (vars : (string * token) list) = (
        let vars = eval_rpn cond vars [] in
        let ret = try List.assoc "_EVAL_" vars
        with Not_found -> raise (InvalidToken (NAME "_EVAL_", "at WHILE")) in
        match ret, List.mem_assoc "_BREAK_" vars with
        | BOOL true, false ->
          let vars = iterate copy vars funs in
          if List.mem_assoc "_RETURN_" vars then
            vars
          else
            loop vars
        | _, true -> iterate stack (List.remove_assoc "_BREAK_" vars) funs
        | BOOL false, _ -> iterate stack vars funs
        | eval, _ -> raise (InvalidToken (eval, "at WHILE"))
      ) in
      loop vars
    )
    | FOR :: tl -> (
      let expr, stack = rpn tl [] [] in
      let vars = eval_rpn expr vars [] in
      let cond, stack = rpn (LET :: NAME "_EVAL_" :: (List.tl stack)) [] [] in
      let term, stack = rpn (List.tl stack) [] [] in
      let copy, stack = copy_for stack in
      let rec loop (vars : (string * token) list) = (
        let vars = eval_rpn cond vars [] in
        let ret = try List.assoc "_EVAL_" vars
        with Not_found -> raise (InvalidToken (NAME "_EVAL_", "at FOR")) in
        match ret, List.mem_assoc "_BREAK_" vars with
        | BOOL true, false ->
          let vars = iterate copy vars funs in
          if List.mem_assoc "_RETURN_" vars then
            vars
          else
            loop (eval_rpn term vars [])
        | _, true -> iterate stack (List.remove_assoc "_BREAK_" vars) funs
        | BOOL false, _ -> iterate stack vars funs
        | eval, _ -> raise (InvalidToken (eval, "at FOR"))
      ) in
      loop vars
    )
    | ENDWHILE :: input
    | ENDIF :: input ->
      iterate input (List.remove_assoc "_EVAL_" vars) funs
    | FUN "_DEF_" :: NAME n :: input ->
      let new_fun, input = store_fun input in
      iterate input vars ((n, new_fun) :: (List.remove_assoc n funs))
    | _ -> (
      let expr, input = rpn input [] [] in
      let vars = eval_rpn expr vars [] in
      iterate input vars funs
    )
  ) in
  Random.self_init ();
  iterate tokens [] []
)

let () = (
  if Array.length Sys.argv > 1 then (
    let file = 
      try open_in Sys.argv.(1)
      with e -> (
        Printf.fprintf stderr "Cannot find file %s.\n" Sys.argv.(1);
        exit 0
      ) in
    let tokens = file |> char_stack |> tokenizer in
    (* print_endline (string_of_token (LIST tokens)); *)
    match interpreter tokens with
    | exception InvalidToken (tok, s) ->
      Printf.fprintf stderr "Encountered an exception InvalidToken %s!\nToken: %s\n" s (string_of_token tok)
    | exception InvalidFilename s ->
      Printf.fprintf stderr "Encountered an exception InvalidFilename %s!\n" s
    | exception InvalidFunction s ->
      Printf.fprintf stderr "Encountered an exception InvalidFunction %s!\n" s
    | exception e -> raise e
    | _ -> ()
  )
)
