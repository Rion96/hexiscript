module StrMap = Map.Make (String)

type token =
  | NAME of string | CD | CWD | ENV | OUTOFBOUNDS of int
  | FUN of string | ENDFUN | CONTINUE | RUN | DIR | ISDIR
  | INT of int | TONUM | DICT | KEYS | ELIF | IS
  | FLOAT of float | DICTIONARY of (string, token) Hashtbl.t
  | BOOL of bool | SCAN_ERR | TONUM_ERR of string
  | CHAR of char | TOCHAR | NOT_FOUND of string | EXISTS
  | STR of string | TOSTR | UNDEFINED of string | EOF
  | INCHAN of in_channel | OPENIN | CLOSE | CATCH | THROW
  | OUTCHAN of out_channel | OPENOUT | READ | WRITE
  | ARRAY of token array | ARR | DREF | FLOOR | CEIL
  | LET | GLET | PRINT | PRINTLN | POSTFIX_INCR | POSTFIX_DECR
  | IF | ELSE | ENDIF | WHILE | ENDWHILE | FOR | ENDFOR
  | NEWLINE | ERROR of string | LIST of token list | SEQ | RETURN
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
  | GLET          -> "GLET"
  | ENDFUN        -> "ENDFUN"
  | INT i         -> ("INT " ^ string_of_int i)
  | TONUM         -> "TONUM"
  | FLOAT f       -> ("FLOAT " ^ string_of_float f)
  | BOOL b        -> ("BOOL " ^ string_of_bool b)
  | CHAR c        -> ("CHAR " ^ String.make 1 c)
  | TOCHAR        -> "TOCHAR"
  | STR s         -> ("STRING " ^ s)
  | TOSTR         -> "TOSTR"
  | INCHAN _      -> "INCHAN"
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
  | ERROR s       -> ("ERROR " ^ s)
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
  | UNDEFINED s   -> ("UNDEFINED " ^ s)
  | NOT_FOUND s   -> ("NOT_FOUND " ^ s)
  | EOF           -> "EOF"
  | EXISTS        -> "EXISTS"
  | SCAN_ERR      -> "SCAN_ERR"
  | TONUM_ERR s   -> ("TONUM_ERR" ^ s)
  | DICTIONARY _  -> "DICTIONARY"
  | DICT          -> "DICT"
  | KEYS          -> "KEYS"
  | ELIF          -> "ELIF"
  | CONTINUE      -> "CONTINUE"
  | CD            -> "CD"
  | CWD           -> "CWD"
  | ENV           -> "ENV"
  | RUN           -> "RUN"
  | DIR           -> "DIR"
  | ISDIR         -> "ISDIR"
  | IS            -> "IS"
  | OUTOFBOUNDS i -> ("OUTOFBOUNDS " ^ (string_of_int i))
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
        with exn -> ERROR ("at num_token: " ^ buffer ^ " is NaN!"), stack
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
    | stack -> ERROR "at char_token", stack
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
    | '\n' :: tl | '\r' :: tl
    | '\t' :: tl | '\b' :: tl -> str_token buffer tl
    | '"' :: stack -> STR buffer, stack
    | hd :: tl -> str_token ((String.make 1 hd) ^ buffer) tl
    | [] -> ERROR "at str_token", []
  ) in
  let rec name_token (buffer : string) (input : char list) = (
    match input with
    | ('0'..'9' as d) :: c :: _ when sep c ->
      let buffer = String.make 1 d in
      ERROR ("at name_token: " ^ buffer ^ " cannot start with number!"), input
    | c :: _ when sep c -> NAME buffer, input
    | 'x' :: '0' :: tl ->
      num_token ("0x" ^ buffer) tl
    | '_' :: c :: tl when c >= 'A' && c <= 'Z'
                       || c >= 'a' && c <= 'z'
                       || c >= '0' && c <= '9' ->
      name_token ((String.make 1 c) ^ "_" ^ buffer) tl
    | hd :: tl when hd >= 'A' && hd <= 'Z'
                 || hd >= 'a' && hd <= 'z'
                 || hd >= '0' && hd <= '9' ->
      name_token ((String.make 1 hd) ^ buffer) tl
    | stack -> ERROR ("at name_token: " ^ buffer), stack
  ) in
  let rec iterate (buffer : token list) = (
    function
    | ';' :: stack ->
      iterate (SEQ :: buffer) stack
    | '\n' :: stack ->
      iterate (NEWLINE :: buffer) stack
    | ']' :: stack ->
      iterate (BR_END :: buffer) stack
    | '[' :: stack ->
      iterate (BR_START :: buffer) stack
    | ')' :: stack ->
      iterate (END :: buffer) stack
    | '+' :: '+' :: [] -> (PREFIX_INCR :: buffer)
    | '+' :: '+' :: c :: tl when sep c ->
      iterate (PREFIX_INCR :: buffer) (c :: tl)
    | '+' :: '+' :: c :: tl when postfix c ->
      iterate (POSTFIX_INCR :: buffer) (c :: tl)
    | '(' :: stack
    | '+' :: '(' :: stack ->
      iterate (START :: buffer) stack
    | '-' :: '-' :: [] -> (PREFIX_DECR :: buffer)
    | '-' :: '-' :: c :: tl when sep c ->
      iterate (PREFIX_DECR :: buffer) (c :: tl)
    | '-' :: '-' :: c :: tl when postfix c ->
      iterate (POSTFIX_DECR :: buffer) (c :: tl)
    | '-' :: '(' :: stack ->
      iterate (START :: UNOP_MINUS :: buffer) stack
    | '-' :: stack ->
      iterate (MINUS :: buffer) stack
    | '+' :: stack ->
      iterate (PLUS :: buffer) stack
    | '*' :: stack ->
      iterate (MULT :: buffer) stack
    | '/' :: stack ->
      iterate (DIV :: buffer) stack
    | '%' :: stack ->
      iterate (MOD :: buffer) stack
    | '^' :: '^' :: stack ->
      iterate (POW :: buffer) stack
    | '^' :: stack ->
      iterate (B_XOR :: buffer) stack
    | '!' :: stack ->
      iterate (NOT :: buffer) stack
    | '~' :: stack ->
      iterate (B_NOT :: buffer) stack
    | '|' :: '|' :: stack ->
      iterate (OR :: buffer) stack
    | '|' :: stack ->
      iterate (B_OR :: buffer) stack
    | '&' :: '&' :: stack ->
      iterate (AND :: buffer) stack
    | '&' :: stack ->
      iterate (B_AND :: buffer) stack
    | '=' :: '>' :: stack ->
      iterate (GEQ :: buffer) stack
    | '=' :: '<' :: stack ->
      iterate (LEQ :: buffer) stack
    | '=' :: '!' :: stack ->
      iterate (NEQ :: buffer) stack
    | '=' :: stack ->
      iterate (EQ :: buffer) stack
    | '<' :: '<' :: stack ->
      iterate (B_SHIFTL :: buffer) stack
    | '<' :: stack ->
      iterate (LESS :: buffer) stack
    | '>' :: '>' :: '>' :: stack ->
      iterate (B_ASHIFTR :: buffer) stack
    | '>' :: '>' :: stack ->
      iterate (B_LSHIFTR :: buffer) stack
    | '>' :: stack ->
      iterate (GREATER :: buffer) stack
    | '#' :: stack ->
      iterate (ignore_token buffer) stack
    | '\t' :: stack
    | ' ' :: stack ->
      iterate buffer stack
    | '"' :: stack ->
      let tok, stack = str_token "" stack in
      iterate (tok :: buffer) stack
    (* println *)
    | 'n' :: 'l' :: 't' :: 'n' :: 'i' :: 'r' :: 'p' :: [] -> (PRINTLN :: buffer)
    | 'n' :: 'l' :: 't' :: 'n' :: 'i' :: 'r' :: 'p' :: c :: tl when sep c ->
      iterate (PRINTLN :: buffer) (c :: tl)
    (* print *)
    | 't' :: 'n' :: 'i' :: 'r' :: 'p' :: [] -> (PRINT :: buffer)
    | 't' :: 'n' :: 'i' :: 'r' :: 'p' :: c :: tl when sep c ->
      iterate (PRINT :: buffer) (c :: tl)
    (* let *)
    | 't' :: 'e' :: 'l' :: [] -> (LET :: buffer)
    | 't' :: 'e' :: 'l' :: c :: tl when sep c ->
      iterate (LET :: buffer) (c :: tl)
    (* true *)
    | 'e' :: 'u' :: 'r' :: 't' :: [] -> (BOOL true :: buffer)
    | 'e' :: 'u' :: 'r' :: 't' :: c :: tl when sep c ->
      iterate (BOOL true :: buffer) (c :: tl)
    (* false *)
    | 'e' :: 's' :: 'l' :: 'a' :: 'f' :: [] -> (BOOL false :: buffer)
    | 'e' :: 's' :: 'l' :: 'a' :: 'f' :: c :: tl when sep c ->
      iterate (BOOL false :: buffer) (c :: tl)
    (* elif *)
    | 'f' :: 'i' :: 'l' :: 'e' :: [] -> (ELIF :: buffer)
    | 'f' :: 'i' :: 'l' :: 'e' :: c :: tl when sep c ->
      iterate (ELIF :: buffer) (c :: tl)
    (* endif *)
    | 'f' :: 'i' :: 'd' :: 'n' :: 'e' :: [] -> (ENDIF :: buffer)
    | 'f' :: 'i' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      iterate (ENDIF :: buffer) (c :: tl)
    (* if *)
    | 'f' :: 'i' :: [] -> (IF :: buffer)
    | 'f' :: 'i' :: c :: tl when sep c ->
      iterate (IF :: buffer) (c :: tl)
    (* else *)
    | 'e' :: 's' :: 'l' :: 'e' :: [] -> (ELSE :: buffer)
    | 'e' :: 's' :: 'l' :: 'e' :: c :: tl when sep c ->
      iterate (ELSE :: buffer) (c :: tl)
    (* endwhile *)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: 'd' :: 'n' :: 'e' :: [] -> (ENDWHILE :: buffer)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      iterate (ENDWHILE :: buffer) (c :: tl)
    (* while *)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: [] -> (WHILE :: buffer)
    | 'e' :: 'l' :: 'i' :: 'h' :: 'w' :: c :: tl when sep c ->
      iterate (WHILE :: buffer) (c :: tl) 
    (* endfun *)
    | 'n' :: 'u' :: 'f' :: 'd' :: 'n' :: 'e' :: [] -> (ENDFUN :: buffer)
    | 'n' :: 'u' :: 'f' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      iterate (ENDFUN :: buffer) (c :: tl)
    (* fun *)
    | 'n' :: 'u' :: 'f' :: [] -> (FUN "_DEF_" :: buffer)
    | 'n' :: 'u' :: 'f' :: c :: tl when sep c ->
      iterate (FUN "_DEF_" :: buffer) (c :: tl)
    (* return *)
    | 'n' :: 'r' :: 'u' :: 't' :: 'e' :: 'r' :: [] -> (RETURN :: buffer)
    | 'n' :: 'r' :: 'u' :: 't' :: 'e' :: 'r' :: c :: tl when sep c ->
      iterate (RETURN :: buffer) (c :: tl)
    (* arr *)
    | 'r' :: 'r' :: 'a' :: [] -> (ARR :: buffer)
    | 'r' :: 'r' :: 'a' :: c :: tl when sep c ->
      iterate (ARR :: buffer) (c :: tl)
    (* endfor *)
    | 'r' :: 'o' :: 'f' :: 'd' :: 'n' :: 'e' :: [] -> (ENDFOR :: buffer)
    | 'r' :: 'o' :: 'f' :: 'd' :: 'n' :: 'e' :: c :: tl when sep c ->
      iterate (ENDFOR :: buffer) (c :: tl)
    (* for *)
    | 'r' :: 'o' :: 'f' :: [] -> (FOR :: buffer)
    | 'r' :: 'o' :: 'f' :: c :: tl when sep c ->
      iterate (FOR :: buffer) (c :: tl)
    (* break *)
    | 'k' :: 'a' :: 'e' :: 'r' :: 'b' :: [] -> (BREAK :: buffer)
    | 'k' :: 'a' :: 'e' :: 'r' :: 'b' :: c :: tl when sep c ->
      iterate (BREAK :: buffer) (c :: tl)
    (* scan *)
    | 'n' :: 'a' :: 'c' :: 's' :: [] -> (SCAN :: buffer)
    | 'n' :: 'a' :: 'c' :: 's' :: c :: tl when sep c ->
      iterate (SCAN :: buffer) (c :: tl)
    (* len *)
    | 'n' :: 'e' :: 'l' :: [] -> (LEN :: buffer)
    | 'n' :: 'e' :: 'l' :: c :: tl when sep c ->
      iterate (LEN :: buffer) (c :: tl)
    (* rand *)
    | 'd' :: 'n' :: 'a' :: 'r' :: [] -> (RAND :: buffer)
    | 'd' :: 'n' :: 'a' :: 'r' :: c :: tl when sep c ->
      iterate (RAND :: buffer) (c :: tl)
    (* floor *)
    | 'r' :: 'o' :: 'o' :: 'l' :: 'f' :: [] -> (FLOOR :: buffer)
    | 'r' :: 'o' :: 'o' :: 'l' :: 'f' :: c :: tl when sep c ->
      iterate (FLOOR :: buffer) (c :: tl)
    (* ceil *)
    | 'l' :: 'i' :: 'e' :: 'c' :: [] -> (CEIL :: buffer)
    | 'l' :: 'i' :: 'e' :: 'c' :: c :: tl when sep c ->
      iterate (CEIL :: buffer) (c :: tl)
    (* include *)
    | 'e' :: 'd' :: 'u' :: 'l' :: 'c' :: 'n' :: 'i' :: [] -> (INCLUDE :: buffer)
    | 'e' :: 'd' :: 'u' :: 'l' :: 'c' :: 'n' :: 'i' :: c :: tl when sep c ->
      iterate (INCLUDE :: buffer) (c :: tl)
    (* tonum *)
    | 'm' :: 'u' :: 'n' :: 'o' :: 't' :: [] -> (TONUM :: buffer)
    | 'm' :: 'u' :: 'n' :: 'o' :: 't' :: c :: tl when sep c ->
      iterate (TONUM :: buffer) (c :: tl)
    (* openin *)
    | 'n' :: 'i' :: 'n' :: 'e' :: 'p' :: 'o' :: [] -> (OPENIN :: buffer)
    | 'n' :: 'i' :: 'n' :: 'e' :: 'p' :: 'o' :: c :: tl when sep c ->
      iterate (OPENIN :: buffer) (c :: tl)
    (* openout *)
    | 't' :: 'u' :: 'o' :: 'n' :: 'e' :: 'p' :: 'o' :: [] -> (OPENOUT :: buffer)
    | 't' :: 'u' :: 'o' :: 'n' :: 'e' :: 'p' :: 'o' :: c :: tl when sep c ->
      iterate (OPENOUT :: buffer) (c :: tl)
    (* close *)
    | 'e' :: 's' :: 'o' :: 'l' :: 'c' :: [] -> (CLOSE :: buffer)
    | 'e' :: 's' :: 'o' :: 'l' :: 'c' :: c :: tl when sep c ->
      iterate (CLOSE :: buffer) (c :: tl)
    (* read *)
    | 'd' :: 'a' :: 'e' :: 'r' :: [] -> (READ :: buffer)
    | 'd' :: 'a' :: 'e' :: 'r' :: c :: tl when sep c ->
      iterate (READ :: buffer) (c :: tl)
    (* write *)
    | 'e' :: 't' :: 'i' :: 'r' :: 'w' :: [] -> (WRITE :: buffer)
    | 'e' :: 't' :: 'i' :: 'r' :: 'w' :: c :: tl when sep c ->
      iterate (WRITE :: buffer) (c :: tl)
    (* throw *)
    | 'w' :: 'o' :: 'r' :: 'h' :: 't' :: [] -> (THROW :: buffer)
    | 'w' :: 'o' :: 'r' :: 'h' :: 't' :: c :: tl when sep c ->
      iterate (THROW :: buffer) (c :: tl)
    (* catch *)
    | 'h' :: 'c' :: 't' :: 'a' :: 'c' :: [] -> (CATCH :: buffer)
    | 'h' :: 'c' :: 't' :: 'a' :: 'c' :: c :: tl when sep c ->
      iterate (CATCH :: buffer) (c :: tl)
    (* tochar *)
    | 'r' :: 'a' :: 'h' :: 'c' :: 'o' :: 't' :: [] -> (TOCHAR :: buffer)
    | 'r' :: 'a' :: 'h' :: 'c' :: 'o' :: 't' :: c :: tl when sep c ->
      iterate (TOCHAR :: buffer) (c :: tl)
    (* tostr *)
    | 'r' :: 't' :: 's' :: 'o' :: 't' :: [] -> (TOSTR :: buffer)
    | 'r' :: 't' :: 's' :: 'o' :: 't' :: c :: tl when sep c ->
      iterate (TOSTR :: buffer) (c :: tl)
    (* sinh *)
    | 'h' :: 'n' :: 'i' :: 's' :: [] -> (SINH :: buffer)
    | 'h' :: 'n' :: 'i' :: 's' :: c :: tl when sep c ->
      iterate (SINH :: buffer) (c :: tl)
    (* asin *)
    | 'n' :: 'i' :: 's' :: 'a' :: [] -> (ASIN :: buffer)
    | 'n' :: 'i' :: 's' :: 'a' :: c :: tl when sep c ->
      iterate (ASIN :: buffer) (c :: tl)
    (* sin *)
    | 'n' :: 'i' :: 's' :: [] -> (SIN :: buffer)
    | 'n' :: 'i' :: 's' :: c :: tl when sep c ->
      iterate (SIN :: buffer) (c :: tl)
    (* cosh *)
    | 'h' :: 's' :: 'o' :: 'c' :: [] -> (COSH :: buffer)
    | 'h' :: 's' :: 'o' :: 'c' :: c :: tl when sep c ->
      iterate (COSH :: buffer) (c :: tl)
    (* acos *)
    | 's' :: 'o' :: 'c' :: 'a' :: [] -> (ACOS :: buffer)
    | 's' :: 'o' :: 'c' :: 'a' :: c :: tl when sep c ->
      iterate (ACOS :: buffer) (c :: tl)
    (* cos *)
    | 's' :: 'o' :: 'c' :: [] -> (COS :: buffer)
    | 's' :: 'o' :: 'c' :: c :: tl when sep c ->
      iterate (COS :: buffer) (c :: tl)
    (* tanh *)
    | 'h' :: 'n' :: 'a' :: 't' :: [] -> (TANH :: buffer)
    | 'h' :: 'n' :: 'a' :: 't' :: c :: tl when sep c ->
      iterate (TANH :: buffer) (c :: tl)
    (* atan *)
    | 'n' :: 'a' :: 't' :: 'a' :: [] -> (ATAN :: buffer)
    | 'n' :: 'a' :: 't' :: 'a' :: c :: tl when sep c ->
      iterate (ATAN :: buffer) (c :: tl)
    (* tan *)
    | 'n' :: 'a' :: 't' :: [] -> (TAN :: buffer)
    | 'n' :: 'a' :: 't' :: c :: tl when sep c ->
      iterate (TAN :: buffer) (c :: tl)
    (* abs *)
    | 's' :: 'b' :: 'a' :: [] -> (ABS :: buffer)
    | 's' :: 'b' :: 'a' :: c :: tl when sep c ->
      iterate (ABS :: buffer) (c :: tl)
    (* sqrt *)
    | 't' :: 'r' :: 'q' :: 's' :: [] -> (SQRT :: buffer)
    | 't' :: 'r' :: 'q' :: 's' :: c :: tl when sep c ->
      iterate (SQRT :: buffer) (c :: tl)
    (* log *)
    | 'g' :: 'o' :: 'l' :: [] -> (LOG :: buffer)
    | 'g' :: 'o' :: 'l' :: c :: tl when sep c ->
      iterate (LOG :: buffer) (c :: tl)
    (* ln *)
    | 'n' :: 'l' :: [] -> (LN :: buffer)
    | 'n' :: 'l' :: c :: tl when sep c ->
      iterate (LN :: buffer) (c :: tl)
    (* exp *)
    | 'p' :: 'x' :: 'e' :: [] -> (EXP :: buffer)
    | 'p' :: 'x' :: 'e' :: c :: tl when sep c ->
      iterate (EXP :: buffer) (c :: tl)
    (* free *)
    | 'e' :: 'e' :: 'r' :: 'f' :: [] -> (FREE :: buffer)
    | 'e' :: 'e' :: 'r' :: 'f' :: c :: tl when sep c ->
      iterate (FREE :: buffer) (c :: tl)
    (* exists *)
    | 's' :: 't' :: 's' :: 'i' :: 'x' :: 'e' :: [] -> (EXISTS :: buffer)
    | 's' :: 't' :: 's' :: 'i' :: 'x' :: 'e' :: c :: tl when sep c ->
      iterate (EXISTS :: buffer) (c :: tl)
    (* dict *)
    | 't' :: 'c' :: 'i' :: 'd' :: [] -> (DICT :: buffer)
    | 't' :: 'c' :: 'i' :: 'd' :: c :: tl when sep c ->
      iterate (DICT :: buffer) (c :: tl)
    (* keys *)
    | 's' :: 'y' :: 'e' :: 'k' :: [] -> (KEYS :: buffer)
    | 's' :: 'y' :: 'e' :: 'k' :: c :: tl when sep c ->
      iterate (KEYS :: buffer) (c :: tl)
    (* continue *)
    | 'e' :: 'u' :: 'n' :: 'i' :: 't' :: 'n' :: 'o' :: 'c' :: [] -> (CONTINUE :: buffer)    
    | 'e' :: 'u' :: 'n' :: 'i' :: 't' :: 'n' :: 'o' :: 'c' :: c :: tl when sep c ->
      iterate (CONTINUE :: buffer) (c :: tl)
    (* cd *)
    | 'd' :: 'c' :: [] -> (CD :: buffer)
    | 'd' :: 'c' :: c :: tl when sep c ->
      iterate (CD :: buffer) (c :: tl)
    (* cwd *)
    | 'd' :: 'w' :: 'c' :: [] -> (CWD :: buffer)
    | 'd' :: 'w' :: 'c' :: c :: tl when sep c ->
      iterate (CWD :: buffer) (c :: tl)
    (* env *)
    | 'v' :: 'n' :: 'e' :: [] -> (ENV :: buffer)
    | 'v' :: 'n' :: 'e' :: c :: tl when sep c ->
      iterate (ENV :: buffer) (c :: tl)
    (* run *)
    | 'n' :: 'u' :: 'r' :: [] -> (RUN :: buffer)
    | 'n' :: 'u' :: 'r' :: c :: tl when sep c ->
      iterate (RUN :: buffer) (c :: tl)
    (* define *)
    | 'e' :: 'n' :: 'i' :: 'f' :: 'e' :: 'd' :: [] -> (GLET :: buffer)
    | 'e' :: 'n' :: 'i' :: 'f' :: 'e' :: 'd' :: c :: tl when sep c ->
      iterate (GLET :: buffer) (c :: tl)
    (* isdir *)
    | 'r' :: 'i' :: 'd' :: 's' :: 'i' :: [] -> (ISDIR :: buffer)
    | 'r' :: 'i' :: 'd' :: 's' :: 'i' :: c :: tl when sep c ->
      iterate (ISDIR :: buffer) (c :: tl)
    (* dir *)
    | 'r' :: 'i' :: 'd' :: [] -> (DIR :: buffer)
    | 'r' :: 'i' :: 'd' :: c :: tl when sep c ->
      iterate (DIR :: buffer) (c :: tl)
    (* is *)
    | 's' :: 'i' :: [] -> (IS :: buffer)
    | 's' :: 'i' :: c :: tl when sep c ->
      iterate (IS :: buffer) (c :: tl)
    | '\'' :: tl ->
      let tok, stack = char_token tl in
      iterate (tok :: buffer) stack
    | '.' :: c :: stack when c >= '0' && c <= '9' ->
      let tok, stack = num_token "" ('.' :: c :: stack) in
      iterate (tok :: buffer) stack
    | c :: stack when c >= '0' && c <= '9' ->
      let tok, stack = num_token "" (c :: stack) in
      iterate (tok :: buffer) stack
    | ('a'..'z' as c) :: stack
    | ('A'..'Z' as c) :: stack ->
      let tok, stack = name_token "" (c :: stack) in
      iterate (tok :: buffer) stack
    | c :: stack ->
      iterate (ERROR (String.make 1 c) :: buffer) stack
    | [] -> 
      buffer
  ) in
  iterate [] stack
)

let char_stack (f : in_channel) = (
  let rec loop stack = (
    match input_char f with
    | c -> loop (c :: stack)
    | exception End_of_file -> close_in f; stack
  ) in
  loop []
)

let find_file (name : string) = (
  let rec iter = (
    function
    | hd :: tl -> (
      match open_in (hd ^ "/" ^ name) with
      | exception _ -> iter tl
      | file -> file
    )
    | [] -> raise (InvalidFilename name)
  ) in
  try open_in name
  with _ ->
  match Sys.getenv_opt "PATH" with
  | Some s -> String.split_on_char ':' s |> iter
  | None   -> raise (InvalidFilename name)
)

let interpreter (tokens : token list) (arg_offset : int) = (
  let rec iterate (input : token list)
                  (vars : token StrMap.t)
                  (funs : (int * string list * token list * string list) StrMap.t) = (
    let rec rpn (input : token list) (stack : token list) (output : token list) = (
      let rec precedence (op : token) (stack : token list) (output : token list) = (
        let higher_order (op : token) (stack_op : token) = (
          let get_lvl (op : token) = (
            match op with
            | FUN _ | POSTFIX_DECR | POSTFIX_INCR -> 0
            | LEN |  RAND | UNOP_MINUS | OPENIN | CLOSE | READ | EXISTS
            | NOT | FLOOR | CEIL | TONUM | OPENOUT | CATCH | SQRT | ENV
            | POW | PREFIX_DECR | PREFIX_INCR | B_NOT | ABS | KEYS| CD
            | TOSTR | TOCHAR | LOG | LN | ASIN | ACOS | ATAN | CWD | DIR
            | ISDIR | SINH | COSH | TANH | SIN | COS | TAN | EXP | SCAN -> 1
            | MULT | DIV | MOD | IS -> 2
            | PLUS | MINUS -> 3
            | B_SHIFTL | B_ASHIFTR | B_LSHIFTR -> 4
            | LESS | LEQ | GREATER | GEQ -> 5
            | NEQ | EQ -> 6
            | B_AND -> 7
            | B_XOR -> 8
            | B_OR -> 9
            | AND -> 10
            | OR -> 11
            | LET | GLET | PRINT | PRINTLN | RETURN
            | WRITE | THROW | FREE | RUN -> 12
            | tok -> raise (InvalidToken (tok, "at higher_order"))
          ) in
          get_lvl op < get_lvl stack_op
        ) in
        match stack with
        | [] ->
          [op], output
        | stack_op :: tl -> (
          match op  , stack_op with
          (* Special cases with parentheses and brackets *)
          | END     , START    -> tl, output
          | BR_END  , BR_START -> tl, (DREF :: output)
          | END     , _ 
          | BR_END  , _        -> precedence op tl (stack_op :: output)
          | START   , _
          | BR_START, _
          | _       , START
          | _       , BR_START -> (op :: stack), output
          | op      , stack_op ->
            if higher_order op stack_op
            then (op :: stack), output
            else precedence op tl (stack_op :: output)
        )
      ) in
      match input with
      | []
      | SEQ     :: _
      | NEWLINE :: _ -> (
        match stack with
        | [] -> (List.rev output), input
        | hd :: tl -> rpn input tl (hd :: output)
      )
      | NAME n :: tl when StrMap.mem n funs ->
        rpn tl (FUN n :: stack) output
      | NAME  _ | STR  _ | INT  _   
      | FLOAT _ | BOOL _ | CHAR _  
      | DICT    | ARR as hd :: tl -> rpn tl stack (hd :: output)
      | op :: tl ->
        let stack, output = precedence op stack output in
        rpn tl stack output
    ) in
    let strip_if (stack : token list) = (
      let rec loop (stack : token list) (cnt : int) = (
        match stack with
        | IF    :: stack -> loop stack (cnt + 1)
        | ELIF  :: stack when cnt = 1 -> (IF :: stack)
        | ELSE  :: stack when cnt = 1 -> stack
        | ENDIF :: stack when cnt = 1 -> stack
        | ENDIF :: stack -> loop stack (cnt - 1)
        | _     :: stack -> loop stack cnt
        | []             -> []
      ) in
      loop stack 1
    ) in
    let strip_else (stack : token list) = (
      let rec loop (stack : token list) (buffer : token list) (cnt : int) = (
        if cnt > 0 then
          match stack with
          | IF    :: tl -> loop tl buffer (cnt + 1)
          | ENDIF :: tl -> loop tl buffer (cnt - 1)
          | _     :: tl -> loop tl buffer cnt
          | []          -> []
        else
          match buffer with
          | hd :: tl -> loop (hd :: stack) tl cnt
          | []       -> stack
      ) in
      let rec store_if (input : token list) (buffer : token list) (cnt : int) = (
        match input with
        | IF    :: tl -> store_if tl (IF :: buffer) (cnt + 1)
        | ELIF  :: tl when cnt = 1 -> loop tl buffer 1
        | ELSE  :: tl when cnt = 1 -> loop tl buffer 1
        | ENDIF :: tl when cnt = 1 -> stack
        | ENDIF :: tl -> store_if tl (ENDIF :: buffer) (cnt - 1)
        | hd    :: tl -> store_if tl (hd :: buffer) cnt
        | []          -> []
      ) in
      store_if stack [] 1
    ) in
    let copy_while (stack : token list) = (
      let rec strip_while (stack : token list) (buffer : token list) (cnt : int) = (
        match stack with
        | WHILE    :: tl -> strip_while tl (WHILE :: buffer) (cnt + 1)
        | ENDWHILE :: tl when cnt = 1 -> List.rev buffer, tl
        | ENDWHILE :: tl -> strip_while tl (ENDWHILE :: buffer) (cnt - 1)
        | hd       :: tl -> strip_while tl (hd :: buffer) cnt
        | []             -> raise (InvalidToken (LIST [], "at copy_while"))
      ) in
      strip_while stack [] 1
    ) in
    let copy_for (stack : token list) = (
      let rec strip_for (stack : token list) (buffer : token list) (cnt : int) = (
        match stack with
        | FOR    :: tl -> strip_for tl (FOR :: buffer) (cnt + 1)
        | ENDFOR :: tl when cnt = 1 -> List.rev buffer, tl
        | ENDFOR :: tl -> strip_for tl (ENDFOR :: buffer) (cnt - 1)
        | hd     :: tl -> strip_for tl (hd :: buffer) cnt
        | []           -> raise (InvalidToken (LIST [], "at copy_for"))
      ) in
      strip_for stack [] 1
    ) in
    let store_fun (input : token list) = (
      let rec parse_args (input : token list) (output : string list) (counter : int) = (
        match input with
        | NEWLINE :: input
        | SEQ     :: input -> input, counter, output
        | NAME n  :: input -> parse_args input (n :: output) (counter + 1)
        | _                -> raise (InvalidFunction "at parse_args")
      ) in 
      let rec skip_fun (input : token list) (output : token list)
                       (locals : string list) (counter : int) = (
        match input with
        | FUN "_DEF_" as hd :: tl -> skip_fun tl (hd :: output) locals (counter + 1)
        | ENDFUN            :: tl when counter = 1 -> tl, List.rev output, locals
        | ENDFUN      as hd :: tl -> skip_fun tl (hd :: output) locals (counter - 1)
        | LET :: NAME n     :: tl when counter = 1 ->
          skip_fun tl (LET :: NAME n :: output) (n :: locals) counter
        | hd                :: tl -> skip_fun tl (hd :: output) locals counter
        | []                      -> raise (InvalidFunction "at skip_fun")
      ) in
      let (input, argc, argv) = parse_args input [] 0 in
      let (output, sequence, locals) = skip_fun input [] [] 1 in
      (argc, argv, sequence, locals), output
    ) in
    let rec eval_rpn (input : token list) (vars : token StrMap.t) (stack : token list) = (
      let dref (stack : token list) (n : int) = (
        let rec loop (stack : token list) (buffer : token list) (index : int) = (
          if index < n then
            match stack with
            | INT i :: ARRAY a :: stack -> (
              match a.(i) with
              | elem        -> loop stack (elem :: buffer) (index + 1)
              | exception _ -> loop stack (OUTOFBOUNDS i :: buffer) (index + 1)
            )
            | STR s :: DICTIONARY d :: stack -> (
              match Hashtbl.find d s with
              | elem                -> loop stack (elem :: buffer) (index + 1)
              | exception Not_found -> loop stack (UNDEFINED s :: buffer) (index + 1)
            )
            | NAME a :: stack -> (
              let elem = (
                match StrMap.find_opt a vars with
                | Some elem -> elem
                | None -> (
                  match StrMap.find_opt ("_GLOBAL_" ^ a) funs with
                  | Some (_, _, [elem], _) -> elem
                  | _ -> UNDEFINED a
                )
              ) in loop stack (elem :: buffer) (index + 1)
            )
            | hd :: tl -> loop tl (hd :: buffer) (index + 1)
            | []       -> loop [] buffer n
          else
            match buffer with
            | hd :: tl -> loop (hd :: stack) tl index
            | []       -> stack
        ) in
        loop stack [] 0
      ) in
      match input with
      | NAME  _ | STR  _ | INT  _
      | FLOAT _ | BOOL _ | CHAR _ 
      | ARR     | DICT as hd :: tl -> eval_rpn tl vars (hd :: stack)
      | op :: input -> (
        match op with
        | LET -> (
          let stack = dref stack 1 in
          match stack with
          | v :: NAME n :: stack ->
            eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (v :: stack)
          | v :: INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | _ -> a.(i) <- v; eval_rpn input vars (v :: stack)
          )
          | v :: STR s :: DICTIONARY d :: stack ->
            Hashtbl.replace d s v; eval_rpn input vars (v :: stack)
          | INT size :: ARR :: NAME n :: stack ->
            let arr = ARRAY (Array.make size (INT 0)) in
            eval_rpn input (StrMap.add n arr (StrMap.remove n vars)) (arr :: stack)
          | INT size :: ARR :: INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | _ -> 
              let arr = ARRAY (Array.make size (INT 0)) in
              a.(i) <- arr; eval_rpn input vars (arr :: stack)
          )
          | INT size :: ARR :: STR s :: DICTIONARY d :: stack ->
            let arr = ARRAY (Array.make size (INT 0)) in
            Hashtbl.replace d s arr; eval_rpn input vars (arr :: stack)
          | INT size :: DICT :: NAME n :: stack ->
            let dict = DICTIONARY (Hashtbl.create size) in
            eval_rpn input (StrMap.add n dict (StrMap.remove n vars)) (dict :: stack)
          | INT size :: DICT :: INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | _ ->
            let dict = DICTIONARY (Hashtbl.create size) in
            a.(i) <- dict; eval_rpn input vars (dict :: stack)
          )
          | INT size :: DICT :: STR s :: DICTIONARY d :: stack ->
            let dict = DICTIONARY (Hashtbl.create size) in
            Hashtbl.replace d s dict; eval_rpn input vars (dict :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LET"))
        )
        | SCAN -> (
          match stack with
          | NAME "int"   :: stack ->
            (try INT (read_int ()) with _ -> SCAN_ERR) :: stack
            |> eval_rpn input vars
          | NAME "str"   :: stack ->
            (try STR (read_line ()) with _ -> SCAN_ERR) :: stack
            |> eval_rpn input vars
          | NAME "float" :: stack ->
            (try Scanf.scanf "%f" (fun f -> FLOAT f) with _ -> SCAN_ERR) :: stack
            |> eval_rpn input vars
          | stack -> raise (InvalidToken (LIST stack, "at SCAN"))
        )
        | PREFIX_INCR -> (
          match stack with
          | STR s :: DICTIONARY d :: stack -> (
            match Hashtbl.find_opt d s with
            | None -> eval_rpn input vars (UNDEFINED s :: stack)
            | Some INT i ->
              let v = INT (i + 1) in
              Hashtbl.replace d s v; eval_rpn input vars (v :: stack)
            | Some FLOAT f ->
              let v = FLOAT (f +. 1.) in
              Hashtbl.replace d s v; eval_rpn input vars (v :: stack)
            | Some tok -> raise (InvalidToken (tok, "at PREFIX_INCR (dict)"))
          )
          | INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | INT j ->
              let v = INT (j + 1) in
              a.(i) <- v; eval_rpn input vars (v :: stack)
            | FLOAT f ->
              let v = FLOAT (f +. 1.) in
              a.(i) <- v; eval_rpn input vars (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_INCR (array)"))
          )
          | NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | INT i ->
              let v = INT (i + 1) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (v :: stack)
            | FLOAT f ->
              let v = FLOAT (f +. 1.) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_INCR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at PREFIX_INCR"))
        )
        | PREFIX_DECR -> (
          match stack with
          | STR s :: DICTIONARY d :: stack -> (
            match Hashtbl.find_opt d s with
            | None -> eval_rpn input vars (UNDEFINED s :: stack)
            | Some INT i ->
              let v = INT (i - 1) in
              Hashtbl.replace d s v; eval_rpn input vars (v :: stack)
            | Some FLOAT f ->
              let v = FLOAT (f -. 1.) in
              Hashtbl.replace d s v; eval_rpn input vars (v :: stack)
            | Some tok -> raise (InvalidToken (tok, "at PREFIX_DECR (dict)"))
          )
          | INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | INT j ->
              let v = INT (j - 1) in
              a.(i) <- v; eval_rpn input vars (v :: stack)
            | FLOAT f ->
              let v = FLOAT (f -. 1.) in
              a.(i) <- v; eval_rpn input vars (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_DECR (array)"))
          )
          | NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | INT i ->
              let v = INT (i - 1) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (v :: stack)
            | FLOAT f ->
              let v = FLOAT (f -. 1.) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (v :: stack)
            | tok -> raise (InvalidToken (tok, "at PREFIX_DECR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at PREFIX_DECR"))
        )
        | POSTFIX_INCR -> (
          match stack with
          | STR s :: DICTIONARY d :: stack -> (
            match Hashtbl.find_opt d s with
            | None -> eval_rpn input vars (UNDEFINED s :: stack)
            | Some INT i ->
              let v = INT (i + 1) in
              Hashtbl.replace d s v; eval_rpn input vars (INT i :: stack)
            | Some FLOAT f ->
              let v = FLOAT (f +. 1.) in
              Hashtbl.replace d s v; eval_rpn input vars (FLOAT f :: stack)
            | Some tok -> raise (InvalidToken (tok, "at POSTFIX_INCR (dict)"))
          )
          | INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | INT j ->
              let v = INT (j + 1) in
              a.(i) <- v; eval_rpn input vars (INT j :: stack)
            | FLOAT f ->
              let v = FLOAT (f +. 1.) in
              a.(i) <- v; eval_rpn input vars (FLOAT f :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_INCR (array)"))
          )
          | NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | INT i ->
              let v = INT (i + 1) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (INT i :: stack)
            | FLOAT f ->
              let v = FLOAT (f +. 1.) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (FLOAT f :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_INCR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at POSTFIX_INCR"))
        )
        | POSTFIX_DECR -> (
          match stack with
          | STR s :: DICTIONARY d :: stack -> (
            match Hashtbl.find_opt d s with
            | None -> eval_rpn input vars (UNDEFINED s :: stack)
            | Some INT i ->
              let v = INT (i - 1) in
              Hashtbl.replace d s v; eval_rpn input vars (INT i :: stack)
            | Some FLOAT f ->
              let v = FLOAT (f -. 1.) in
              Hashtbl.replace d s v; eval_rpn input vars (FLOAT f :: stack)
            | Some tok -> raise (InvalidToken (tok, "at POSTFIX_DECR (dict)"))
          )
          | INT i :: ARRAY a :: stack -> (
            match a.(i) with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | INT j ->
              let v = INT (j - 1) in
              a.(i) <- v; eval_rpn input vars (INT j :: stack)
            | FLOAT f ->
              let v = FLOAT (f -. 1.) in
              a.(i) <- v; eval_rpn input vars (FLOAT f :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_DECR (array)"))
          )
          | NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | INT i ->
              let v = INT (i - 1) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (INT i :: stack)
            | FLOAT f ->
              let v = FLOAT (f -. 1.) in
              eval_rpn input (StrMap.add n v (StrMap.remove n vars)) (FLOAT f :: stack)
            | tok -> raise (InvalidToken (tok, "at POSTFIX_DECR"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at POSTFIX_DECR"))
        )
        | DREF -> (
          let stack = dref stack 1 in
          match stack with
          | first :: STR a :: DICTIONARY d :: stack -> (
            match Hashtbl.find d a, first with
            | exception Not_found ->
              eval_rpn input vars (UNDEFINED a :: stack)
            | STR s, INT i -> (
              match s.[i] with
              | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
              | c -> eval_rpn input vars (CHAR c :: stack)
            )
            | DICTIONARY d, INT i -> 
              STR (string_of_int i) :: DICTIONARY d :: stack
              |> eval_rpn input vars
            | elem, first ->
              eval_rpn input vars (first :: elem :: stack)
          )
          | first :: INT i :: ARRAY a :: stack -> (
            match a.(i), first with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | STR s, INT i -> (
              match s.[i] with
              | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
              | c -> eval_rpn input vars (CHAR c :: stack)
            )
            | DICTIONARY d, INT i -> 
              STR (string_of_int i) :: DICTIONARY d :: stack
              |> eval_rpn input vars
            | elem, first -> eval_rpn input vars (first :: elem :: stack)
          )
          | INT i :: NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | ARRAY a ->
              eval_rpn input vars (INT i :: ARRAY a :: stack)
            | STR s -> (
              match s.[i] with
              | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
              | c -> eval_rpn input vars (CHAR c :: stack)
            )
            | DICTIONARY d ->
              STR (string_of_int i) :: DICTIONARY d :: stack
              |> eval_rpn input vars
            | tok -> raise (InvalidToken (tok, "at DREF"))
          )
          | INT i :: STR s :: stack -> (
            match s.[i] with
            | exception _ -> eval_rpn input vars (OUTOFBOUNDS i :: stack)
            | c -> eval_rpn input vars (CHAR c :: stack)
          )
          | STR attr :: NAME n :: stack -> (
            let elem = (
              match StrMap.find_opt n vars with
              | Some elem -> elem
              | None -> (
                match StrMap.find_opt ("_GLOBAL_" ^ n) funs with
                | Some (_, _, [elem], _) -> elem
                | _ -> UNDEFINED n
              )
            ) in
            match elem with
            | UNDEFINED n -> eval_rpn input vars (UNDEFINED n :: stack)
            | DICTIONARY d -> eval_rpn input vars (STR attr :: DICTIONARY d :: stack)
            | tok -> raise (InvalidToken (tok, "at DREF (dict)"))
          )
          | stack -> raise (InvalidToken (LIST stack, "at DREF"))
        )
        | FUN n -> (
          let argc, args, sequence, locals = 
            match StrMap.find_opt n funs with
            | Some f -> f
            | None -> raise (InvalidToken (FUN n, "at FUN")) in
          let stack = dref stack argc in
          let rec assign (args : string list) (stack : token list) (vars : token StrMap.t) funs = (
            match args, stack with
            | n :: tl , v :: stack -> assign tl stack (StrMap.add n v vars) (StrMap.remove n funs)
            | []      , _          -> vars, funs, stack
            | _                    -> raise (InvalidFunction ("at assign for FUN " ^ n))
          ) in
          let rec set_locals (locals : string list) funs = (
            match locals with
            | hd :: tl -> set_locals tl (StrMap.remove hd funs)
            | [] -> funs
          ) in
          let fn_vars, fn_funs, stack = assign args stack StrMap.empty funs in
          let fn_funs = set_locals locals fn_funs in
          let res_vars = iterate sequence fn_vars fn_funs in
          if StrMap.mem "_RETURN_" res_vars then
            StrMap.find "_RETURN_" res_vars :: stack
            |> eval_rpn input vars
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
            string_of_bool a |> print_string;
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
            string_of_bool a |> print_endline;
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
          | BOOL b :: e :: stack ->
            eval_rpn (PLUS :: input) vars (STR (string_of_bool b) :: e :: stack)
          | e :: BOOL b :: stack ->
            eval_rpn (PLUS :: input) vars (e :: STR (string_of_bool b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at PLUS") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at MINUS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at MINUS"))
        )
        | UNOP_MINUS -> (
          let stack = dref stack 1 in
          match stack with
          | INT a :: stack ->
            eval_rpn input vars ((INT (-a)) :: stack)
          | FLOAT a :: stack ->
            eval_rpn input vars ((FLOAT (-.a) :: stack))
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at UNOP_MINUS") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at MULT") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at MULT"))
        )
        | DIV -> (
          let stack = dref stack 2 in
          match stack with
          | INT 0 :: a :: stack ->
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", INT 0) at DIV") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at DIV") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at DIV"))
        )
        | MOD -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars ((INT (a mod b)) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at MOD") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at LESS") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at GREATER") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at LEQ") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at GEQ") :: stack)
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
          | BOOL b :: BOOL a :: stack ->
            eval_rpn input vars (BOOL (a <> b) :: stack)
          | ARRAY _ :: _ :: stack ->
            eval_rpn input vars ((BOOL true) :: stack)
          | _ :: ARRAY _ :: stack ->
            eval_rpn input vars ((BOOL true) :: stack)
          | DICTIONARY _ :: _ :: stack ->
            eval_rpn input vars ((BOOL true) :: stack)
          | _ :: DICTIONARY _ :: stack ->
            eval_rpn input vars ((BOOL true) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at NEQ") :: stack)
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
          | BOOL b :: BOOL a :: stack ->
            eval_rpn input vars (BOOL (a = b) :: stack)
          | ARRAY _ :: _ :: stack ->
            eval_rpn input vars ((BOOL false) :: stack)
          | _ :: ARRAY _ :: stack ->
            eval_rpn input vars ((BOOL false) :: stack)
          | DICTIONARY _ :: _ :: stack ->
            eval_rpn input vars ((BOOL false) :: stack)
          | _ :: DICTIONARY _ :: stack ->
            eval_rpn input vars ((BOOL false) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at EQ") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at EQ"))
        )
        | AND -> (
          let stack = dref stack 2 in
          match stack with
          | _ :: BOOL false :: stack -> eval_rpn input vars (BOOL false :: stack)
          | BOOL b :: BOOL a :: stack ->
            BOOL (a && b) :: stack
            |> eval_rpn input vars
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at AND") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at AND"))
        )
        | OR -> (
          let stack = dref stack 2 in
          match stack with
          | _ :: BOOL true :: stack -> eval_rpn input vars (BOOL true :: stack)
          | BOOL b :: BOOL a :: stack ->
            BOOL (a || b) :: stack
            |> eval_rpn input vars
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at OR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OR"))
        )
        | NOT -> (
          let stack = dref stack 1 in
          match stack with
          | BOOL a :: stack ->
            BOOL (not a) :: stack
            |> eval_rpn input vars
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at EQ") :: stack)
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
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at POW") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at POW"))
        )
        | LEN -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack ->
            eval_rpn input vars ((INT (String.length s)) :: stack)
          | ARRAY a :: stack ->
            eval_rpn input vars ((INT (Array.length a)) :: stack)
          | DICTIONARY d :: stack ->
            eval_rpn input vars ((INT (Hashtbl.length d)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at LEN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LEN"))
        )
        | RAND -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT (Random.int i) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at RAND") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at RAND"))
        )
        | FLOOR -> (
          let stack = dref stack 1 in
          match stack with
          | INT   _ :: _     -> eval_rpn input vars stack
          | FLOAT f :: stack ->
            INT (floor f |> int_of_float) :: stack
            |> eval_rpn input vars
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at FLOOR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at FLOOR"))
        )
        | CEIL -> (
          let stack = dref stack 1 in
          match stack with
          | INT   _ :: _     -> eval_rpn input vars stack
          | FLOAT f :: stack ->
            INT (ceil f |> int_of_float) :: stack
            |> eval_rpn input vars
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at CEIL") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at CEIL"))
        )
        | TONUM -> (
          let stack = dref stack 1 in
          match stack with
          | INT   _ :: _ 
          | FLOAT _ :: _     -> eval_rpn input vars stack
          | CHAR c  :: stack ->
            INT (Char.code c) :: stack
            |> eval_rpn input vars
          | STR s :: stack ->
            let num = try INT   (int_of_string s)
            with _ -> try FLOAT (float_of_string s)
            with _ -> TONUM_ERR s in
            eval_rpn input vars (num :: stack)
          | t :: stack ->
            eval_rpn input vars (TONUM_ERR (" (" ^ string_of_token t ^ ")") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TONUM"))
        )
        | OPENIN -> (
          let stack = dref stack 1 in
          match stack with
          | STR n :: stack -> (
            match open_in n with
            | file        -> eval_rpn input vars (INCHAN file :: stack)
            | exception _ -> eval_rpn input vars (NOT_FOUND n :: stack)
          )
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at OPENIN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OPENIN"))
        )
        | OPENOUT -> (
          let stack = dref stack 1 in
          match stack with
          | STR n :: stack -> (
            match open_out n with
            | file        -> eval_rpn input vars (OUTCHAN file :: stack)
            | exception _ -> eval_rpn input vars (NOT_FOUND  n :: stack)
          )
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at OPENOUT") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at OPENOUT"))
        )
        | CLOSE -> (
          let stack = dref stack 1 in
          match stack with
          | INCHAN c  :: stack -> close_in  c; eval_rpn input vars stack
          | OUTCHAN c :: stack -> close_out c; eval_rpn input vars stack
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at CLOSE") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at CLOSE"))
        )
        | CATCH -> (
          let stack = dref stack 1 in
          match stack with
          | EOF           :: stack
          | SCAN_ERR      :: stack
          | TONUM_ERR _   :: stack
          | UNDEFINED _   :: stack
          | NOT_FOUND _   :: stack
          | OUTOFBOUNDS _ :: stack -> eval_rpn input vars (BOOL  true :: stack)
          | _             :: stack -> eval_rpn input vars (BOOL false :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at CATCH"))
        )
        | READ -> (
          let stack = dref stack 1 in
          match stack with
          | INCHAN c :: NAME t :: stack -> (
            match t with
            | "str" -> (
              match input_line c with
              | s                     -> eval_rpn input vars (STR s :: stack)
              | exception End_of_file -> eval_rpn input vars (EOF   :: stack)
            )
            | "char" -> (
              match input_char c with
              | c                     -> eval_rpn input vars (CHAR c :: stack)
              | exception End_of_file -> eval_rpn input vars (EOF    :: stack)
            )
            | _ -> raise (InvalidToken (NAME t, "at READ"))
          )
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at READ") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at READ"))
        )
        | WRITE -> (
          let stack = dref stack 2 in
          match stack with
          | OUTCHAN o :: STR s :: stack ->
            output_string o s; eval_rpn input vars stack
          | OUTCHAN o :: CHAR c :: stack ->
            output_char o c; eval_rpn input vars stack
          | OUTCHAN o :: INT i :: stack ->
            output_string o (string_of_int i); eval_rpn input vars stack
          | OUTCHAN o :: FLOAT f :: stack ->
            output_string o (string_of_float f); eval_rpn input vars stack
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at WRITE") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at WRITE"))
        )
        | TOCHAR -> (
          let stack = dref stack 1 in
          match stack with
          | INT  i :: stack ->
            eval_rpn input vars (CHAR (Char.chr i) :: stack)
          | STR  s :: stack ->
            if String.length s >= 1 
            then eval_rpn input vars (CHAR  (s.[0]) :: stack)
            else eval_rpn input vars (OUTOFBOUNDS 0 :: stack)
          | CHAR _ :: _     ->
            eval_rpn input vars stack
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at TOCHAR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TOCHAR"))
        )
        | TOSTR -> (
          let stack = dref stack 1 in
          match stack with
          | STR   _ :: _     ->
            eval_rpn input vars stack
          | INT   i :: stack ->
            eval_rpn input vars (STR (string_of_int i) :: stack)
          | FLOAT f :: stack ->
            eval_rpn input vars (STR (string_of_float f) :: stack)
          | CHAR  c :: stack ->
            eval_rpn input vars (STR (String.make 1 c) :: stack)
          | BOOL b :: stack ->
            eval_rpn input vars (STR (string_of_bool b) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at TOSTR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TOSTR"))
        )
        | B_AND -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a land b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_AND") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_AND"))
        )
        | B_OR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lor b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_OR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_OR"))
        )
        | B_XOR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lxor b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_XOR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_XOR"))
        )
        | B_SHIFTL -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lsl b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_SHIFTL") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_SHIFTL"))
        )
        | B_LSHIFTR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a asr b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_LSHIFTR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_LSHIFTR"))
        )
        | B_ASHIFTR -> (
          let stack = dref stack 2 in
          match stack with
          | INT b :: INT a :: stack ->
            eval_rpn input vars (INT (a lsr b) :: stack)
          | b :: a :: stack -> 
            let a, b = string_of_token a, string_of_token b in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at B_ASHIFTR") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_ASHIFTR"))
        )
        | B_NOT -> (
          let stack = dref stack 1 in
          match stack with
          | INT i :: stack ->
            eval_rpn input vars (INT (lnot i) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at B_NOT") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at B_NOT"))
        )
        | SINH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sinh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sinh (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at SINH") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SINH"))
        )
        | ASIN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (asin f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (asin (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at ASIN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ASIN"))
        )
        | SIN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sin f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sin (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at SIN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SIN"))
        )
        | COSH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (cosh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (cosh (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at COSH") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at COSH"))
        )
        | ACOS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (acos f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (acos (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at ACOS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ACOS"))
        )
        | COS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (cos f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (cos (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at COS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at COS"))
        )
        | TANH -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (tanh f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (tanh (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at TANH") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TANH"))
        )
        | ATAN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (atan f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (atan (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at ATAN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ATAN"))
        )
        | TAN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (tan f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (tan (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at TAN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at TAN"))
        )
        | ABS -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (abs_float f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (INT (abs i) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at ABS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at ABS"))
        )
        | SQRT -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (sqrt f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (sqrt (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at SQRT") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at SQRT"))
        )
        | LOG -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (log10 f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (log10 (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at LOG") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LOG"))
        )
        | LN -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (log f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (log (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at LN") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at LN"))
        )
        | EXP -> (
          let stack = dref stack 1 in
          match stack with
          | FLOAT f :: stack ->
            eval_rpn input vars (FLOAT (exp f) :: stack)
          | INT i :: stack ->
            eval_rpn input vars (FLOAT (exp (float_of_int i)) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at EXP") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at EXP"))
        )
        | FREE -> (
          match stack with
          | NAME n :: stack ->
            eval_rpn input (StrMap.remove n vars) stack
          | STR s :: DICTIONARY d :: stack ->
            Hashtbl.remove d s; eval_rpn input vars stack
          | stack -> raise (InvalidToken (LIST stack, "at FREE"))
        )
        | EXISTS -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack ->
            eval_rpn input vars (BOOL (Sys.file_exists s) :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at EXISTS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at EXISTS"))
        )
        | KEYS -> (
          let stack = dref stack 1 in
          match stack with
          | DICTIONARY d :: stack ->
            let keys = Hashtbl.to_seq_keys d 
            |> Array.of_seq |> Array.map (fun e -> STR e) in
            eval_rpn input vars (ARRAY keys :: stack)
          | a :: stack -> 
            let a = string_of_token a in
            eval_rpn input vars (UNDEFINED ("(" ^ a ^ ") at KEYS") :: stack)
          | stack -> raise (InvalidToken (LIST stack, "at KEYS"))
        )
        | CD -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack -> (
            match Sys.chdir s with
            | exception _ -> eval_rpn input vars (NOT_FOUND         s :: stack)
            | ()          -> eval_rpn input vars (STR (Sys.getcwd ()) :: stack)
          )
          | stack -> raise (InvalidToken (LIST stack, "at CD"))
        )
        | DIR -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack -> (
            match Sys.readdir s with
            | exception _ -> eval_rpn input vars (NOT_FOUND s :: stack)
            | a -> ARRAY (Array.map (fun e -> STR e) a) :: stack
                |> eval_rpn input vars
          )
          | stack -> raise (InvalidToken (LIST stack, "at DIR"))
        )
        | ISDIR -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack -> (
            match Sys.is_directory s with
            | exception _ -> eval_rpn input vars (NOT_FOUND s :: stack)
            | b -> eval_rpn input vars (BOOL b :: stack)
          )
          | stack -> raise (InvalidToken (LIST stack, "at ISDIR"))
        )
        | ENV -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack -> (
            match Sys.getenv_opt s with
            | Some s -> eval_rpn input vars (STR       s :: stack)
            | None   -> eval_rpn input vars (NOT_FOUND s :: stack)
          )
          | stack -> raise (InvalidToken (LIST stack, "at ENV"))
        )
        | RUN -> (
          let stack = dref stack 1 in
          match stack with
          | STR s :: stack ->
            INT (Sys.command s) :: stack
            |> eval_rpn input vars
          | stack -> raise (InvalidToken (LIST stack, "at RUN"))
        )
        | IS -> (match stack with
          | t :: stack -> (
            let stack = dref stack 1 in
            match t, stack with
            | NAME   "str", STR _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |           _ , STR _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | NAME   "int", INT _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |           _ , INT _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | NAME  "char", CHAR _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |           _ , CHAR _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | NAME  "bool", BOOL _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |           _ , BOOL _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | NAME "float", FLOAT _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |           _ , FLOAT _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            |  ARR, ARRAY      _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |   _ , ARRAY      _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | DICT, DICTIONARY _ :: stack -> eval_rpn input vars (BOOL true  :: stack)
            |   _ , DICTIONARY _ :: stack -> eval_rpn input vars (BOOL false :: stack)
            | a, b :: stack -> 
              let a, b = string_of_token a, string_of_token b in
              eval_rpn input vars (UNDEFINED ("(" ^ a ^ ", " ^ b ^ ") at IS") :: stack)
            | _, [] -> raise (InvalidToken (LIST [], "at IS"))
          )
          | [] -> raise (InvalidToken (LIST [], "at IS"))
        )
        | CWD   -> eval_rpn input vars (STR (Sys.getcwd ()) :: stack)
        | THROW -> raise (InvalidToken (LIST stack, "at THROW"))
        | op    -> raise (InvalidToken (op, "at eval_rpn"))
      )
      | [] -> vars
    ) in  
    match input with
    | []
    | CONTINUE :: _ -> vars
    | SEQ     :: input
    | ENDIF   :: input
    | NEWLINE :: input -> iterate input vars funs
    | BREAK   :: input -> StrMap.add "_BREAK_" BREAK (StrMap.remove "_BREAK_" vars)
    | INCLUDE :: STR n :: input ->
      let tokens = find_file n |> char_stack |> tokenizer in
      iterate (tokens @ input) vars funs
    | IF :: input -> (
      let expr, input = rpn (LET :: NAME "_EVAL_" :: input) [] [] in
      let vars = eval_rpn expr vars [] in
      match StrMap.find_opt "_EVAL_" vars with
      | None            -> raise (InvalidToken (NAME "_EVAL_", "at IF"))
      | Some BOOL true  -> iterate (strip_else input) vars funs
      | Some BOOL false -> iterate (strip_if input) vars funs
      | Some eval       -> raise (InvalidToken (eval, "at IF"))
    )
    | RETURN :: input -> (
      let expr, _ = rpn (LET :: NAME "_RETURN_" :: input) [] [] in
      eval_rpn expr vars []
    )
    | WHILE :: tl -> (
      let cond, stack = rpn (LET :: NAME "_EVAL_" :: tl) [] [] in
      let copy, stack = copy_while stack in
      let rec loop (vars : token StrMap.t) = (
        let vars = eval_rpn cond vars [] in
        match StrMap.find_opt "_EVAL_" vars with
        | None -> raise (InvalidToken (NAME "_EVAL_", "at WHILE"))
        | Some BOOL true ->
          let vars = iterate copy vars funs in
          if StrMap.mem "_RETURN_" vars then
            vars
          else if StrMap.mem "_BREAK_" vars then
            iterate stack (StrMap.remove "_BREAK_" vars) funs
          else
            loop vars
        | Some BOOL false -> iterate stack vars funs
        | Some eval -> raise (InvalidToken (eval, "at WHILE"))
      ) in
      loop vars
    )
    | FOR :: tl -> (
      let expr, stack = rpn tl [] [] in
      let vars = eval_rpn expr vars [] in
      let cond, stack = rpn (LET :: NAME "_EVAL_" :: (List.tl stack)) [] [] in
      let term, stack = rpn (List.tl stack) [] [] in
      let copy, stack = copy_for stack in
      let rec loop (vars : token StrMap.t) = (
        let vars = eval_rpn cond vars [] in
        match StrMap.find_opt "_EVAL_" vars with
        | None -> raise (InvalidToken (NAME "_EVAL_", "at FOR"))
        | Some BOOL true ->
          let vars = iterate copy vars funs in
          if StrMap.mem "_RETURN_" vars then
            vars
          else if StrMap.mem "_BREAK_" vars then
            iterate stack (StrMap.remove "_BREAK_" vars) funs
          else
            loop (eval_rpn term vars [])
        | Some BOOL false -> iterate stack vars funs
        | Some eval -> raise (InvalidToken (eval, "at FOR"))
      ) in
      loop vars
    )
    | FUN "_DEF_" :: NAME n :: input ->
      let new_fun, input = store_fun input in
      let funs = StrMap.remove ("_GLOBAL_" ^ n) (StrMap.remove n funs) in
      iterate input vars (StrMap.add n new_fun funs)
    | GLET :: NAME n :: input -> (
      let expr, input = rpn (LET :: NAME "_DEFINE_" :: input) [] [] in
      let vars = eval_rpn expr vars [] in
      let elem = StrMap.find "_DEFINE_" vars in
      let funs = StrMap.remove ("_GLOBAL_" ^ n) (StrMap.remove n funs) in
      iterate input vars (StrMap.add ("_GLOBAL_" ^ n) (0, [], [elem], []) funs)
    )
    | _ -> (
      let expr, input = rpn input [] [] in
      let vars = eval_rpn expr vars [] in
      iterate input vars funs
    )
  ) in
  (* Initializing random number generator *)
  Random.self_init ();
  (* Initializing args array *)
  let args = ARRAY (
    Array.sub Sys.argv arg_offset ((Array.length Sys.argv) - arg_offset) 
    |> Array.map (fun e -> STR e)
  ) in
  iterate tokens (StrMap.add "args" args StrMap.empty) StrMap.empty
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
    (* arg_offset just constant 1 for now, but might add more options in the future. *)
    match interpreter tokens 1 with
    | exception InvalidToken (tok, s) ->
      Printf.fprintf stderr "Encountered an exception InvalidToken %s!\nToken: %s\n" s (string_of_token tok)
    | exception InvalidFilename s ->
      Printf.fprintf stderr "Encountered an exception InvalidFilename %s!\n" s
    | exception InvalidFunction s ->
      Printf.fprintf stderr "Encountered an exception InvalidFunction %s!\n" s
    | exception e -> raise e
    | vars -> (
      match StrMap.find_opt "_RETURN_" vars with
      | Some INT i -> exit i
      | _ -> ()
    )
  )
)
