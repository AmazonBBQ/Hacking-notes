type token =
  | Number of float
  | TLParen
  | TRParen
  | TComma
  | TSemicolon
  | TAsterisk
  | TDot
  | TQuote
  | TKeyword of string
  | TIdentifier of string
  | TOperator of string

let tokenize (input : string) : token list =
  let is_keyword (s : string) : bool =
    List.mem
      (String.uppercase_ascii s)
      [ "SELECT"
      ; "FROM"
      ; "WHERE"
      ; "AND"
      ; "OR"
      ; "NOT"
      ; "INSERT"
      ; "UPDATE"
      ; "DELETE"
      ; "CREATE"
      ; "DROP"
      ; "ALTER"
      ; "TABLE"
      ; "DATABASE"
      ; "INTO"
      ; "VALUES"
      ; "SET"
      ; "JOIN"
      ; "LEFT"
      ; "RIGHT"
      ; "INNER"
      ; "OUTER"
      ; "ON"
      ; "ORDER"
      ; "BY"
      ; "GROUP"
      ; "HAVING"
      ; "LIMIT"
      ; "OFFSET"
      ; "UNION"
      ; "ALL"
      ; "DISTINCT"
      ; "CASE"
      ; "WHEN"
      ; "THEN"
      ; "ELSE"
      ; "END"
      ; "AS"
      ; "IN"
      ; "LIKE"
      ; "BETWEEN"
      ; "IS"
      ; "NULL"
      ; "TRUE"
      ; "FALSE"
      ; "CAST"
      ; "CROSS"
      ; "FULL"
      ; "PRIMARY"
      ; "FOREIGN"
      ; "KEY"
      ; "UNIQUE"
      ; "VIEW"
      ; "INDEX"
      ; "DESC"
      ; "ASC"
      ; "WITH"
      ; "AUTOINCREMENT"
      ; "DEFAULT"
      ; "CHECK"
      ; "CONSTRAINT"
      ; "REFERENCES"
      ; "COLLATE"
      ; "NATURAL"
      ; "FETCH"
      ; "ROWS"
      ; "OVER"
      ; "PARTITION"
      ]
  in
  let is_identifier_char (c : char) : bool =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
  in
  let len : int = String.length input in
  let read_while_predicate
        (initial_char : char)
        (start_i : int)
        (predicate : int -> char -> bool)
    : string * int
    =
    let buffer = Buffer.create 16 in
    Buffer.add_char buffer initial_char;
    let rec read_chars i =
      if i >= len
      then i
      else if predicate i input.[i]
      then (
        Buffer.add_char buffer input.[i];
        read_chars (i + 1))
      else i
    in
    let end_i = read_chars (start_i + 1) in
    Buffer.contents buffer, end_i
  in
  let rec aux (acc : token list) (i : int) : token list =
    if i >= len
    then List.rev acc
    else (
      match input.[i] with
      | ' ' | '\t' | '\n' | '\r' -> aux acc (i + 1)
      | '\'' -> aux (TQuote :: acc) (i + 1)
      | '(' -> aux (TLParen :: acc) (i + 1)
      | ')' -> aux (TRParen :: acc) (i + 1)
      | ',' -> aux (TComma :: acc) (i + 1)
      | ';' -> aux (TSemicolon :: acc) (i + 1)
      | '*' -> aux (TAsterisk :: acc) (i + 1)
      | '.' -> aux (TDot :: acc) (i + 1)
      | '=' | '<' | '>' | '!' | '+' | '-' | '/' | '%' | '|' ->
        let op, end_i =
          read_while_predicate input.[i] i (fun j c ->
            match c with
            | '=' | '<' | '>' | '!' -> j = i + 1
            | '-' when input.[i] = '-' -> j = i + 1
            | _ -> false)
        in
        aux (TOperator op :: acc) end_i
      | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
        let word, end_i =
          read_while_predicate input.[i] i (fun _j c -> is_identifier_char c)
        in
        let token =
          if is_keyword word
          then TKeyword (String.uppercase_ascii word)
          else TIdentifier word
        in
        aux (token :: acc) end_i
      | c when c >= '0' && c <= '9' ->
        let num_str, end_i =
          read_while_predicate input.[i] i (fun _j c ->
            if c >= '0' && c <= '9' then true else false)
        in
        let num_str, end_i =
          if
            end_i < len
            && input.[end_i] = '.'
            && end_i + 1 < len
            && input.[end_i + 1] >= '0'
            && input.[end_i + 1] <= '9'
          then (
            let rest, final_i =
              read_while_predicate '.' end_i (fun _j c -> c >= '0' && c <= '9')
            in
            let decimal_part = String.sub rest 1 (String.length rest - 1) in
            num_str ^ "." ^ decimal_part, final_i + 1)
          else num_str, end_i
        in
        aux (Number (float_of_string num_str) :: acc) end_i
      | _ -> aux acc (i + 1))
  in
  aux [] 0
;;

let is_safe : token list -> bool =
  List.for_all (fun token ->
    match token with
    | TKeyword k -> List.mem k [ "IS"; "NULL"; "OR"; "AND"; "NOT"; "TRUE"; "FALSE" ]
    | TQuote -> false
    | TIdentifier _
    | Number _
    | TLParen
    | TRParen
    | TComma
    | TSemicolon
    | TAsterisk
    | TDot -> true
    | TOperator op ->
      op = "="
      || op = ">"
      || op = "<"
      || op = ">="
      || op = "<="
      || op = "!="
      || op = "+"
      || op = "-"
      || op = "|")
;;

let rec interleave (parts : 'a list) (placeholders : 'a list) (acc : 'a list) : 'a list =
  match parts, placeholders with
  | [], [] -> acc
  | p :: ps, [] -> interleave ps [] (acc @ [ p ])
  | p :: ps, ph :: phs -> interleave ps phs (acc @ [ p; ph ])
  | [], _ :: _ -> raise (Failure "Remaining placeholder values")
;;

let sanitize_sql (query : string) (placeholders : string list) : (string, string) result =
  let token_map = List.map tokenize placeholders in
  if List.for_all is_safe token_map
  then (
    let parts = String.split_on_char '?' query in
    let num_placeholders = List.length parts - 1 in
    if num_placeholders > List.length placeholders
    then Error "Not enough input values provided"
    else if num_placeholders < List.length placeholders
    then Error "Too many input values provided"
    else Ok (interleave parts placeholders [] |> String.concat ""))
  else Error "Unsafe query detected"
;;
