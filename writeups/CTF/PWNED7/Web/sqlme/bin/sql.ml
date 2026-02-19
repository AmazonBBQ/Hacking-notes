type db = Sqlite3.db

let open_db (filename : string) : db =
  let db = Sqlite3.db_open filename in
  ignore (Sqlite3.enable_load_extension db true);
  ignore (Sqlite3.exec db "SELECT load_extension('plugins/fileio.so');");
  db
;;

let close_db (db : db) : unit = ignore (Sqlite3.db_close db)

let row_data (row : Sqlite3.row) : string list =
  let data = ref [] in
  let rec loop i =
    try
      let value =
        match row.(i) with
        | None -> "NULL"
        | Some s -> s
      in
      data := !data @ [ value ];
      loop (i + 1)
    with
    | _ -> ()
  in
  loop 0;
  !data
;;

let query (db : db) (sql : string) : string list list =
  let results = ref [] in
  let callback (row : Sqlite3.row) (_headers : Sqlite3.headers) : unit =
    results := !results @ [ row_data row ]
  in
  let rc = Sqlite3.exec db ~cb:callback sql in
  (match rc with
   | Sqlite3.Rc.OK -> ()
   | _ ->
     Printf.eprintf "SQL Error on query: %s\n" sql;
     Printf.eprintf "Error code: %s\n" (Sqlite3.Rc.to_string rc);
     Printf.eprintf "Error message: %s\n" (Sqlite3.errmsg db));
  !results
;;

let query_single (db : db) (sql : string) : string list option =
  match query db sql with
  | [] -> None
  | x :: _ -> Some x
;;

let execute (db : db) (sql : string) : (unit, string) result =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> Ok ()
  | rc -> Error (Sqlite3.Rc.to_string rc)
;;

let create_table (db : db) (table_name : string) (columns : string list)
  : (unit, string) result
  =
  execute db
  @@ Printf.sprintf "CREATE TABLE %s (%s)" table_name
  @@ String.concat ", " columns
;;

let sanitize_sql (query : string) (inout : string) : (string, string) result =
  Sanitize.sanitize_sql query [ inout ]
;;
