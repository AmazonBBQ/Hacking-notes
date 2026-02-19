type blog_post =
  { released : bool
  ; title : string
  ; date : string
  ; body : string
  }

let blog_jsons : string list =
  let read_all (name : string) : string =
    let ic = open_in name in
    let content = really_input_string ic @@ in_channel_length ic in
    close_in ic;
    content
  in
  Sys.readdir "./bin/blogs"
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".json")
  |> List.map (fun name -> read_all ("./bin/blogs" ^ "/" ^ name))
;;

let to_blog_post (json : string) : blog_post =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_string json in
  { released = j |> member "released" |> to_bool
  ; title = j |> member "title" |> to_string
  ; date = j |> member "date" |> to_string
  ; body = j |> member "body" |> to_string
  }
;;

let insert_blog (db : Sql.db) (post : blog_post) : unit =
  ignore
  @@ Sql.execute db
  @@ Printf.sprintf
       "INSERT INTO blog_posts (title, date, body, released) VALUES ('%s', '%s', '%s', \
        %b)"
       post.title
       post.date
       post.body
       post.released
;;

let create_db () : Sql.db =
  if Sys.file_exists "blog.db" then Sys.remove "blog.db";
  let db = Sql.open_db "blog.db" in
  let _ =
    Sql.create_table
      db
      "blog_posts"
      [ "id INTEGER PRIMARY KEY AUTOINCREMENT"
      ; "title TEXT"
      ; "date TEXT"
      ; "body TEXT"
      ; "released BOOLEAN"
      ]
  in
  db
;;

let () =
  let db = create_db () in
  List.iter (fun json -> insert_blog db (to_blog_post json)) blog_jsons;
  Sql.close_db db
;;