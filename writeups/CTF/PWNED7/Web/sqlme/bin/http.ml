let index (_rq : Dream.request) : Dream.response Lwt.t =
  Dream.html @@ Template_index.render ()
;;

let blog (db : Sql.db) (rq : Dream.request) : Dream.response Lwt.t =
  [%require_params
    [ "title" ]
  , let post_body =
      match
        Sql.sanitize_sql
          "SELECT body FROM blog_posts WHERE title = '?' AND released = 1"
          title
      with
      | Ok sanitized_query -> Sql.query db sanitized_query
      | Error err ->
        Printf.eprintf "Error sanitizing SQL query: %s\n" err;
        []
    in
    Dream.html @@ Template_blogpost.render title @@ List.hd @@ List.hd post_body]
;;

let blog_list (db : Sql.db) (_rq : Dream.request) : Dream.response Lwt.t =
  Dream.html
  @@ Template_bloglist.render
  @@ List.flatten
  @@ Sql.query db "SELECT title FROM blog_posts WHERE released = 1"
;;

let redirect_handler (target : string) (rq : Dream.request) : Dream.response Lwt.t =
  Dream.redirect rq target
;;

let serve (db : Sql.db) =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [ Dream.get "/" index
       ; Dream.get "/blog" @@ redirect_handler "/blogs"
       ; Dream.get "/blog/" @@ redirect_handler "/blogs"
       ; Dream.get "/blog/:title" @@ blog db
       ; Dream.get "/blogs" @@ blog_list db
       ]
;;


let () =
  let db = Sql.open_db "blog.db" in
  serve db;
  Sql.close_db db
;;