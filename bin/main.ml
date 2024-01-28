open Lwt
open Cohttp_lwt_unix
open Sqlite3
open Tyxml.Html


let random_string len : string = Base64.encode_exn ?pad:(Some false) ?alphabet:(Some Base64.uri_safe_alphabet) @@ String.of_bytes @@  Bytes.map (fun _ -> Char.chr (Random.int (255))) (Bytes.create len)

let new_id _ : string = random_string 16 

let form_action_param (action: string) = input ~a:[a_name "action"; a_id "action"; a_input_type `Text; a_value action; a_hidden ()] ()

let html_form_page (u: string) (uname: string) = [form ~a:[a_action (u); a_method `Post]
  [ 
    form_action_param "post";
    label ~a:[a_label_for "name"] [txt "Name "];
    input ~a:[a_name "name"; a_id "name"; a_input_type `Text; a_value uname] () ;
    p [txt "Content"];
    textarea ~a:[a_name "content" ; a_id "content" ] (txt "") ;
    br ();
    input ~a:[a_input_type `Submit ; a_value "send"] ();
  ];
  form ~a:[a_action (u); a_method `Get ]
  [ input ~a:[a_input_type `Submit ; a_value "refresh"] ();]
  ]


let format_date (t:int64) : string =  
  let f = Format.str_formatter  in
  Ptime.pp f (Option.get (Ptime.of_float_s (Int64.to_float t)));
  Format.pp_print_flush f (); 
  Format.flush_str_formatter () 

(* ?space:None ?frac_s:None ?tz_offset_s:None (Option.get (Ptime.of_float_s (Int64.to_float t)))  *)

let format_post (name: string) (content: string) (date: int64) = 
  li ~a:[a_style "display: block;" ] 
  [ p ~a:[] [strong [Unsafe.data name] ; txt " at "; txt (format_date date)] ;
    (div ~a:[a_id "replycontent"] [Unsafe.data content])] 

type post = {
  id : string;
  onuri : string;
  uname : string;
  content: string;
  date: int64;
  deleted: bool 
}

let format_post_edit (p: post) =  li
  [strong [txt p.uname] ; txt " at "; txt (format_date p.date);
    br (); (pre ~a:[] [txt p.content]);
  form ~a:[a_action (p.onuri); a_method `Post] [
    form_action_param "delete_post";
    input ~a:[a_name "post_id"; a_id "post_id"; a_input_type `Text; a_value p.id; a_hidden ()] () ;
    input ~a:[a_input_type `Submit ; a_value "delete"] ();
    ];
  (*form ~a:[a_action (p.onuri); a_method `Post] [
    form_action_param "edit";
    input ~a:[a_name "post_id"; a_id "edit_post_id"; a_input_type `Text; a_value p.id; a_hidden ()] () ;
    input ~a:[a_input_type `Submit ; a_value "edit"] ();
  ];*)
  ]
 (* li [ p ~a:[] [strong [txt name] ; txt " at "; txt (format_date date)] ; br (); (pre ~a:[] [txt content])] *)


let db_name = "database.db"


let unpack_post (r: Data.t array ) : post option = 
  let to_s s = Data.to_string_exn s in
  match r with 
  | [| iid;  uuri; name; content; date; d |] -> 
       Some ({id=(to_s iid); 
        onuri=(to_s uuri); uname=(to_s name); content=(to_s content); 
        date=(Data.to_int64_exn date); deleted=(Data.to_bool_exn d)})
  | _ -> None 


let get_post (id: string) : post option = 
  let& db = db_open db_name in
  let q = "SELECT * from comments where id = ?" in
  let s = prepare db q in 
  bind_text s 1 id |> ignore;
  step s |> ignore;
  let r = row_data s |> unpack_post in 
  step s |> Rc.check; 
  r

let get_post_list (page: string) : post list = 
  let& db = db_open db_name in
  let q = "SELECT * from comments where uri = ? and deleted = false order by time desc" in
  let s = prepare db q in 
  bind_text s 1 page |> ignore;
  let e = fold s ~f:(fun (left: post list) r -> (match (unpack_post r) with 
    | Some post -> 
        List.append left [post]
    | _ -> left)) ~init: [] in 
  match e with 
    | (Rc.DONE, e) -> e 
    | _ -> []


let get_posts (page: string) = 
  let posts : post list = get_post_list page  in
  ul ~a:[] (List.map (fun p -> format_post (p.uname) (p.content) (p.date)) posts) 

let get_posts_admin (page: string) = 
  let posts : post list = get_post_list page  in
  ul ~a:[] (List.map format_post_edit posts) 



 (* let e = fold s ~f:(fun (left: _ list) r -> (match (r : Data.t array) with 
    | [| _;  _ ; name; content; date |] -> List.append left [(format_post (Data.to_string_exn name) (Data.to_string_exn content) (Data.to_int64_exn date) )]
    | _ -> left)) ~init: [] in 
  match e with 
    | (Rc.DONE, e) -> (ul ~a:[] e)
    | _ -> (txt "error")*)


let make_resp (authorised: bool)  (page: string) (pagename: string)  (username: string) : string = 
  let posts = match authorised with 
    | false -> get_posts page
    | true -> get_posts_admin page in
 Format.asprintf "%a" (Tyxml.Html.pp ()) ( html 
  (head (title (txt pagename)) []) 
  (body (List.concat [
    (*[h2 ~a:[] [txt "Hello "; txt pagename]] ; *)
    html_form_page page username;
    [br (); hr () ; br () ];
    [posts]
  ])))

let setup_db : unit = 
  let& db = db_open db_name in
  let create_table =  "CREATE TABLE if not exists comments (id TEXT UNIQUE, uri text, user text, content text, time int64, deleted BOOL)" in
  Rc.check (exec db create_table);
  ()

  (**
     - moderaton
     - post editing
  *)

let insert_or_replace_post_now (p:post) : post = 
  let insert = "INSERT OR REPLACE INTO comments VALUES (?, ?, ?, ?, ?, ?)" in
  print_endline (Printf.sprintf "updating %s %s %s" p.onuri p.uname p.content); 
  let& db = db_open db_name in
  let s = prepare db insert in
  List.map2 (fun a b -> (bind_text s a b)) [1;2;3;4] [p.id; p.onuri; p.uname; p.content] |> ignore ; 
  bind_int64 s 5 p.date |> Rc.check;
  bind_bool s 6 p.deleted |> Rc.check;
  iter s ~f:(fun _ -> ()) |> Rc.check ;
  print_endline (Printf.sprintf "updated %s %s %s" p.onuri p.uname p.content); 
  p


let create_post_now (uri: string) (name: string) (content: string) : post = 
  let id = new_id () in
  {id=id; onuri=uri; uname=name; content=content; 
    date=(Int64.of_float (Unix.time ())); deleted=false}


let validate_post (s: string) : string = 
  Omd.escape_html_entities s |> 
  Omd.of_string |> Omd.to_html 

let submit_now (uri: string) (name: string) (content: string) : post option = 
  match (uri, name, content) with 
    | ("", _, _) -> None
    | (_, "", _) -> None
    | (_, _, "") -> None
    | _ -> 
  let name = Omd.escape_html_entities name in
  let content = validate_post content in 
  let p = create_post_now uri name content in
  Some (insert_or_replace_post_now p)

let delete_post(id: string): post option =
  print_endline "delete post";
  let p = Option.map (fun p -> {p with deleted = true}) (get_post id)  |> 
  Option.map insert_or_replace_post_now in
  match p with
    | exception _ -> None
    | s -> s


let key_authorised key: bool = 
  (compare key "kdsahflkjsahflkjhsalkfdhakjfhdsalkh") == 0


let server =
  let callback _conn req body =
    (*let meth = req |> Request.meth |> Code.string_of_method in *)
    let headers = req |> Request.headers in
    headers |> Cohttp.Header.to_string |> print_endline ;
    (*let c = Cohttp.Header.get headers "Cookie" ; *)
    let uri: Uri.t = req |> Request.uri in
    let base_uri = (Uri.with_query uri []) |> Uri.to_string in
    let pagename = (Uri.path uri) in
    let ident : (string * string) option = match (Uri.get_query_param uri "auth") with
        | Some k -> Some ("param", k)
        | None -> let cokies = Cohttp.Cookie.Cookie_hdr.extract headers in
            (List.find_map (fun f -> 
              match f with 
                | ("u", ident) ->
                    Some ("cookie", ident)
                | _ -> None
              ) cokies)
    in
    let response_header = match ident with 
      | Some ("param", i) -> Some (match (Cohttp.Cookie.Set_cookie_hdr.serialize (Cohttp.Cookie.Set_cookie_hdr.make ("u", i))) with
        | (a, b) -> Cohttp.Header.init_with a b 
      )
      | _ -> None
    in
    let authorised = match ident with 
            | Some (_, i) -> key_authorised i
            | None -> false
            in
    ( body |> Cohttp_lwt.Body.to_string >|= (fun b ->
      (match (Request.meth req) with 
        | `POST -> 
          let data = (Uri.of_string ("/?" ^ b)) in
          print_endline @@ b ;
          print_endline @@ Uri.to_string data ;
          let qry (f: string) = (match (Uri.get_query_param (data) f) with
            | Some x -> x 
            | _ -> "") in
          
          (match (authorised, (Uri.get_query_param data "action")) with 
            | _, Some "delete_post" -> (delete_post (qry "post_id")) |> ignore
            | _, Some "post" -> (submit_now base_uri (qry "name") (qry "content")) |> ignore
            | _, Some _ -> print_endline "error form bad" 
            | _, None -> ());
            ((make_resp authorised base_uri pagename (qry "name")), response_header)
        | _ -> 
            ((make_resp authorised base_uri pagename "anon"), response_header)))
         ) >>= fun r -> match r with 
          | (body, Some hdr) -> Server.respond_string ~status:`OK ~headers:hdr ~body ()
          | (body, None) -> Server.respond_string ~status:`OK ~body ()
  in
  print_endline "Serving on localhost:8000";
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = setup_db ; ignore (Lwt_main.run server)
