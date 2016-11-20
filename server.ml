open Core.Std
open Async.Std

(* ************************************************************************** *)
(* Types                                                                      *)
(* ************************************************************************** *)
type server_feed_item = {
  time: Time.t;
  username: string;
  data: string;
} [@@deriving compare, sexp]

type user = {
  username:  string;
  password:  string;
  following: String.Set.t;
  followers: String.Set.t;
  feed:      server_feed_item list;
} [@@deriving compare, sexp]

module Username = String
module Cookie = String
type username = string
type cookie = string
type users = user Username.Map.t
type sessions = username Cookie.Map.t

(* ************************************************************************** *)
(* Helpers                                                                    *)
(* ************************************************************************** *)
let gen_cookie () : cookie =
  List.init 64 ~f:(fun _ -> Random.int 10)
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:""

let new_user (username: string) (password: string) : user =
  {
    username;
    password;
    following=String.Set.empty;
    followers=String.Set.empty;
    feed=[];
  }

let get_user (users: users) (sessions: sessions) (cookie: cookie)
             : user option =
  let (>>=) = Option.(>>=) in
  Cookie.Map.find sessions cookie >>= fun username ->
  Username.Map.find users username

let add_user (users: users) (user: user) : users =
  Username.Map.add users ~key:user.username ~data:user

let server_feed_item_cmp (a: server_feed_item) (b: server_feed_item) : int =
  Time.compare a.time b.time

(* ************************************************************************** *)
(* Server                                                                     *)
(* ************************************************************************** *)
let eval (req: Api.request) (users: users) (sessions: sessions)
         : Api.response * users * sessions =
  let open Api in
  match req with
  | Register {username; password} -> begin
    if String.Map.mem users username then
      (Bad "Username already exists", users, sessions)
    else
      let user = new_user username password in
      let users = String.Map.add users ~key:username ~data:user in
      (Good, users, sessions)
  end
  | Login {username; password} -> begin
    match String.Map.find users username with
    | None -> (Bad "Username and password don't match.", users, sessions)
    | Some user ->
      if user.password <> password then
        (Bad "Username and password don't match.", users, sessions)
      else
        let cookie = gen_cookie () in
        let sessions = String.Map.add sessions ~key:cookie ~data:username in
        (Cookie cookie, users, sessions)
  end
  | Logout {cookie} -> (Good, users, String.Map.remove sessions cookie)
  | Follow {cookie; username} -> begin
    match get_user users sessions cookie, String.Map.find users username with
    | None, _ -> (Bad "Invalid cookie.", users, sessions)
    | Some _, None -> (Bad "Username doesn't exist.", users, sessions)
    | Some me, Some them ->
      if me.username = them.username ||
         String.Set.mem me.following them.username then
        (Good, users, sessions)
      else
        let me = {me with
          following=String.Set.add me.following them.username;
          feed=me.feed @ them.feed
        } in
        let them = {them with
          followers=String.Set.add them.followers me.username
        } in
        let users = add_user users me in
        let users = add_user users them in
        (Good, users, sessions)
  end
  | Post {cookie; post} -> begin
    match get_user users sessions cookie with
    | None -> (Bad "Invalid cookie.", users, sessions)
    | Some me ->
      let feed_item = {
        time=Time.now ();
        username=me.username;
        data=post;
      } in
      let me = {me with feed=feed_item::me.feed} in
      let follower_usernames = String.Set.to_list me.followers in
      let followers = List.map follower_usernames ~f:(fun username ->
        let user = String.Map.find_exn users username in
        {user with feed=feed_item::user.feed}
      ) in
      let users = List.fold_left (me::followers) ~init:users ~f:add_user in
      (Good, users, sessions)
  end
  | Getfeed {cookie} -> begin
    match get_user users sessions cookie with
    | None -> (Bad "Invalid cookie.", users, sessions)
    | Some user ->
        let feed = List.sort server_feed_item_cmp user.feed in
        let users = add_user users {user with feed=feed} in
        let feed = List.map feed ~f:(fun {username; data; _} ->
          {username; post=data}
        ) in
        (Feed feed, users, sessions)
  end

let rec server (r: (Api.request * Api.response Pipe.Writer.t) Pipe.Reader.t)
               (users: users)
               (sessions: sessions)
               : unit Deferred.t =
  let open Api in
  Pipe.read r >>= function
  | `Eof -> failwith "Internal pipe error."
  | `Ok (req, w) -> begin
    let (resp, users, sessions) = eval req users sessions in
    don't_wait_for (Pipe.write w resp);
    server r users sessions
  end

let serve _
          (r: Reader.t)
          (w: Writer.t)
          (server_w: (Api.request * Api.response Pipe.Writer.t) Pipe.Writer.t)
          : unit Deferred.t =
  let (pipe_r, pipe_w) = Pipe.create () in
  Pipe.iter (Reader.read_sexps r) ~f:(fun sexp ->
    Pipe.write server_w (Api.request_of_sexp sexp, pipe_w) >>= fun () ->
    Pipe.read pipe_r >>= function
      | `Eof -> failwith "Server pipe failed."
      | `Ok resp -> begin
        Writer.write_sexp w (Api.sexp_of_response resp);
        return ()
      end
  )

let main () : unit Deferred.t =
  let (server_r, server_w) = Pipe.create () in
  don't_wait_for (server server_r String.Map.empty String.Map.empty);
  printf "Server listening on port %d\n." Api.port;
  ignore (Tcp.Server.create (Tcp.on_port Api.port) (fun a r w ->
    serve a r w server_w
  ));
  never ()

let () =
  Command.(run (async ~summary:"" Spec.empty main))
