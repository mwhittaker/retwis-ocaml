open Core.Std
open Async.Std
open Protocol

(* ************************************************************************** *)
(* Types                                                                      *)
(* ************************************************************************** *)
type server_post = {
  time:     Time.t;
  username: string;
  data:     string;
} [@@deriving compare, sexp]

type user = {
  username:  string;
  password:  string;
  following: String.Set.t;
  followers: String.Set.t;
  feed:      server_post list;
} [@@deriving compare, sexp]

module Username = String
module Cookie = String
type users = user Username.Table.t
type sessions = username Cookie.Table.t

type state = {
  users:    users;
  sessions: sessions;
}

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

let get_user ({users; sessions}: state) (cookie: cookie) : user option =
  let (>>=) = Option.(>>=) in
  Cookie.Table.find sessions cookie >>= fun username ->
  Username.Table.find users username

let add_user (users: users) (user: user) : unit =
  Username.Table.set users ~key:user.username ~data:user

(* ************************************************************************** *)
(* Server                                                                     *)
(* ************************************************************************** *)
let register_impl {users; _} (username, password) =
  if String.Table.mem users username then
    return (Result.Error "Error: Username already exists")
  else
    let user = new_user username password in
    String.Table.set users ~key:username ~data:user;
    return (Result.Ok ())

let login_impl {users; sessions} (username, password) =
  match String.Table.find users username with
  | None -> return (Result.Error "Error: Username and password don't match.")
  | Some user ->
    if user.password <> password then
      return (Result.Error "Error: Username and password don't match.")
    else
      let cookie = gen_cookie () in
      String.Table.set sessions ~key:cookie ~data:username;
      return (Result.Ok cookie)

let logout_impl {sessions; _} cookie =
  String.Table.remove sessions cookie;
  return (Result.Ok ())

let follow_impl {users; sessions} (cookie, username) =
  match get_user {users; sessions} cookie, String.Table.find users username with
  | None, _ -> return (Result.Error "Error: Invalid cookie.")
  | Some _, None -> return (Result.Error "Error: Username doesn't exist.")
  | Some me, Some them ->
    if me.username = them.username ||
       String.Set.mem me.following them.username then
      return (Result.Ok ())
    else begin
      add_user users {me with
        following=String.Set.add me.following them.username;
        feed=me.feed @ them.feed
      };
      add_user users {them with
        followers=String.Set.add them.followers me.username
      };
      return (Result.Ok ())
    end

let post_impl {users; sessions} (cookie, post) =
  match get_user {users; sessions} cookie with
  | None -> return (Result.Error "Error: Invalid cookie.")
  | Some user ->
    let server_post = {
      time=Time.now ();
      username=user.username;
      data=post;
    } in
    let user = {user with feed=server_post::user.feed} in
    let follower_usernames = String.Set.to_list user.followers in
    let followers = List.map follower_usernames ~f:(fun username ->
      let user = String.Table.find_exn users username in
      {user with feed=server_post::user.feed}
    ) in
    List.iter (user::followers) ~f:(add_user users);
    return (Result.Ok ())

let getfeed_impl {users; sessions} cookie =
  match get_user {users; sessions} cookie with
  | None -> return (Result.Error "Error: Invalid cookie.")
  | Some user ->
    let server_post_cmp a b = Time.compare a.time b.time in
    let feed = List.sort ~cmp:server_post_cmp user.feed in
    add_user users {user with feed=feed};
    let feed = List.map feed ~f:(fun {username; data; _} ->
      (username, data)
    ) in
    return (Result.Ok feed)

(* ************************************************************************** *)
(* main                                                                       *)
(* ************************************************************************** *)
let main () : unit Deferred.t =
  let implementations = Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Raise
    ~implementations:[
      Rpc.Rpc.implement Protocol.register_rpc register_impl;
      Rpc.Rpc.implement Protocol.login_rpc login_impl;
      Rpc.Rpc.implement Protocol.logout_rpc logout_impl;
      Rpc.Rpc.implement Protocol.follow_rpc follow_impl;
      Rpc.Rpc.implement Protocol.post_rpc post_impl;
      Rpc.Rpc.implement Protocol.getfeed_rpc getfeed_impl;
    ]
  in
  printf "Server listening on port %d.\n" Protocol.port;
  let state = {
    users=String.Table.create ();
    sessions=String.Table.create ()
  } in
  Tcp.Server.create (Tcp.on_port Protocol.port) (fun _ r w ->
    Rpc.Connection.server_with_close
      r w
      ~on_handshake_error:`Raise
      ~connection_state:(fun _ -> state)
      ~implementations
  ) >>= Tcp.Server.close_finished

let () =
  Command.(run (async ~summary:"Retwis server." Spec.empty main))
