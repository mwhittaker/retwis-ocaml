open Core.Std
open Async.Std
open Protocol

let register = Rpc.Rpc.dispatch_exn register_rpc
let login = Rpc.Rpc.dispatch_exn login_rpc
let logout = Rpc.Rpc.dispatch_exn logout_rpc
let follow = Rpc.Rpc.dispatch_exn follow_rpc
let post = Rpc.Rpc.dispatch_exn post_rpc
let getfeed = Rpc.Rpc.dispatch_exn getfeed_rpc

let print_feed (feed: (username * post) list) : unit =
  List.map feed ~f:(fun (username, post) -> sprintf "%s\n%s" username post)
  |> String.concat ~sep:"\n\n"
  |> print_endline

let repl (conn: Rpc.Connection.t) : unit Deferred.t =
  let stdin = Lazy.force Reader.stdin in
  Pipe.fold (Reader.lines stdin) ~init:None ~f:(fun cookie line ->
    match (String.split line ~on:' ', cookie) with
    | ["register"; username; password], _ -> begin
      register conn (username, password) >>= function
      | Result.Error msg -> (print_endline msg; return cookie)
      | Result.Ok () -> (print_endline "OK"; return cookie)
    end
    | ["login"; username; password], _ -> begin
      login conn (username, password) >>= function
      | Result.Error msg -> (print_endline msg; return cookie)
      | Result.Ok cookie -> (print_endline "OK"; return (Some cookie))
    end
    | ["logout"], Some cookie -> begin
      logout conn cookie >>= function
      | Result.Error msg -> (print_endline msg; return (Some cookie))
      | Result.Ok () -> (print_endline "OK"; return None)
    end
    | ["follow"; username], Some cookie -> begin
      follow conn (cookie, username) >>= function
      | Result.Error msg -> (print_endline msg; return (Some cookie))
      | Result.Ok () -> (print_endline "OK"; return (Some cookie))
    end
    | "post"::msgs, Some cookie -> begin
      post conn (cookie, String.concat ~sep:" " msgs) >>= function
      | Result.Error msg -> (print_endline msg; return (Some cookie))
      | Result.Ok () -> (print_endline "OK"; return (Some cookie))
    end
    | ["getfeed"], Some cookie -> begin
      getfeed conn cookie >>= function
      | Result.Error msg -> (print_endline msg; return (Some cookie))
      | Result.Ok feed -> (print_feed feed; return (Some cookie))
    end
    | ["logout"], None
    | ["follow"; _], None
    | "post"::_, None
    | ["getfeed"], None -> begin
      print_endline "Error: you must log in first.%s\n";
      return cookie
    end
    | _ -> begin
      printf "Error: unrecognized command %s\n" line;
      return cookie
    end
  ) >>= fun _ ->
  return ()

let main () : unit Deferred.t =
  let where_to_connect = Tcp.to_host_and_port "localhost" Protocol.port in
  Tcp.with_connection where_to_connect (fun _ r w ->
    Rpc.Connection.create r w ~connection_state:(fun _ -> ()) >>= function
    | Result.Error exn -> raise exn
    | Result.Ok conn -> repl conn
  )

let () =
  Command.(run (async ~summary:"Retwis client." Spec.empty main))
