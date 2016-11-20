open Core.Std
open Async.Std

let print_feed (feed: Api.feed_item list) : unit =
  let open Api in
  List.map feed ~f:(fun {username; post} -> sprintf "%s\n%s" username post)
  |> String.concat ~sep:"\n\n"
  |> print_endline

let eval (req: Api.request) (r: Reader.t) (w: Writer.t)
         : string option Deferred.t =
  let open Api in
  Writer.write_sexp w (sexp_of_request req);
  Reader.read_sexp r >>= function
  | `Eof -> failwith "Connection with server closed."
  | `Ok sexp -> begin
    match req, response_of_sexp sexp with
    | Register _, Good -> begin
      print_endline "OK";
      return None
    end
    | Login _, Cookie cookie
    | Logout   {cookie; _}, Good
    | Follow   {cookie; _}, Good
    | Post     {cookie; _}, Good -> begin
      print_endline "OK";
      return (Some cookie)
    end
    | Getfeed {cookie; _}, Feed feed -> begin
      print_feed feed;
      return (Some cookie)
    end

    | Register _, Bad msg
    | Login _, Bad msg -> begin
      print_endline msg;
      return None
    end
    | Logout   {cookie; _}, Bad msg
    | Follow   {cookie; _}, Bad msg
    | Post     {cookie; _}, Bad msg
    | Getfeed  {cookie; _}, Bad msg -> begin
      print_endline msg;
      return (Some cookie)
    end

    | _, _ -> failwith "Unexpected response."
  end

let connect _ (r: Reader.t) (w: Writer.t) : unit Deferred.t =
  let stdin  = Lazy.force Reader.stdin in
  Pipe.fold (Reader.lines stdin) ~init:None ~f:(fun cookie line ->
    match (String.split line ~on:' ', cookie) with
    | ["register"; username; password], _ ->
        eval Api.(Register {username; password}) r w
    | ["login"; username; password], _ ->
        eval Api.(Login {username; password}) r w
    | ["logout"], Some cookie ->
        eval Api.(Logout {cookie}) r w
    | ["follow"; username], Some cookie ->
        eval Api.(Follow {cookie; username}) r w
    | "post"::post, Some cookie ->
        eval Api.(Post {cookie; post=String.concat ~sep:" " post}) r w
    | ["getfeed"], Some cookie ->
        eval Api.(Getfeed {cookie}) r w
    | ["logout"], None
    | ["follow"; _], None
    | "post"::_, None
    | ["getfeed"], None -> begin
      printf "Error: you must log in first.%s\n" "";
      return cookie
    end
    | _ -> begin
      printf "Error: unrecognized command %s\n" line;
      return cookie
    end
  ) >>= fun _ ->
  return ()

let main () : unit Deferred.t =
  Tcp.with_connection (Tcp.to_host_and_port "localhost" Api.port) connect

let () =
  Command.(run (async ~summary:"Command line Retwis client." Spec.empty main))
