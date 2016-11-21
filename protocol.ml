open Core.Std
open Async.Std

type username = string [@@deriving bin_io, compare, sexp]
type password = string [@@deriving bin_io, compare, sexp]
type cookie = string [@@deriving bin_io, compare, sexp]
type post = string [@@deriving bin_io, compare, sexp]
type feed = (username * post) list [@@deriving bin_io, compare, sexp]
type 'ok result = ('ok, string) Result.t [@@deriving bin_io, compare, sexp]

module StringStringPair = Tuple.Binable(String)(String)

let register_rpc =
  Rpc.Rpc.create
    ~name:"register"
    ~version:1
    ~bin_query:StringStringPair.bin_t
    ~bin_response:(Result.bin_t Unit.bin_t String.bin_t)

let login_rpc =
  Rpc.Rpc.create
    ~name:"login"
    ~version:1
    ~bin_query:StringStringPair.bin_t
    ~bin_response:(Result.bin_t String.bin_t String.bin_t)

let logout_rpc =
  Rpc.Rpc.create
    ~name:"logout"
    ~version:1
    ~bin_query:String.bin_t
    ~bin_response:(Result.bin_t Unit.bin_t String.bin_t)

let follow_rpc =
  Rpc.Rpc.create
    ~name:"follow"
    ~version:1
    ~bin_query:StringStringPair.bin_t
    ~bin_response:(Result.bin_t Unit.bin_t String.bin_t)

let post_rpc =
  Rpc.Rpc.create
    ~name:"post"
    ~version:1
    ~bin_query:StringStringPair.bin_t
    ~bin_response:(Result.bin_t Unit.bin_t String.bin_t)

let getfeed_rpc =
  Rpc.Rpc.create
    ~name:"getfeed"
    ~version:1
    ~bin_query:String.bin_t
    ~bin_response:(Result.bin_t bin_feed String.bin_t)

let port = 4242
