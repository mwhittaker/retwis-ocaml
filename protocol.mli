open Core.Std
open Async.Std

type username = string [@@deriving bin_io, compare, sexp]
type password = string [@@deriving bin_io, compare, sexp]
type cookie = string [@@deriving bin_io, compare, sexp]
type post = string [@@deriving bin_io, compare, sexp]
type feed = (username * post) list [@@deriving bin_io, compare, sexp]
type 'ok result = ('ok, string) Result.t [@@deriving bin_io, compare, sexp]

val register_rpc : (username * password, unit result)   Rpc.Rpc.t
val login_rpc    : (username * password, cookie result) Rpc.Rpc.t
val logout_rpc   : (cookie,              unit result)   Rpc.Rpc.t
val follow_rpc   : (cookie * username,   unit result)   Rpc.Rpc.t
val post_rpc     : (cookie * post,       unit result)   Rpc.Rpc.t
val getfeed_rpc  : (cookie,              feed result)   Rpc.Rpc.t

val port: int
