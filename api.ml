open Core.Std
open Async.Std

type register = {username: string; password: string} [@@deriving compare, sexp]
type login    = {username: string; password: string} [@@deriving compare, sexp]
type logout   = {cookie: string}                     [@@deriving compare, sexp]
type follow   = {cookie: string; username: string}   [@@deriving compare, sexp]
type post     = {cookie: string; post: string}       [@@deriving compare, sexp]
type getfeed  = {cookie: string}                     [@@deriving compare, sexp]

type request =
  | Register of register
  | Login    of login
  | Logout   of logout
  | Follow   of follow
  | Post     of post
  | Getfeed  of getfeed
[@@deriving compare, sexp]

type feed_item = {username: string; post: string} [@@deriving compare, sexp]

type response =
  | Good
  | Bad    of string
  | Cookie of string
  | Feed   of feed_item list
[@@deriving compare, sexp]

let port = 4242
