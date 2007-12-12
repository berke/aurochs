(* Cgi *)

open Pffpsf

(* Functions processing environment vars *)

type invocation = POST | GET

let invocation_method () =
  try
    match Sys.getenv ("REQUEST_METHOD") with
    | "POST" -> POST
    | "GET" -> GET
    | m -> invalid_arg (sf "Unknown invocation method %s" m)
  with
    Not_found -> GET (* Default value *)

let remote_host =
  let g x =
    try
      Sys.getenv x
    with
      Not_found -> ""
  and f x y = match (x,y) with
    "",_ -> y
  | _,"" -> x
  | _,_ -> x^","^y
  in
  f (g "REMOTE_HOST")
    (f (g "REMOTE_USER")
       (g "REMOTE_ADDR"))

let reply_html f =
  fp stdout "Content-type: text/html\n";
  fp stdout "\n";
  f stdout
