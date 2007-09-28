(* Unix_util *)

open Unix;;
open Pffpsf;;
open Util;;

(*** string_of_process_status *)
let string_of_process_status thing = function
| WEXITED(rc) -> if rc <> 0 then Some(sf "%s failed with code %d" thing rc) else None
| WSIGNALED(sg) -> Some(sf "%s exited with signal %d" thing sg)
| WSTOPPED(sg) -> Some(sf "%s stopped with signal %d" thing sg)
;;
(* ***)
(*** unix_really_read *)
let rec unix_really_read fd u o m =
  let l = read fd u o m in
  if l < m then
    if l = 0 then raise End_of_file
    else
      unix_really_read fd u (o + l) (m - l)
  else
    ()
;;
(* ***)
(*** string_of_sockaddr *)
let string_of_sockaddr =
  function
  | ADDR_UNIX x -> sf "UNIX(%S)" x
  | ADDR_INET (a, p) -> sf "%s:%d" (string_of_inet_addr a) p
;;
(* ***)
(*** sockaddr_of_string *)
let sockaddr_of_string u =
  let m = String.length u in
  if is_prefix "UNIX(" u then
    ADDR_UNIX(String.sub u 5 (m - 6))
  else
    let (a,p) = split_once_at ((=) ':') u in
    ADDR_INET(inet_addr_of_string a, int_of_string p)
;;
(* ***)
(*** int64_of_inet_addr *)
let int64_of_inet_addr ip =
  let w = string_of_inet_addr ip in
  match List.map int_of_string (split_at '.' w) with
  | [a;b;c;d] ->
      Int64.logor
        (Int64.shift_left (Int64.of_int a) 24)
        (Int64.logor
          (Int64.shift_left (Int64.of_int b) 16)
          (Int64.logor
            (Int64.shift_left (Int64.of_int c) 8)
            (Int64.of_int d)))
  | _ -> assert false
;;
(* ***)
(*** proc_get_free_mem *)
let proc_get_free_mem () =
  let ic = open_in "/proc/meminfo" in
  wind (fun () -> 
    let tot = ref 0 in
    try
      while true do
        let l = input_line ic in
        match split_at ' ' l with
        | [("MemFree:"|"Cached:");x;"kB"] -> tot :=  (int_of_string x) + !tot
        | _ -> ()
      done;
      assert false
    with
    | End_of_file -> !tot
    | _ -> 16384 (* assumption *)) ()
    (fun () -> close_in ic) ()
;;
(* ***)
(*** proc_get_rsz_vsz *)
let proc_get_rsz_vsz () =
  let ic = open_in (sf "/proc/%d/statm" (getpid ())) in
  wind (fun () -> 
    Scanf.fscanf ic "%d %d %d %d %d %d %d"
      (fun size resident share trs drs lrs dt ->
        (resident,share))) () (fun () -> close_in ic) ()
;;
(* ***)
(*** string_of_iso_8601 *)
let string_of_iso_8601 (y,m,d) = sf "%04d-%02d-%02d" y m d;;
(* ***)
(*** seconds_of_iso_8601 *)
let seconds_of_iso_8601 (y,m,d) =
  let tm = {
    tm_sec = 0;
    tm_min = 0;
    tm_hour = 0;
    tm_mon = m + 1;
    tm_wday = 0;
    tm_yday = 0;
    tm_year = y - 1900;
    tm_mday = d;
    tm_isdst = false
  }
  in
  let (t,_) = mktime tm in
  t
;;
(* ***)
(*** iso_8601_of_string *)
let iso_8601_of_string date =
  match List.map int_of_string (split_at '-' date) with
  | [y;m;d] -> (y,m,d)
  | _ -> invalid_arg "bad date"
;;
(* ***)
(*** mkdirhier *)
let mkdirhier path =
  let exists fn =
    try
      let st = stat fn in
      st.st_kind = S_DIR
    with
    | Unix_error(ENOENT,_,_) -> false
  in
  let l = split_at '/' path in
  let rec loop dir = function
    | [] -> ()
    | x::y ->
        let path = Filename.concat dir x in
        if not (exists path) then mkdir path 0o755;
        loop path y
  in
  let base = if String.length path > 1 && path.[0] = '/' then "/" else "" in
  loop base l
;;
(* ***)
(*** today *)
let today () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  (tm.Unix.tm_year + 1900, tm.Unix.tm_mon + 1, tm.Unix.tm_mday)
;;
(* ***)
