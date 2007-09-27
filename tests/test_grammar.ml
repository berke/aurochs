let _ =
  let b = Buffer.create 1024 in
  try
    while true do
      let l = input_line stdin in
      Buffer.add_string b l
    done
  with
  | End_of_file ->
    let u = Buffer.contents b in
    Printf.printf "%S\n%!" u;
    let t = Grammar.parse u in
    Grammar.print_tree stdout t
;;

let parsing_prefixes u =
  let m = String.length u in
  for i = 1 to m - 1 do
    let v = String.sub u 0 i in
    try
      let _t = Grammar.parse v in
      Printf.printf ">> %d\n%!" i
    with
    | _ -> ()
  done
;;
