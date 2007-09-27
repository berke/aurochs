(* Parse 1 *)

type 'v result =
  | Parsed of 'v * char list (* Semantic value, rest *)
  | No_parse
;;

let rec p_additive s =
  let alt2 () =
    match p_multitive s with
    | Parsed(v, s1) -> Parsed(v, s1)
    | No_parse -> No_parse
  in
  match p_multitive s with
  | Parsed(v_left, s1) ->
      begin
        match s' with
        | '+' :: s2 ->
            begin
              match p_additive s2 with
              | Parsed(v_right, s3) -> Parsed(v_left + v_right, s3)
              | _ -> alt2 ()
            end
        | _ -> alt2 ()
      end
  | _ -> alt2 ()
and p_multitive s =

;;
