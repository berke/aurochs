let generate_word m =
  for i = 0 to m - 1 do
    output_char stdout (Char.chr (Random.int 26 + 97))
  done
;;

let r r0 r1 =
  if r0 = r1 then
   r0
  else
   r0 + (Random.int (r1 - r0));;

let pf = Printf.printf;;

let rec generate k =
  if k = 0 then
    begin
      generate_word (r 5 20); pf " "
    end
  else
    match r 0 20 with
    | 0 -> pf " "; generate_word (r 5 20); pf " "
    | 1 ->
      pf "(";
      generate (k - 1);
      pf ")"
    | 2 ->
      pf "(";
      generate (k - 1);
      pf " AND ";
      generate (k - 1);
      pf ")"
    | 3 ->
      pf "(";
      generate (k - 1);
      pf " OR ";
      generate (k - 1);
      pf ")"
    | 4 ->
      pf "(";
      generate (k - 1);
      begin
        match r 0 1 with
        | 0 -> pf " NEAR "
        | _ -> pf " NEAR/%d " (r 0 10000)
      end;
      generate (k - 1);
      pf ")"
    | 5 ->
      pf "(NOT (";
      generate (k - 1);
      pf "))"
    | 6 ->
        generate_word (r 2 15);
        pf ":(";
        generate (k - 1);
        pf ")";
    | _ ->
        for i = 1 to r 1 (max 1 (k - 1)) do
          generate (k - 1);
          pf " "
        done
;;

let _ =
  for i = 1 to 2000 do
    let n = Random.int 15 in
    generate n;
    Printf.printf "\n"
  done
;;
