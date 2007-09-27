(* Check_grammar *)

(* XXX: Deprecated *)

open Pffpsf;;

type 'a name =
| Resolved of string * 'a
| Unresolved of string
;;

let process ?(start="start") fn =
  let u = Driver.read_file fn in
  let m = String.length u in
  let (q, i, t) = Peg.descent (fun x -> x) Grammar.start u in
  try
    if q && i = m then
      begin
        fp stdout "GRAMMAR TREE:\n%a\n%!" (Peg.print_tree ()) t;
        let peg = Convert_grammar.convert_grammar t in
        Printf.printf "Grammar loaded from file %S, OK.\n%!" fn;
        fp stdout "GRAMMAR:\n%a\n%!" Pretty.print_grammar peg;
        let peg = Canonify.canonify_grammar peg in
        fp stdout "CANONICAL:\n%a\n%!" Pretty.print_grammar peg;
        begin
          let fn' = Filename.chop_extension fn in
          Ritchie.generate fn' ~start peg
        end;
        Convert_grammar.check_grammar start peg;
        (*let find name =
          try
            List.assoc name peg
          with
          | Not_found ->
              Printf.printf "ERROR: Can't find production named %S.\n%!" name;
              raise Not_found
        in*)
        let peg' =
          List.map
            begin fun (name, pe) ->
              (name,
                Peg.map_over_n
                  begin fun name' ->
                    ref (Unresolved name')
                  end
                  pe)
            end
            peg
        in
        let resolve r =
          match !r with
          | Resolved(_, x) -> x
          | Unresolved name -> failwith (sf "Unresolved non-terminal %S" name)
        in
        List.iter
          begin fun (_, pe') ->
            Peg.iter_over_n
              begin fun r ->
                match !r with
                | Resolved(_, _) -> ()
                | Unresolved name' -> r := Resolved(name', List.assoc name' peg')
              end
              pe'
          end
          peg';
        let start = List.assoc "start" peg' in
        (*if false then
          begin
            let fn2 = Sys.argv.(2) in
            let v = Driver.read_file fn2 in
            try
              let (q, i, t) = Peg.descent find (find start) v in
              fp stdout "%b %d\n%a\n" q i (Peg.print_tree ~depth:0) t
            with
            | Peg.Fail ->
                fp stdout "Does not parse.\n"
          end
        else*)
          begin
            try
              let count = ref 0 in
              while true do
                incr count;
                let v = input_line stdin in
                try
                  let (q, i, t) = Peg.descent resolve start v in
                  (*fp stdout ">>> %s\n" v;
                  fp stdout "%b %d\n%a\n" q i (Peg.print_tree ()) t*)
                  if q then
                    begin
                      fp stdout "%05d RESULT OK\n" !count;
                      Peg.print_tree () stdout (Peg.Node("Root",[],[t]))
                    end
                  else
                    fp stdout "%05d RESULT PREFIX %d\n" !count i
                with
                | Peg.Fail ->
                    fp stdout "%05d RESULT NOPREFIX\n" !count
              done
            with
            | End_of_file ->
                Printf.printf "EOF\n"
          end
      end
    else
      begin
        Printf.printf "ERROR - Parse error at character %d.\n" (i + 1);
        raise Peg.Fail
      end
  with
  | Peg.Fail ->
      Printf.printf "ERROR - Cannot parse input grammar.\n"
;;

let _ =
  let fn = Sys.argv.(1) in
  process ~start:Sys.argv.(2) fn
;;
