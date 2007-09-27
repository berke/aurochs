(* Treecomp *)

type ('l,'k,'op) tree =
  | Const of 'l * 'k
  | Op of 'l * 'op * ('l, 'k, 'op) tree list
;;

let l = ();;

let add x y = Op(l, "add", [x;y]);;
let k x = Const(l, x);;

let x1 = 
  Op(l, "add",
      [Op(l, "mul",
         [Op(l, "add",
            [Const(l, 123);
            add (add (add (add (k 10) (k 20)) (k 30)) (k 40)) (k 50)]);
          Op(l, "add",
             [Op(l, "not", [Const(l, 789)]);
             Const(l, 666)])]);
       Const(l,444)])
;;

let get_label = function
  | Const(l, _) -> l
  | Op(l, _, _) -> l
;;

let rec label_with_register_complexity = function
  | Const(_, k) -> Const(1, k)
  | Op(_, op, lst) ->
      let (n', arity, args) =
        List.fold_left
          begin fun (n, arity, args) t ->
            let t' = label_with_register_complexity t in
            let n' = get_label t' in
            (max n n', arity + 1, t' :: args)
          end
          (0, 0, [])
          lst
      in
      let n = n' + arity - 1 in
      Op(n, op, List.rev args)
;;

let pf = Printf.printf;;

let rec generate register = function
  | Const(_, k) -> pf "\tr%d <- %d\n" register k
  | Op(_, op, lst) ->
      let lst' =
        List.sort
          begin fun t1 t2 ->
            let n1 = get_label t1
            and n2 = get_label t2
            in
            compare n2 n1
          end
          lst
      in
      let r = ref (register - 1) in
      List.iter
        begin fun t ->
          incr r;
          generate !r t
        end
        lst';
      pf "\tr%d <- %s" register op;
      r := register;
      List.iter
        begin fun t ->
          pf " r%d" !r;
          incr r
        end
        lst';
      pf "\n"
;;
