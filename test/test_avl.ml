open Algos
module IntAvl = Avl.Make (Int)

let rec calc_height = function
  | IntAvl.Empty -> 0
  | IntAvl.Node { l; r; _ } -> 1 + max (calc_height l) (calc_height r)
;;

let rec is_height_balanced = function
  | IntAvl.Empty -> true
  | IntAvl.Node { l; r; h; _ } as node ->
    is_height_balanced l
    && is_height_balanced r
    && abs (calc_height l - calc_height r) < 2
    && h = calc_height node
;;

let () =
  let open IntAvl in
  let list = List.init 10000 (fun x -> x) in
  let tree = List.fold_left (fun tree num -> add num (-num) tree) empty list in
  assert (to_list tree |> List.map fst = list);
  assert (calc_height tree < 20);
  assert (calc_height tree = height tree);
  assert (is_height_balanced tree)
;;
