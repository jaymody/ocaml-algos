open Algos
module IntAvl = Avl.Make (Int)

let rec calc_height = function
  | IntAvl.Empty -> 0
  | IntAvl.Node { l; r; _ } -> 1 + max (calc_height l) (calc_height r)
;;

let () =
  let open IntAvl in
  let list = List.init 100 (fun x -> x) in
  let tree = List.fold_left (fun tree num -> add num (-num) tree) empty list in
  assert (to_list tree |> List.map fst = list);
  assert (calc_height tree < 10);
  assert (calc_height tree = height tree)
;;
