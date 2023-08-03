open Algos
module IntAVL = Avl.Make (Int)

let () =
  let open IntAVL in
  let list = List.init 100 (fun x -> x) in
  let tree = List.fold_left (fun tree num -> add num (-num) tree) empty list in
  assert (to_list tree |> List.map fst = list)
;;
