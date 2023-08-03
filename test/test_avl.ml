open Algos
module StringAvl = Avl.Make (String)
module IntAvl = Avl.Make (Int)

let rec calc_height = function
  | IntAvl.Empty -> 0
  | IntAvl.Node (l, _, _, r, _) -> 1 + max (calc_height l) (calc_height r)
;;

let rec is_height_balanced = function
  | IntAvl.Empty -> true
  | IntAvl.Node (l, _, _, r, h) as node ->
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
  assert (is_height_balanced tree);
  let list = List.init (10000 - 5000) (fun x -> x) in
  let tree = List.fold_left (fun tree num -> snd (remove num tree)) tree list in
  assert (to_list tree |> List.map (fun (num, _) -> num - 5000) = list);
  assert (calc_height tree < 15);
  assert (is_height_balanced tree)
;;

(* Same tests as BST *)

let () =
  let open StringAvl in
  let tree = empty |> add "banana" 5 |> add "apple" 10 |> add "dog" 0 |> add "cat" (-3) in
  assert (find "apple" tree = Some 10);
  assert (find "banana" tree = Some 5);
  assert (find "cat" tree = Some (-3));
  assert (find "dog" tree = Some 0);
  assert (find "elephant" tree = None);
  let res, tree = remove "banana" tree in
  assert (res = Some 5);
  assert (find "banana" tree = None);
  let res, tree = remove "banana" tree in
  assert (res = None);
  assert (find "dog" tree = Some 0);
  let tree = add "apple" 20 tree in
  assert (find "apple" tree = Some 20)
;;

let () =
  let open IntAvl in
  let tree =
    empty
    |> add 5 (-5)
    |> add 2 (-2)
    |> add 3 100000
    |> add 1 (-1)
    |> add 0 0
    |> add 10 (-10)
    |> add 11 (-11)
    |> add 12 (-12)
    |> add 8 (-8)
    |> add 7 (-7)
    |> add 3 (-3)
    |> add 4 (-4)
    |> add 6 (-6)
    |> add 9 (-9)
  in
  assert (to_list tree |> List.map fst = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ]);
  let res, tree = remove 12 tree in
  assert (Option.is_some res);
  let res, tree = remove 5 tree in
  assert (Option.is_some res);
  let res, tree = remove 8 tree in
  assert (Option.is_some res);
  let res, tree = remove 0 tree in
  assert (Option.is_some res);
  let res, tree = remove 3 tree in
  assert (Option.is_some res);
  assert (to_list tree |> List.map fst = [ 1; 2; 4; 6; 7; 9; 10; 11 ])
;;
