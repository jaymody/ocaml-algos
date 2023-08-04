open Algos
module StringAvl = Avl.Make (String)
module IntAvl = Avl.Make (Int)

let rec calc_height = function
  | IntAvl.Empty -> 0
  | IntAvl.Node (l, _, _, r, _, _) -> 1 + max (calc_height l) (calc_height r)
;;

let rec calc_size = function
  | IntAvl.Empty -> 0
  | IntAvl.Node (l, _, _, r, _, _) -> 1 + calc_size l + calc_size r
;;

let rec is_height_balanced = function
  | IntAvl.Empty -> true
  | IntAvl.Node (l, _, _, r, h, n) as node ->
    is_height_balanced l
    && is_height_balanced r
    && abs (calc_height l - calc_height r) < 2
    && h = calc_height node
    && n = calc_size node
;;

let () =
  let open IntAvl in
  let list = List.init 10000 (fun x -> x) in
  let tree = List.fold_left (fun tree num -> upsert num (-num) tree) empty list in
  assert (size tree = 10000);
  assert (to_list tree |> List.map fst = list);
  assert (calc_height tree < 20);
  assert (is_height_balanced tree);
  let list = List.init (10000 - 5000) (fun x -> x) in
  let tree = List.fold_left (fun tree num -> snd (remove num tree)) tree list in
  assert (size tree = 5000);
  assert (to_list tree |> List.map (fun (num, _) -> num - 5000) = list);
  assert (calc_height tree < 15);
  assert (is_height_balanced tree)
;;

(* Same tests as BST *)

let () =
  let open StringAvl in
  let tree =
    empty |> upsert "banana" 5 |> upsert "apple" 10 |> upsert "dog" 0 |> upsert "cat" (-3)
  in
  assert (size tree = 4);
  assert (find "apple" tree = Some 10);
  assert (find "banana" tree = Some 5);
  assert (find "cat" tree = Some (-3));
  assert (find "dog" tree = Some 0);
  assert (find "elephant" tree = None);
  let res, tree = remove "banana" tree in
  assert (res = Some 5);
  assert (size tree = 3);
  assert (find "banana" tree = None);
  let res, tree = remove "banana" tree in
  assert (res = None);
  assert (size tree = 3);
  assert (find "dog" tree = Some 0);
  let tree = upsert "apple" 20 tree in
  assert (find "apple" tree = Some 20)
;;

let () =
  let open IntAvl in
  let tree =
    empty
    |> upsert 5 (-5)
    |> upsert 2 (-2)
    |> upsert 3 100000
    |> upsert 1 (-1)
    |> upsert 0 0
    |> upsert 10 (-10)
    |> upsert 11 (-11)
    |> upsert 12 (-12)
    |> upsert 8 (-8)
    |> upsert 7 (-7)
    |> upsert 3 (-3)
    |> upsert 4 (-4)
    |> upsert 6 (-6)
    |> upsert 9 (-9)
  in
  assert (size tree = 13);
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
  assert (to_list tree |> List.map fst = [ 1; 2; 4; 6; 7; 9; 10; 11 ]);
  assert (size tree = 8)
;;

let () =
  let open IntAvl in
  let tree =
    empty
    |> insert 1 (-1)
    |> insert 2 (-2)
    |> insert 3 (-3)
    |> insert 3 (-30)
    |> insert 4 (-4)
    |> insert 4 4
    |> insert 4 40
    |> insert 4 400
  in
  let _, tree = remove 4 tree in
  assert (to_list tree |> List.map fst = [ 1; 2; 3; 3; 4; 4; 4 ])
;;
