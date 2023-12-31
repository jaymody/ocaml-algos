open Algos
module StringAvl = Avl.MakeKV (String)
module IntAvl = Avl.MakeKV (Int)

(* test basics via String keys *)
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
  let res, tree = pop "banana" tree in
  assert (res = Some 5);
  assert (size tree = 3);
  assert (find "banana" tree = None);
  let res, tree = pop "banana" tree in
  assert (res = None);
  assert (size tree = 3);
  assert (find "dog" tree = Some 0);
  let tree = upsert "apple" 20 tree in
  assert (find "apple" tree = Some 20)
;;

(* test basics via Int keys *)
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
  let res, tree = pop 12 tree in
  assert (Option.is_some res);
  let res, tree = pop 5 tree in
  assert (Option.is_some res);
  let res, tree = pop 8 tree in
  assert (Option.is_some res);
  let res, tree = pop 0 tree in
  assert (Option.is_some res);
  let res, tree = pop 3 tree in
  assert (Option.is_some res);
  assert (to_list tree |> List.map fst = [ 1; 2; 4; 6; 7; 9; 10; 11 ]);
  assert (size tree = 8)
;;

(* test insert *)
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
  let _, tree = pop 4 tree in
  assert (to_list tree |> List.map fst = [ 1; 2; 3; 3; 4; 4; 4 ])
;;

(* test scaling *)
let () =
  let open Avl.Make (Int) in
  let list = List.init 10000 (fun x -> x) in
  let tree = List.fold_left (fun tree num -> upsert num (-num) tree) empty list in
  assert (size tree = 10000);
  assert (to_list tree |> List.map fst = list);
  assert (calc_height tree < 20);
  assert (is_height_balanced tree);
  let list = List.init (10000 - 5000) (fun x -> x) in
  let tree = List.fold_left (fun tree num -> snd (pop num tree)) tree list in
  assert (size tree = 5000);
  assert (to_list tree |> List.map (fun (num, _) -> num - 5000) = list);
  assert (calc_height tree < 15);
  assert (is_height_balanced tree)
;;

(* test min max stuff *)
let () =
  let open IntAvl in
  let tree =
    empty
    |> upsert 4 0
    |> upsert 1 0
    |> upsert 6 0
    |> upsert 2 0
    |> upsert 3 0
    |> upsert 5 0
    |> upsert 7 0
  in
  let mn, tree = pop_min tree in
  assert (mn = Some (1, 0));
  assert (find 1 tree = None);
  assert (to_list tree |> List.map fst = [ 2; 3; 4; 5; 6; 7 ]);
  let mn, tree = pop_min tree in
  assert (mn = Some (2, 0));
  let mx, tree = pop_max tree in
  assert (mx = Some (7, 0));
  let mn, tree = pop_min tree in
  assert (mn = Some (3, 0));
  let mx, tree = pop_max tree in
  assert (mx = Some (6, 0));
  assert (to_list tree |> List.map fst = [ 4; 5 ]);
  assert (pop_min (empty |> upsert 10 0) = (Some (10, 0), empty));
  assert (pop_max (empty |> upsert 10 0) = (Some (10, 0), empty));
  assert (pop_min empty = (None, empty));
  assert (pop_max empty = (None, empty))
;;

(* test with a custom key module *)
module CustomKey = struct
  type t = string * int

  let compare (_, a) (_, b) = compare a b
end

let () =
  let open Avl.Make (CustomKey) in
  let tree =
    empty
    |> upsert ("a", 5) "old"
    |> upsert ("b", 2) "old"
    |> upsert ("c", 7) "old"
    |> upsert ("d", 2) "new"
    |> upsert ("e", 1) "old"
  in
  assert (
    to_list tree = [ ("e", 1), "old"; ("b", 2), "new"; ("a", 5), "old"; ("c", 7), "old" ])
;;

(* test MakeJustV variant *)
let () =
  let open Avl.MakeJustV (Int) in
  let tree =
    empty
    |> insert 5
    |> upsert 5
    |> insert 5
    |> insert 2
    |> insert 1
    |> upsert 3
    |> insert 4
    |> insert 4
    |> upsert 1
    |> insert 1
  in
  assert (to_list tree = [ 1; 1; 2; 3; 4; 4; 5; 5 ]);
  assert (get_min tree = Some 1);
  assert (get_max tree = Some 5);
  let res, tree = pop_min tree in
  assert (res = Some 1);
  let res, tree = pop_min tree in
  assert (res = Some 1);
  let res, tree = pop_min tree in
  assert (res = Some 2);
  let res, tree = remove 3 tree in
  assert res;
  assert (mem 4 tree);
  let res, tree = pop_min tree in
  assert (res = Some 4);
  assert (mem 4 tree);
  let res, tree = remove 5 tree in
  assert res;
  let res, tree = pop_max tree in
  assert (res = Some 5);
  let res, tree = pop_max tree in
  assert (res = Some 4);
  assert (not (mem 4 tree));
  let res, _ = pop_max tree in
  assert (res = None)
;;
