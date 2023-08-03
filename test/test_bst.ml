open Algos
module StringBst = Bst.Make (String)

let () =
  let open StringBst in
  let tree = empty |> add "banana" 5 |> add "apple" 10 |> add "dog" 0 |> add "cat" (-3) in
  assert (find "apple" tree = Some 10);
  assert (find "banana" tree = Some 5);
  assert (find "cat" tree = Some (-3));
  assert (find "dog" tree = Some 0);
  assert (find "elephant" tree = None);
  let res, tree = remove "cat" tree in
  assert (res = Some (-3));
  assert (find "cat" tree = None);
  let res, tree = remove "cat" tree in
  assert (res = None);
  assert (find "dog" tree = Some 0);
  let tree = add "apple" 20 tree in
  assert (find "apple" tree = Some 20)
;;

module IntBst = Bst.Make (Int)

let () =
  let open IntBst in
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
