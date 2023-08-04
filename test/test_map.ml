open Algos.Map.Make (String)

let () =
  let map = empty in
  assert (is_empty map);
  assert (size map = 0);
  let map = map |> put "apple" 10 |> put "banana" 5 |> put "cat" (-3) |> put "dog" 0 in
  assert (size map = 4);
  assert (not (is_empty map));
  assert (get "apple" map = Some 10);
  assert (get "banana" map = Some 5);
  assert (get "cat" map = Some (-3));
  assert (get "dog" map = Some 0);
  assert (get "elephant" map = None);
  let res, map = pop "cat" map in
  assert (res = Some (-3));
  assert (get "cat" map = None);
  let res, map = pop "cat" map in
  assert (res = None);
  assert (get "dog" map = Some 0);
  let map = put "apple" 20 map in
  assert (get "apple" map = Some 20);
  assert (size map = 3)
;;

let () =
  let map = empty in
  assert (is_empty map);
  assert (size map = 0);
  let map =
    List.fold_left
      (fun m (k, v) -> put k v m)
      empty
      [ "S", 1; "E", 2; "A", 3; "R", 4; "C", 5; "H", 6; "X", 7; "M", 8; "P", 9; "L", 10 ]
  in
  assert (size map = 10);
  assert (not (is_empty map));
  assert (get "S" map = Some 1);
  assert (get "E" map = Some 2);
  assert (get "A" map = Some 3);
  assert (get "R" map = Some 4);
  assert (get "C" map = Some 5);
  assert (get "H" map = Some 6);
  assert (get "X" map = Some 7);
  assert (get "M" map = Some 8);
  assert (get "P" map = Some 9);
  assert (get "L" map = Some 10)
;;
