open Algos.List;;

(* cons tests *)
assert (cons 0 [] = [ 0 ]);;
assert (cons 1 (cons 0 []) = [ 1; 0 ]);;
assert (cons 0 [ 0; 0 ] = [ 0; 0; 0 ]);;

(* append tests *)
assert (append [] [] = []);;
assert (append [ 0; 1 ] [] = [ 0; 1 ]);;
assert (append [] [ 0; 1 ] = [ 0; 1 ]);;
assert (append [ 0; 1; 2 ] [ 3; 4 ] = [ 0; 1; 2; 3; 4 ]);;

(* hd tests *)
assert (hd [] = None);;
assert (hd [ 0 ] = Some 0);;
assert (hd [ 0; 1 ] = Some 0);;

(* tl tests *)
assert (tl [] = None);;
assert (tl [ 0 ] = Some []);;
assert (tl [ 0; 1 ] = Some [ 1 ]);;
assert (tl [ 0; 1; 2 ] = Some [ 1; 2 ]);;

(* length tests *)
assert (length [] = 0);;
assert (length [ 1 ] = 1);;
assert (length [ 1; 2 ] = 2);;
assert (length [ 1; 6; 2 ] = 3);;
assert (length [ 8; 1; 3; 4 ] = 4);;
assert (length [ 0; 0; 0; 0 ] = 4);;
assert (length [ 4; 1; 1; -4 ] = 4);;

(* nth tests *)
assert (nth [] 0 = None);;
assert (nth [] 1 = None);;
assert (nth [ 1 ] 0 = Some 1);;
assert (nth [ 1 ] 1 = None);;
assert (nth [ 1; 5 ] 0 = Some 1);;
assert (nth [ 1; 5 ] 1 = Some 5);;
assert (nth [ 1; 5 ] 2 = None);;
assert (nth [ 1; 5; 10; -5 ] 0 = Some 1);;
assert (nth [ 1; 5; 10; -5 ] 1 = Some 5);;
assert (nth [ 1; 5; 10; -5 ] 2 = Some 10);;
assert (nth [ 1; 5; 10; -5 ] 3 = Some (-5));;
assert (nth [ 1; 5; 10; -5 ] 4 = None);;
assert (nth [ 1; 5; 10; -5 ] 5 = None);;
assert (nth [ 1; 5; 10; -5 ] 100 = None);;

(* rev tests *)
assert (rev [] = []);;
assert (rev [ 1 ] = [ 1 ]);;
assert (rev [ 1; 2 ] = [ 2; 1 ]);;
assert (rev [ 1; 1 ] = [ 1; 1 ]);;
assert (rev [ 1; 6; 2 ] = [ 2; 6; 1 ]);;
assert (rev [ 8; 1; 3; 4 ] = [ 4; 3; 1; 8 ]);;
assert (rev [ 0; 0; 0; 0 ] = [ 0; 0; 0; 0 ]);;
assert (rev [ 4; 1; 1; -4 ] = [ -4; 1; 1; 4 ]);;

(* concat tests *)
assert (concat [] = []);;
assert (concat [ [] ] = []);;
assert (concat [ []; [] ] = []);;
assert (concat [ [ 1; 2; 3 ]; [ 5; 4 ]; [ -10 ] ] = [ 1; 2; 3; 5; 4; -10 ]);;

(* init tests *)
assert (init 0 (fun x -> x) = []);;
assert (init 1 (fun x -> x) = [ 0 ]);;
assert (init 5 (fun x -> x) = [ 0; 1; 2; 3; 4 ]);;
assert (init 5 (fun x -> -x * x) = [ 0; -1; -4; -9; -16 ]);;

(* map tests *)
assert (map (fun x -> -x * x) [] = []);;
assert (map (fun x -> -x * x) [ 0; 1; 2; 3; 4 ] = [ 0; -1; -4; -9; -16 ]);;
assert (map (fun _ -> [ 1 ]) [ 0; 1; 2; 3; 4 ] = [ [ 1 ]; [ 1 ]; [ 1 ]; [ 1 ]; [ 1 ] ]);;

(* fold_left tests *)
assert (fold_left (fun _ x -> x) 10 [] = 10);;
assert (fold_left (fun acc x -> acc + x) 10 [] = 10);;
assert (fold_left (fun acc x -> acc + x) 10 [ 1; 2; 3; 4; 5 ] = 25);;
assert (fold_left (fun acc x -> acc / x) (100 * 100) [ 100; 10; 2 ] = 5);;
assert (fold_left (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ] = [ 5; 4; 3; 2; 1 ]);;

(* fold_right tests *)
assert (fold_right (fun _ x -> x) 10 [] = 10);;
assert (fold_right (fun acc x -> acc + x) 10 [] = 10);;
assert (fold_right (fun acc x -> acc + x) 10 [ 1; 2; 3; 4; 5 ] = 25);;
assert (fold_left (fun acc x -> acc / x) (100 * 100) [ 2; 10; 100 ] = 5);;
assert (fold_right (fun acc x -> x :: acc) [] [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]);;

(* for_all tests *)
assert (for_all (fun _ -> false) [] = true);;
assert (for_all (fun _ -> false) [ 0 ] = false);;
assert (for_all (fun x -> x mod 2 = 0) [ 0; 2; 4; 6 ] = true);;
assert (for_all (fun x -> x mod 2 = 0) [ 0; 2; 3; 6 ] = false);;

(* exists tests *)
assert (exists (fun _ -> true) [] = false);;
assert (exists (fun _ -> true) [ 0 ] = true);;
assert (exists (fun x -> x mod 2 = 0) [ 1; 3; 5; 7 ] = false);;
assert (exists (fun x -> x mod 2 = 0) [ 1; 3; 4; 7 ] = true);;

(* filter tests *)
assert (filter (fun _ -> true) [] = []);;
assert (filter (fun _ -> false) [] = []);;
assert (filter (fun _ -> true) [ 0; 1; 2; 3; 4 ] = [ 0; 1; 2; 3; 4 ]);;
assert (filter (fun x -> x mod 2 = 0) [ 0; 1; 2; 3; 4; 5; 6 ] = [ 0; 2; 4; 6 ]);;

(* zip tests *)
assert (zip [] [] = []);;
assert (zip [ 1 ] [ "a" ] = [ 1, "a" ]);;
assert (zip [ 1; 2; 3 ] [ "a"; "b"; "c" ] = [ 1, "a"; 2, "b"; 3, "c" ]);;

assert (
  try zip [ 1; 2; 3 ] [ "a"; "b" ] = [] with
  | Invalid_argument _ -> true)
