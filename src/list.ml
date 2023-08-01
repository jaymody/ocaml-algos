let cons a l = a :: l
let append l1 l2 = l1 @ l2

let hd = function
  | [] -> None
  | hd :: _ -> Some hd
;;

let tl = function
  | [] -> None
  | _ :: tl -> Some tl
;;

let length l =
  let rec aux n = function
    | [] -> n
    | _ :: tl -> aux (n + 1) tl
  in
  aux 0 l
;;

let rec nth l n =
  match l with
  | [] -> None
  | hd :: tl -> if n = 0 then Some hd else nth tl (n - 1)
;;

let rev l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] l
;;

let rec concat = function
  | [] -> []
  | hd :: tl -> hd @ concat tl
;;

let init len f =
  let rec aux n acc = if n >= 0 then aux (n - 1) (f n :: acc) else acc in
  aux (len - 1) []
;;

let map f l =
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl -> aux (f hd :: acc) tl
  in
  aux [] l
;;

let fold_left f init_val l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (f acc hd) tl
  in
  aux init_val l
;;

let fold_right f init_val l =
  let rec aux = function
    | [] -> init_val
    | hd :: tl -> f (aux tl) hd
  in
  aux l
;;

let rec for_all p = function
  | [] -> true
  | hd :: tl -> p hd && for_all p tl
;;

let rec exists p = function
  | [] -> false
  | hd :: tl -> p hd || exists p tl
;;

let filter p l =
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl -> aux (if p hd then hd :: acc else acc) tl
  in
  aux [] l
;;

let zip l1 l2 =
  let rec aux l1 l2 acc =
    match l1, l2 with
    | [], [] -> rev acc
    | hd1 :: tl1, hd2 :: tl2 -> aux tl1 tl2 ((hd1, hd2) :: acc)
    | _ -> invalid_arg "l1 and l2 must be same size to zip"
  in
  aux l1 l2 []
;;
