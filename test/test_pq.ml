open Algos
open Pq.Make (Int)

let pq = empty;;

assert (is_empty pq = true);
assert (size pq = 0);
assert (fst (pop pq) = None);
assert (peek pq = None);
let pq = pq |> push 4 |> push 2 |> push 6 |> push 3 in
assert (peek pq = Some 2);
let res, pq = pop pq in
assert (res = Some 2);
let res, pq = pop pq in
assert (res = Some 3);
assert (size pq = 2);
let res, pq = pop pq in
assert (res = Some 4);
assert (size pq = 1);
assert (peek pq = Some 6);
let res, pq = pop pq in
assert (res = Some 6);
assert (size pq = 0);
let res, pq = pop pq in
assert (res = None);
assert (size pq = 0);
assert (peek pq = None);
let pq = pq |> push 1 |> push 2 |> push 2 |> push (-1) in
assert (size pq = 4);
assert (peek pq = Some (-1));
let res, pq = pop pq in
assert (res = Some (-1));
let res, pq = pop pq in
assert (res = Some 1);
let res, pq = pop pq in
assert (res = Some 2);
let pq = pq |> push 0 |> push 3 in
let res, pq = pop pq in
assert (res = Some 0);
let res, pq = pop pq in
assert (res = Some 2);
let res, pq = pop pq in
assert (res = Some 3);
let res, pq = pop pq in
assert (res = None);
assert (is_empty pq)
