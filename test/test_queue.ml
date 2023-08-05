open Algos.Queue

let () =
  let queue = empty in
  let res, queue = pop queue in
  assert (res = None);
  assert (is_empty queue = true);
  assert (size queue = 0);
  assert (peek queue = None);
  let queue = push 1 queue in
  assert (peek queue = Some 1);
  let queue = queue |> push 2 |> push 3 in
  assert (peek queue = Some 1);
  let res, queue = pop queue in
  assert (res = Some 1);
  let res, queue = pop queue in
  assert (res = Some 2);
  let res, queue = pop queue in
  assert (res = Some 3);
  let res, queue = pop queue in
  assert (res = None);
  assert (peek queue = None);
  let queue = push 10 queue in
  assert (peek queue = Some 10);
  let queue = push 5 queue in
  assert (peek queue = Some 10);
  let res, queue = pop queue in
  assert (res = Some 10);
  assert (peek queue = Some 5);
  let queue = queue |> push 4 |> push 3 |> push 2 in
  assert (peek queue = Some 5);
  let res, queue = pop queue in
  assert (res = Some 5);
  let res, queue = pop queue in
  assert (res = Some 4);
  let res, queue = pop queue in
  assert (res = Some 3);
  assert (peek queue = Some 2);
  let queue = push 1 queue in
  let res, queue = pop queue in
  assert (res = Some 2);
  let res, queue = pop queue in
  assert (res = Some 1);
  let res, queue = pop queue in
  assert (res = None);
  assert (is_empty queue)
;;
