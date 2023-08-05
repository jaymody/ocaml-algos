open Algos.Stack

let () =
  let stack = empty in
  let res, stack = pop stack in
  assert (res = None);
  assert (peek stack = None);
  assert (size stack = 0);
  assert (is_empty stack);
  let stack = stack |> push 1 |> push 2 |> push 3 |> push 4 in
  assert (size stack = 4);
  assert (not (is_empty stack));
  assert (peek stack = Some 4);
  let res, stack = pop stack in
  assert (res = Some 4);
  let res, stack = pop stack in
  assert (res = Some 3);
  let res, stack = pop stack in
  assert (res = Some 2);
  assert (size stack = 1);
  assert (not (is_empty stack));
  assert (peek stack = Some 1);
  let stack = stack |> push 5 |> push 2 in
  assert (size stack = 3);
  let res, stack = pop stack in
  assert (res = Some 2);
  let res, stack = pop stack in
  assert (res = Some 5);
  let res, stack = pop stack in
  assert (res = Some 1);
  let res, stack = pop stack in
  assert (res = None);
  assert (peek stack = None);
  assert (size stack = 0);
  assert (is_empty stack)
;;
