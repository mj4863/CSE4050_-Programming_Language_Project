module P6

/// From list 'l', find the element that appears most frequently in the list,
/// and return how many times it appears. If the input list is empty, return 0.
let rec countMostFrequent (l: List<'a>) : int =
  // TODO
  match l with
  | [] -> 0
  | head :: tail ->
    let rec countElement (e: 'a) (lst: List<'a>) (acc: int) : int =
      match lst with
      | [] -> acc
      | x :: y ->
        if x = e then countElement e y (acc + 1)
        else countElement e y acc
    let cnt = countElement head tail 1
    let mx = countMostFrequent tail
    if cnt > mx then cnt else mx
