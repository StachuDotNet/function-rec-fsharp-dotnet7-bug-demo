let dropWhileWithMatch condition list =
  let rec f (l : List<int>) : List<int> =
    match l with
    | [] -> []
    | head :: tail ->
      match condition head with
      | true -> f tail
      | false -> head :: tail

  f list

let dropWhileWithFunction condition list =
  let rec f : List<int> -> List<int> =
    function
    | [] -> []
    | head :: tail ->
      match condition head with
      | true -> f tail
      | false -> head :: tail

  f list

let list = [1; 2; 3]
let condition n = n < 3

let matchResult = dropWhileWithMatch condition list
printfn "Match, no Ply: %A" matchResult

let functionResult = dropWhileWithFunction condition list
printfn "Function, no Ply: %A" functionResult