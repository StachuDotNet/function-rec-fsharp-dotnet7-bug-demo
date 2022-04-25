open Ply
open FSharp.Control.Tasks.Affine.Unsafe

let dropWhileSimple condition list =
  let rec f (l : List<int>) : List<int> =
    match l with
    | [] -> []
    | head :: tail ->
      match condition head with
      | true -> f tail
      | false -> head :: tail

  f list

let dropWhileWithPlyAndMatch condition list =
  uply {
    let rec f (l : List<int>) : Ply<List<int>> =
      match l with
      | [] -> Ply []
      | head :: tail ->
        uply {
          match condition head with
          | true -> return! f tail
          | false -> return head :: tail
        }

    let! result = f list
    return result
  }

let dropWhileWithPlyAndFunction condition list =
  uply {
    let rec f : List<int> -> Ply<List<int>> =
      function
      | [] -> Ply []
      | head :: tail ->
        uply {
          match condition head with
          | true -> return! f tail
          | false -> return head :: tail
        }

    let! result = f list
    return result
  }

printfn "Hello from F#"

let list = [1; 2; 3]
let condition n = n < 3

// simple usage:
let simpleResult = dropWhileSimple condition list
printfn "Simple version: %A" simpleResult

let matchPlyResult = (dropWhileWithPlyAndMatch condition list |> Ply.TplPrimitives.runPlyAsTask).Result
printfn "Ply version with match: %A" matchPlyResult

let functionPlyResult = (dropWhileWithPlyAndFunction condition list |> Ply.TplPrimitives.runPlyAsTask).Result
printfn "Ply version with function: %A" matchPlyResult
