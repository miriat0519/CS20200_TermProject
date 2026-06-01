module AI

open Types
open Deck
open Poker

let rec powerSet lst =
    match lst with
    | [] -> [[]]
    | h::t -> let pt = powerSet t in pt @ List.map (fun subset -> h::subset) pt

let rec combinations n lst =
    match n, lst with
    | 0, _ -> [[]] | _, [] -> []
    | k, x::xs -> (combinations (k - 1) xs |> List.map (fun c -> x::c)) @ combinations k xs

let rec removeCards deck toRemove =
    toRemove |> List.fold (fun acc c ->
        match List.tryFindIndex ((=) c) acc with | Some i -> let b, a = List.splitAt i acc in b @ List.tail a | None -> acc
    ) deck

let rankToScore diff rank =
    let baseScore =
        match rank with
        | HighCard _ -> 0.0 | OnePair _ -> 1.0 | TwoPair _ -> 2.0 | Triple _ -> 3.0
        | Straight _ -> 4.0 | Flush _ -> 5.0 | FullHouse _ -> 6.0 | FourCard _ -> 7.0
        | StraightFlush _ -> 8.0 | RoyalFlush -> 9.0
    if diff = Delta then 9.0 - baseScore else baseScore

let simulateAverage diff deck abilities enemyAbilities =
    if List.length deck < 5 then 0.0 else
        let sims = 
            [1..10] |> List.map (fun _ -> 
                deck |> shuffle |> draw 5 |> evaluateHand abilities enemyAbilities |> rankToScore diff
            )
        List.average sims

let calculateEV diff deck abilities enemyAbilities =
    if List.length deck < 5 then 0.0 else
        let allCombs = combinations 5 deck
        (allCombs |> List.map (fun h -> evaluateHand abilities enemyAbilities h |> rankToScore diff) |> List.sum) / (float (List.length allCombs))

let chooseRemoveCards diff abilities enemyAbilities shown currentDeck =
    let poss = powerSet shown
    match diff with
    | Beginner -> shown |> List.filter (fun _ -> System.Random.Shared.Next(2) = 0)
    | Intermediate -> poss |> List.maxBy (fun r -> simulateAverage diff (removeCards currentDeck r) abilities enemyAbilities)
    | _ -> poss |> List.maxBy (fun r -> calculateEV diff (removeCards currentDeck r) abilities enemyAbilities)

let chooseAddCards diff abilities enemyAbilities comm currentDeck =
    let poss = powerSet comm
    match diff with
    | Beginner -> comm |> List.filter (fun _ -> System.Random.Shared.Next(2) = 0)
    | Intermediate -> poss |> List.maxBy (fun a -> simulateAverage diff (currentDeck @ a) abilities enemyAbilities)
    | _ -> poss |> List.maxBy (fun a -> calculateEV diff (currentDeck @ a) abilities enemyAbilities)