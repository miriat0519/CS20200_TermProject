module Deck

open Types

let allSuits = [ Hearts; Diamonds; Clubs; Spades ]
let allRanks = 
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let createSingleDeck () =
    [ for s in allSuits do
        for r in allRanks do
            yield { Suit = s; Rank = r } ]

let createDoubleDeck () =
    createSingleDeck() @ createSingleDeck()

let shuffle deck =
    deck |> List.sortBy (fun _ -> System.Random.Shared.Next())

// 🌟 FIX: take 대신 truncate를 사용하여 덱이 부족해도 튕기지 않도록 방어합니다.
let draw count deck =
    deck |> List.truncate count