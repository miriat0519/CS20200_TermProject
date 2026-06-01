module Poker

open Types
open Deck

type HandRank =
    | HighCard of int list
    | OnePair of int * int list
    | TwoPair of int * int * int list
    | Triple of int * int list
    | Straight of int
    | Flush of int list
    | FullHouse of int * int
    | FourCard of int * int list
    | StraightFlush of int
    | RoyalFlush

let rankValue rank =
    match rank with
    | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6
    | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10
    | Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14

// 🌟 FIX: 카드가 5장보다 많을 때 5장짜리 조합을 모두 만들어주는 함수 추가
let rec combinations n lst =
    match n, lst with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, x::xs -> (combinations (k - 1) xs |> List.map (fun c -> x::c)) @ combinations k xs

let rec generatePromotionHands (cards: Card list) =
    match cards with
    | [] -> [[]]
    | c :: tail ->
        let tailHands = generatePromotionHands tail
        let v = rankValue c.Rank
        if v >= 2 && v <= 10 then
            let opt1 = tailHands |> List.map (fun h -> c :: h)
            let nextRank =
                match v with
                | 2 -> Three | 3 -> Four | 4 -> Five | 5 -> Six | 6 -> Seven
                | 7 -> Eight | 8 -> Nine | 9 -> Ten | 10 -> Jack
                | _ -> c.Rank
            let opt2 = tailHands |> List.map (fun h -> { c with Rank = nextRank } :: h)
            opt1 @ opt2
        else
            tailHands |> List.map (fun h -> c :: h)

let rec evaluateHand (abilities: Ability list) (enemyAbilities: Ability list) (cards: Card list) =
    // 🌟 FIX: 패가 5장을 초과할 경우, 5장으로 만들 수 있는 모든 족보 중 가장 높은 것을 반환
    if List.length cards > 5 then
        combinations 5 cards
        |> List.map (fun h -> evaluateHand abilities enemyAbilities h)
        |> List.max
    elif List.contains Joker abilities && List.length cards > 0 then
        let allPossibleCards = createSingleDeck()
        let bestHand = 
            allPossibleCards 
            |> List.map (fun jokerCard -> 
                let testHand = jokerCard :: (List.tail cards)
                evaluateHand (abilities |> List.filter ((<>) Joker)) enemyAbilities testHand)
            |> List.maxBy id
        bestHand
    elif List.contains Promotion abilities then
        generatePromotionHands cards
        |> List.map (fun h -> evaluateHand (abilities |> List.filter ((<>) Promotion)) enemyAbilities h)
        |> List.max
    else
        let colorCards =
            if List.contains ColorPlay abilities then cards |> List.map (fun c -> 
                match c.Suit with | Spades -> {c with Suit=Clubs} | Hearts -> {c with Suit=Diamonds} | _ -> c)
            else cards

        let ranksWithJack =
            let baseRanks = colorCards |> List.map (fun c -> rankValue c.Rank)
            if List.contains DisguisedJack abilities && List.contains 11 baseRanks then
                let highestNonJack = baseRanks |> List.filter (fun r -> r <> 11) |> function [] -> 14 | lst -> List.max lst
                baseRanks |> List.map (fun r -> if r = 11 then highestNonJack else r)
            else baseRanks

        let ranks = ranksWithJack |> List.sortDescending
        let isOnlyRoyal = ranks |> List.forall (fun r -> r >= 11 && r <= 13)
        let isFifthStar = ranks |> Set.ofList |> Set.count = 1 && List.contains FifthStar abilities

        let rankCounts = ranks |> List.countBy id |> List.sortByDescending (fun (r, c) -> (c, r))
        
        let isFlush = 
            let req = if List.contains FlushBros abilities then 4 else 5
            let diaCnt = if List.contains Gemstone abilities then colorCards |> List.filter (fun c -> c.Suit = Diamonds) |> List.length else 0
            colorCards |> List.countBy (fun c -> c.Suit) 
                       |> List.exists (fun (s, c) -> s <> Diamonds && c + diaCnt >= req) || diaCnt >= req

        let isHousekeeper2Flush = List.contains Housekeeper2 abilities && (colorCards |> List.map (fun c -> c.Suit) |> Set.ofList |> Set.count = 2)
        let isBlackAndWhiteFlush = List.contains BlackAndWhite abilities && (ranks |> List.forall (fun r -> r % 2 = 0) || ranks |> List.forall (fun r -> r % 2 <> 0))
        let finalFlush = isFlush || isHousekeeper2Flush || isBlackAndWhiteFlush

        let isStraight, straightHigh = 
            let isSeq lst = 
                let len = if List.contains StraightBros abilities then 4 else 5
                lst |> List.windowed 2 |> List.take (min (List.length lst - 1) (len - 1)) |> List.forall (fun w -> w.[0] - w.[1] = 1)
            let isArith lst =
                if List.contains Arithmetic abilities then lst |> List.windowed 2 |> List.map (fun w -> w.[0] - w.[1]) |> Set.ofList |> Set.count = 1 else false
                
            if List.length ranks >= 4 && (isSeq ranks || isArith ranks) then true, ranks.[0]
            elif List.length ranks >= 5 && ranks.[0] = 14 && isSeq (5 :: ranks |> List.skip 1) then true, 5
            else false, 0

        let baseRank =
            match finalFlush, isStraight, isFifthStar with
            | _, _, true -> RoyalFlush
            | true, true, _ when straightHigh = 14 -> RoyalFlush
            | true, true, _ -> StraightFlush straightHigh
            | true, false, _ -> Flush ranks
            | false, true, _ -> Straight straightHigh
            | false, false, _ ->
                match rankCounts with
                | [(r1, 4); (r2, 1)] -> FourCard (r1, [r2])
                | [(r1, 3); (r2, 2)] -> FullHouse (r1, r2)
                | [(r1, 3); (r2, 1); (r3, 1)] -> Triple (r1, [r2; r3])
                | [(r1, 2); (r2, 2); (r3, 1)] -> TwoPair (r1, r2, [r3])
                | [(r1, 2); (r2, 1); (r3, 1); (r4, 1)] -> OnePair (r1, [r2; r3; r4])
                | _ -> HighCard ranks

        let upgradedRank = 
            if List.contains Jackpot abilities && (match baseRank with Triple (7, _) -> true | _ -> false) then RoyalFlush
            elif List.contains SmallEvolution abilities && (match baseRank with TwoPair _ -> true | _ -> false) then 
                match baseRank with TwoPair(r1, r2, k) -> Triple(r1, r2::k) | _ -> baseRank
            elif List.contains Protagonist abilities && (match baseRank with HighCard _ -> true | _ -> false) then 
                match baseRank with HighCard k -> TwoPair(15, 15, k) | _ -> baseRank
            elif List.contains Housekeeper abilities && (match baseRank with TwoPair _ -> true | _ -> false) then 
                match baseRank with TwoPair(r1, r2, k) -> Flush(15 :: r1 :: r2 :: k) | _ -> baseRank
            elif List.contains LoneWolves abilities && (match baseRank with HighCard _ -> true | _ -> false) then 
                match baseRank with HighCard k -> Triple(k.[0], List.tail k) | _ -> baseRank
            elif List.contains RoyalDignity abilities && isOnlyRoyal then
                match baseRank with | HighCard k -> OnePair(k.[0], k) | OnePair(r, k) -> TwoPair(r, r, k) | _ -> baseRank
            else baseRank

        let isEmbargoed targetRank =
            let check abList =
                abList |> List.exists (fun ab ->
                    match ab, targetRank with
                    | Embargo "OnePair", OnePair _ -> true
                    | Embargo "TwoPair", TwoPair _ -> true
                    | Embargo "Triple", Triple _ -> true
                    | Embargo "Straight", Straight _ -> true
                    | Embargo "Flush", Flush _ -> true
                    | Embargo "FullHouse", FullHouse _ -> true
                    | Embargo "FourCard", FourCard _ -> true
                    | Embargo "StraightFlush", StraightFlush _ -> true
                    | Embargo "RoyalFlush", RoyalFlush -> true
                    | _ -> false)
            check abilities || check enemyAbilities

        if isEmbargoed upgradedRank then HighCard [0]
        else upgradedRank

let compareHands hand1 hand2 = compare hand1 hand2