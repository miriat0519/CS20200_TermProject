module DuelPhase

open Types
open Deck
open Poker
open Card
open RefinePhase
open Ability

let rng = System.Random()

let rec localPowerSet lst =
    match lst with
    | [] -> [[]]
    | h::t -> let pt = localPowerSet t in pt @ List.map (fun subset -> h::subset) pt

let localRankToScore diff rank =
    let s = match rank with
            | HighCard _ -> 0.0 | OnePair _ -> 1.0 | TwoPair _ -> 2.0 | Triple _ -> 3.0
            | Straight _ -> 4.0 | Flush _ -> 5.0 | FullHouse _ -> 6.0 | FourCard _ -> 7.0
            | StraightFlush _ -> 8.0 | RoyalFlush -> 9.0
    if diff = Delta then 9.0 - s else s

let valueToString lang v =
    match v with | 14 -> "A" | 13 -> "K" | 12 -> "Q" | 11 -> "J" | n -> string n

let handRankToString lang rank =
    match lang, rank with
    | KOR, HighCard (h::_) -> sprintf "하이카드 - %s" (valueToString KOR h)
    | KOR, OnePair (p, _) -> sprintf "원페어 - %s" (valueToString KOR p)
    | KOR, TwoPair (p1, _, _) -> sprintf "투페어 - %s" (valueToString KOR p1)
    | KOR, Triple (t, _) -> sprintf "트리플 - %s" (valueToString KOR t)
    | KOR, Straight h -> sprintf "스트레이트 - %s" (valueToString KOR h)
    | KOR, Flush (h::_) -> sprintf "플러시 - %s" (valueToString KOR h)
    | KOR, FullHouse (t, k) -> sprintf "풀하우스 - %s" (valueToString KOR t)
    | KOR, FourCard (f, _) -> sprintf "포카드 - %s" (valueToString KOR f)
    | KOR, StraightFlush h -> sprintf "스트레이트 플러시 - %s" (valueToString KOR h)
    | KOR, RoyalFlush -> "로열 플러시"
    | ENG, HighCard (h::_) -> sprintf "High Card - %s" (valueToString ENG h)
    | ENG, OnePair (p, _) -> sprintf "One Pair - %s" (valueToString ENG p)
    | ENG, TwoPair (p1, _, _) -> sprintf "Two Pair - %s" (valueToString ENG p1)
    | ENG, Triple (t, _) -> sprintf "Triple - %s" (valueToString ENG t)
    | ENG, Straight h -> sprintf "Straight - %s" (valueToString ENG h)
    | ENG, Flush (h::_) -> sprintf "Flush - %s" (valueToString ENG h)
    | ENG, FullHouse (t, k) -> sprintf "Full House - %s" (valueToString ENG t)
    | ENG, FourCard (f, _) -> sprintf "Four of a Kind - %s" (valueToString ENG f)
    | ENG, StraightFlush h -> sprintf "Straight Flush - %s" (valueToString ENG h)
    | ENG, RoyalFlush -> "Royal Flush"
    | _, _ -> "Unknown"

let isRoyalOnlyRank rank =
    match rank with
    | HighCard nums -> nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | OnePair (p, nums) -> p >= 11 && p <= 13 && nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | TwoPair (p1, p2, nums) -> p1 >= 11 && p1 <= 13 && p2 >= 11 && p2 <= 13 && nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | Triple (t, nums) -> t >= 11 && t <= 13 && nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | Straight h -> h >= 11 && h <= 13
    | Flush nums -> nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | FullHouse (t, k) -> t >= 11 && t <= 13 && k >= 11 && k <= 13 
    | FourCard (f, nums) -> f >= 11 && f <= 13 && nums |> List.forall (fun r -> r >= 11 && r <= 13)
    | StraightFlush h -> h >= 11 && h <= 13
    | RoyalFlush -> false 

let calcPoints lang isPlayer diff (abilities: Ability list) rank isWin roundNumber mulliganCount =
    if not isWin then 0
    else
        let baseP = if diff = Goldwing then roundNumber * 2 else roundNumber
        let p1 = if List.contains BonusPoint abilities then announceAbility lang isPlayer BonusPoint; baseP + System.Random.Shared.Next(3) else baseP
        let p2 = if List.contains Twins abilities && (match rank with OnePair _ | TwoPair _ -> true | _ -> false) then announceAbility lang isPlayer Twins; p1 + 2 else p1
        let p3 = if List.contains LuckySeven abilities && (match rank with HighCard _ -> true | _ -> false) then announceAbility lang isPlayer LuckySeven; p2 + 7 else p2
        let p4 = if List.contains Gift abilities && isRoyalOnlyRank rank then announceAbility lang isPlayer Gift; p3 * 2 else p3
        let p5 = if List.contains FirstImpression abilities && mulliganCount = 0 then announceAbility lang isPlayer FirstImpression; p4 + 2 else p4
        let finalRound = if diff = Aion then 3 else 5
        let p6 = if List.contains GrandFinale abilities && roundNumber = finalRound then announceAbility lang isPlayer GrandFinale; p5 + 4 else p5
        p6

let doMulligan lang player sortedHand drawCount =
    let pool = player.Deck |> List.filter (fun c -> not (List.contains c sortedHand)) |> shuffle
    let maxRedrawable = List.length pool
    let maxIdx = List.length sortedHand
    
    if lang = ENG then
        if maxRedrawable < 5 then printfn "\nSelect up to %d cards to replace (0 to finish)." maxRedrawable
        else printfn "\nSelect cards to replace (0 to finish)."
    else
        if maxRedrawable < 5 then printfn "\n최대 %d장까지 교체할 카드의 번호를 입력하세요 (0 입력 시 종료):" maxRedrawable
        else printfn "\n교체할 카드의 번호를 입력하세요 (0 입력 시 종료):"
    
    let indices = getSelections lang [] 0 maxRedrawable maxIdx |> List.map (fun i -> i - 1) |> Set.ofList
    let kept = sortedHand |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> not (Set.contains i indices)) |> List.map snd
    let redraw = drawCount - List.length kept
    let newCards = pool |> List.truncate redraw
    (sortCards (kept @ newCards), indices)

let doMulliganAI difficulty enemy playerAbilities hand drawCount =
    let pool = enemy.Deck |> List.filter (fun c -> not (List.contains c hand)) |> shuffle
    let maxRedrawable = List.length pool
    
    let indices =
        match difficulty with
        | Beginner ->
            hand |> List.mapi (fun i _ -> i) |> List.filter (fun _ -> System.Random.Shared.Next(2) = 0) |> List.truncate maxRedrawable |> Set.ofList
        | Intermediate ->
            let subsets = localPowerSet hand
            let bestSubset =
                subsets |> List.maxBy (fun kept ->
                    let redrawCount = drawCount - List.length kept
                    let clampedRedraw = min redrawCount maxRedrawable
                    if clampedRedraw = 0 then 
                        localRankToScore difficulty (evaluateHand enemy.Abilities playerAbilities kept)
                    else 
                        let sims = 
                            [1..5] |> List.map (fun _ ->
                                let drawn = pool |> shuffle |> draw clampedRedraw
                                localRankToScore difficulty (evaluateHand enemy.Abilities playerAbilities (kept @ drawn))
                            )
                        List.average sims
                )
            hand |> List.mapi (fun i c -> i, c) |> List.filter (fun (_, c) -> not (List.contains c bestSubset)) |> List.map fst |> List.truncate maxRedrawable |> Set.ofList
        | _ ->
            let subsets = localPowerSet hand
            let bestSubset =
                subsets |> List.maxBy (fun kept ->
                    let redrawCount = drawCount - List.length kept
                    let clampedRedraw = min redrawCount maxRedrawable
                    if clampedRedraw = 0 then 
                        localRankToScore difficulty (evaluateHand enemy.Abilities playerAbilities kept)
                    else 
                        let sims = 
                            [1..15] |> List.map (fun _ ->
                                let drawn = pool |> shuffle |> draw clampedRedraw
                                localRankToScore difficulty (evaluateHand enemy.Abilities playerAbilities (kept @ drawn))
                            )
                        List.average sims
                )
            hand |> List.mapi (fun i c -> i, c) |> List.filter (fun (_, c) -> not (List.contains c bestSubset)) |> List.map fst |> List.truncate maxRedrawable |> Set.ofList

    let kept = hand |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> not (Set.contains i indices)) |> List.map snd
    let redraw = drawCount - List.length kept
    let newCards = pool |> List.truncate redraw
    (sortCards (kept @ newCards), indices)

let duel lang roundNumber difficulty player enemy =
    if List.contains MakeFriends player.Abilities then announceAbility lang true MakeFriends
    if List.contains MakeFriends enemy.Abilities then announceAbility lang false MakeFriends

    let pDrawCount = if List.contains MakeFriends player.Abilities then 6 else 5
    let eDrawCount = 
        if difficulty = Goliath then 
            if lang = ENG then printfn "🧌 [Goliath] The Giant draws 7 cards!" else printfn "🧌 [골리앗] 거인의 왕이 7장의 카드를 드로우합니다!"
            7
        elif List.contains MakeFriends enemy.Abilities then 6 else 5

    let pExtraCard, eTempDeck = 
        if List.contains CrowIdol player.Abilities && List.length enemy.Deck > 0 then
            announceAbility lang true CrowIdol
            ([enemy.Deck.[0]], List.tail enemy.Deck)
        else ([], enemy.Deck)

    let playerInitialHand = sortCards ((player.Deck |> shuffle |> draw pDrawCount) @ pExtraCard)
    let enemyInitialHand = sortCards (eTempDeck |> shuffle |> draw eDrawCount)

    if lang = ENG then printfn "\n=== Duel Phase ===\nYour Initial Hand:" else printfn "\n=== 듀얼 페이즈 ===\n당신의 최초 손패:"
    printCards lang playerInitialHand

    let isBackToTheWall = List.contains BackToTheWall player.Abilities || List.contains BackToTheWall enemy.Abilities
    let pCanMulligan = not isBackToTheWall && not (List.contains HandOfFate player.Abilities)
    let eCanMulligan = not isBackToTheWall && not (List.contains HandOfFate enemy.Abilities)

    if isBackToTheWall then
        if lang = ENG then printfn "\n⚔️ [Back to the Wall] Both players cannot mulligan!" else printfn "\n⚔️ [배수의 진] 효과로 양측 모두 교체(멀리건)를 진행할 수 없습니다!"
    else
        if List.contains HandOfFate player.Abilities then
            if lang = ENG then printfn "\n⚔️ [Hand of Fate] You cannot mulligan!" else printfn "\n⚔️ [운명의 패] 효과로 당신은 교체(멀리건)를 진행할 수 없습니다!"
        if List.contains HandOfFate enemy.Abilities then
            if lang = ENG then printfn "\n⚔️ [Hand of Fate] AI cannot mulligan!" else printfn "\n⚔️ [운명의 패] 효과로 AI는 교체(멀리건)를 진행할 수 없습니다!"

    let pFinalHand1, pMulliganCount1 =
        if pCanMulligan then
            let h1, idx1 = doMulligan lang player playerInitialHand pDrawCount
            (h1, Set.count idx1)
        else (playerInitialHand, 0)

    let pFinalHand2, pMulliganCount2 =
        if pCanMulligan && List.contains SecondChance player.Abilities then
            announceAbility lang true SecondChance
            printCards lang pFinalHand1
            let h2, idx2 = doMulligan lang player pFinalHand1 pDrawCount
            (h2, pMulliganCount1 + Set.count idx2)
        else (pFinalHand1, pMulliganCount1)

    let pFinalHand = pFinalHand2
    let pMulliganCount = pMulliganCount2

    if pCanMulligan && pMulliganCount > 0 then
        if lang = ENG then printfn "\n[Your Final Hand after Swap]:" else printfn "\n[교체 완료된 당신의 최종 패]:"
        printCards lang pFinalHand

    let eFinalHand, eMulliganCount =
        if eCanMulligan then 
            let h, idx = doMulliganAI difficulty enemy player.Abilities enemyInitialHand eDrawCount
            (h, Set.count idx)
        else (enemyInitialHand, 0)

    if lang = ENG then printfn "\n[AI's Final Hand]:" else printfn "\n[AI의 최종 패]:"
    printCards lang eFinalHand

    let pRank = evaluateHand player.Abilities enemy.Abilities pFinalHand
    let eRank = evaluateHand enemy.Abilities player.Abilities eFinalHand
    
    let rawResult = compareHands pRank eRank
    let result = if difficulty = Delta then -rawResult else rawResult

    let pIsWin = result > 0 || (List.contains Footstool player.Abilities && result = 0)
    let eIsWin = result < 0 || (List.contains Footstool enemy.Abilities && result = 0)

    let pEarnBase = calcPoints lang true difficulty player.Abilities pRank pIsWin roundNumber pMulliganCount
    let eEarnBase = calcPoints lang false difficulty enemy.Abilities eRank eIsWin roundNumber 0

    let pEarn = if List.contains LastHope player.Abilities && (match pRank with RoyalFlush | StraightFlush _ -> true | _ -> false) then 999 else pEarnBase
    let eEarn = if List.contains LastHope enemy.Abilities && (match eRank with RoyalFlush | StraightFlush _ -> true | _ -> false) then 999 else eEarnBase

    let pTaxed, eTaxed =
        match List.contains Tax player.Abilities, List.contains Tax enemy.Abilities with
        | true, false when eEarn > 0 -> pEarn + 1, eEarn - 1
        | false, true when pEarn > 0 -> pEarn - 1, eEarn + 1
        | _ -> pEarn, eEarn

    let pGWP = if difficulty = Goldwing then pMulliganCount else 0
    let eGWP = if difficulty = Goldwing then eMulliganCount else 0

    let pDeckAfterDuel = 
        let d1 = if List.contains Disconnection player.Abilities then player.Deck |> List.filter (fun c -> not (List.contains c playerInitialHand)) else player.Deck
        let d2 = if List.contains Oblivion player.Abilities then d1 |> List.filter (fun c -> not (List.contains c pFinalHand)) else d1
        d2

    let nextPlayer = { player with Score = player.Score + pTaxed - pGWP; Deck = pDeckAfterDuel }
    let nextEnemy = { enemy with Score = enemy.Score + eTaxed - eGWP }

    let pFire = if nextPlayer.Score > nextEnemy.Score then " 🔥" else ""
    let eFire = if nextEnemy.Score > nextPlayer.Score then " 🔥" else ""

    printfn "\n======================"
    if lang = ENG then
        if difficulty = Delta then printfn "🌀 [Delta] Hand rankings are INVERTED!"
        printfn "Your Hand: [ %s ]" (handRankToString ENG pRank)
        printfn "AI's Hand: [ %s ]" (handRankToString ENG eRank)
        printfn ""
        if pGWP > 0 then printfn "💰 [Goldwing Penalty] You lost %d Score for swapping cards!" pGWP
        if eGWP > 0 then printfn "💰 [Goldwing Penalty] AI lost %d Score for swapping cards!" eGWP
        
        if pIsWin then printfn "You win!" elif eIsWin then printfn "AI wins..." else printfn "Draw"
        printfn "----------------------"
        printfn "Total Score:\nPlayer: %d%s\nAI: %d%s" nextPlayer.Score pFire nextEnemy.Score eFire
    else
        if difficulty = Delta then printfn "🌀 [델타] 가치 역전! 족보의 서열이 반대로 적용되었습니다!"
        printfn "당신의 족보 [ %s ]" (handRankToString KOR pRank)
        printfn "AI의 족보 [ %s ]" (handRankToString KOR eRank)
        printfn ""
        if pGWP > 0 then printfn "💰 [골드윙 페널티] 교체(멀리건) 비용으로 승점 %d점을 잃었습니다!" pGWP
        if eGWP > 0 then printfn "💰 [골드윙 페널티] AI가 교체 비용으로 승점 %d점을 잃었습니다!" eGWP
        
        if pIsWin then printfn "당신이 승리했습니다!" elif eIsWin then printfn "AI가 승리했습니다..." else printfn "무승부입니다."
        printfn "----------------------"
        printfn "현재 총 승점:\n당신: %d점%s\nAI: %d점%s" nextPlayer.Score pFire nextEnemy.Score eFire
    printfn "======================"
    System.Threading.Thread.Sleep(1500)

    let roundResultText = 
        if lang = ENG then
            let winStr = if pIsWin then "Win" elif eIsWin then "Loss" else "Draw"
            sprintf "%s (You: %s vs AI: %s)" winStr (handRankToString ENG pRank) (handRankToString ENG eRank)
        else
            let winStr = if pIsWin then "승리" elif eIsWin then "패배" else "무승부"
            sprintf "%s (당신: %s vs AI: %s)" winStr (handRankToString KOR pRank) (handRankToString KOR eRank)

    (nextPlayer, nextEnemy, roundResultText)