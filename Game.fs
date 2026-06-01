module Game

open Types
open Deck
open Poker 
open RefinePhase
open DuelPhase
open Ability
open AI
open Card
open System

let rec removeCardsFromPool pool toRemove =
    toRemove |> List.fold (fun acc c ->
        match List.tryFindIndex ((=) c) acc with | Some i -> let b, a = List.splitAt i acc in b @ List.tail a | None -> acc
    ) pool

let rec getAdalanDraft lang maxIdx =
    if lang = ENG then printfn "\nEnter 20 unique card numbers separated by spaces (e.g. 1 5 12 ...):"
    else printfn "\n스페이스바로 구분하여 20개의 카드 번호를 한 번에 입력하세요 (예: 1 5 12 ...):"
    
    let input = Console.ReadLine()
    let parts = input.Split([|' '; ','; '\t'|], StringSplitOptions.RemoveEmptyEntries)
    let parsed = parts |> Array.choose (fun s -> match Int32.TryParse(s) with true, v -> Some (v - 1) | _ -> None) |> Set.ofArray |> Set.toList
    
    if parsed.Length = 20 && parsed |> List.forall (fun i -> i >= 0 && i < maxIdx) then parsed
    else
        if lang = ENG then printfn "Invalid input. Please enter exactly 20 valid numbers."
        else printfn "입력이 잘못되었습니다. 1~40 사이의 중복되지 않는 번호 정확히 20개를 입력해주세요."
        getAdalanDraft lang maxIdx

let setupGame lang difficulty =
    match difficulty with
    | Adalan ->
        let pool = createDoubleDeck() |> shuffle |> List.take 40 |> sortCards
        if lang = ENG then printfn "\n=== Sage Adalan's Trial: Draft 20 cards from 40 ===" 
        else printfn "\n=== 현자 아달란의 시련: 40장의 카드 중 20장 선택 ==="
        printCards lang pool
        
        let pIndices = getAdalanDraft lang 40
        let pDeck = pIndices |> List.map (fun i -> pool.[i])
        let p = { Name = "Player"; Deck = pDeck; Abilities = []; Score = 0 }
        
        let rankGroups = pool |> List.groupBy (fun c -> c.Rank) |> List.sortByDescending (fun (r, cards) -> (cards.Length, rankValue r))
        let eDeck = rankGroups |> List.collect snd |> List.truncate 20 |> sortCards
        let e = { Name = "Adalan"; Deck = eDeck; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\nSage Adalan has completed his draft!" else printfn "\n현자 아달란이 덱 선택을 마쳤습니다!"
        (p, e)
    | Dante ->
        let blackPool = createDoubleDeck() |> List.filter (fun c -> c.Suit = Spades || c.Suit = Clubs) |> shuffle
        let p = { Name = "Player"; Deck = blackPool |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Dante"; Deck = blackPool |> List.skip 20 |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n📜 King Dante rewrites the world in black ink!" else printfn "\n📜 기록의 왕 단테가 세상을 흑백으로 재기록합니다!"
        (p, e)
    | Stella ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Stella"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 4 }
        if lang = ENG then printfn "\n✨ Queen Stella starts the game with 4 Score!" else printfn "\n✨ 별의 여왕 스텔라가 승점 4점을 가지고 게임을 시작합니다!"
        (p, e)
    | Machina ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Machina"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n⚙️ King Machina connects to the ability core..." else printfn "\n⚙️ 기계의 왕 마키나가 능력 코어에 접속합니다..."
        (p, e)
    | Mirage ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Mirage"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n🪞 Queen Mirage stares into your soul..." else printfn "\n🪞 거울의 여왕 미라쥬가 당신의 영혼을 들여다봅니다..."
        (p, e)
    | Goliath ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Goliath"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n🧌 King Goliath roars!" else printfn "\n🧌 거인의 왕 골리앗이 포효합니다!"
        (p, e)
    | Venus ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Venus"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n👁️ King Venus engulfs the arena in darkness..." else printfn "\n👁️ 심연의 왕 베누스가 전장을 어둠으로 물들입니다..."
        (p, e)
    | Goldwing ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Goldwing"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n💰 King Goldwing flips a coin!" else printfn "\n💰 황금의 왕 골드윙이 판돈을 올립니다!"
        (p, e)
    | Delta ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Delta"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n🌀 King Delta inverts the laws of the universe!" else printfn "\n🌀 뒤틀린 왕 델타가 우주의 법칙을 뒤집습니다!"
        (p, e)
    | Sylphid ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Sylphid"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n🌿 Queen Sylphid embraces the wild..." else printfn "\n🌿 숲의 여왕 실피드가 거대한 자연을 품습니다..."
        (p, e)
    | Behemoth ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Behemoth"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n🐾 King Behemoth hungers for your deck..." else printfn "\n🐾 백수의 왕 베히모스가 당신의 덱을 탐냅니다..."
        (p, e)
    | Aion ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "Aion"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        if lang = ENG then printfn "\n⏳ Queen Aion accelerates time..." else printfn "\n⏳ 시간의 여왕 아이온이 모래시계를 거꾸로 돌립니다..."
        (p, e)
    | _ ->
        let p = { Name = "Player"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        let e = { Name = "AI"; Deck = createSingleDeck() |> shuffle |> List.take 20; Abilities = []; Score = 0 }
        (p, e)

let getBossName lang diff =
    match lang, diff with
    | KOR, Machina -> "마키나" | ENG, Machina -> "Machina"
    | KOR, Adalan -> "아달란" | ENG, Adalan -> "Adalan"
    | KOR, Stella -> "스텔라" | ENG, Stella -> "Stella"
    | KOR, Mirage -> "미라쥬" | ENG, Mirage -> "Mirage"
    | KOR, Goliath -> "골리앗" | ENG, Goliath -> "Goliath"
    | KOR, Venus -> "베누스" | ENG, Venus -> "Venus"
    | KOR, Goldwing -> "골드윙" | ENG, Goldwing -> "Goldwing"
    | KOR, Delta -> "델타" | ENG, Delta -> "Delta"
    | KOR, Sylphid -> "실피드" | ENG, Sylphid -> "Sylphid"
    | KOR, Dante -> "단테" | ENG, Dante -> "Dante"
    | KOR, Behemoth -> "베히모스" | ENG, Behemoth -> "Behemoth"
    | KOR, Aion -> "아이온" | ENG, Aion -> "Aion"
    | _, _ -> "AI"

let getBossQuote lang diff isBossWin =
    match lang, diff with
    | KOR, Machina -> if isBossWin then "이것까지 나의 계산이다." else "계산#@!# 오류... 재정립.. 재정립...@!# 실패"
    | ENG, Machina -> if isBossWin then "Everything was within my calculation." else "Calculation#@!# Error... Recalibrating...@!# Failed."
    | KOR, Adalan -> if isBossWin then "시련을 통과하지 못한 자여... 다음에 다시 돌아오게나" else "후대의 지식은 나를 뛰어 넘었단 말인가..."
    | ENG, Adalan -> if isBossWin then "You who failed the trial... Return another time." else "Has the knowledge of the future truly surpassed mine..."
    | KOR, Stella -> if isBossWin then "별의 힘앞에... 굴복하라!!" else "내 별들이... 무너져내린다..."
    | ENG, Stella -> if isBossWin then "Submit... before the power of the stars!!" else "My stars... are crumbling down..."
    | KOR, Mirage -> if isBossWin then "(웃음소리)..." else "(거울이 깨지는 소리)..."
    | ENG, Mirage -> if isBossWin then "(Laughs)..." else "(Sound of a shattering mirror)..."
    | KOR, Goliath -> if isBossWin then "압도적인 힘 앞에 굴복해라" else "다윗... 자네란 말인가..."
    | ENG, Goliath -> if isBossWin then "Submit before my overwhelming power." else "David... is that you..."
    | KOR, Venus -> if isBossWin then "크하하! 어둠의 힘을 느껴라!" else "크윽.. 두고보자.."
    | ENG, Venus -> if isBossWin then "Mwahaha! Feel the power of the abyss!" else "Ugh... Just you wait..."
    | KOR, Goldwing -> if isBossWin then "우리 함께 다음 게임에서 보자구 친구!" else "잠깐.. 방금은 무효야!"
    | ENG, Goldwing -> if isBossWin then "See you in the next game, friend!" else "Wait... that last one doesn't count!"
    | KOR, Delta -> if isBossWin then "가치가 뒤집힌 세계에 온 것을 환영한다." else "내 완벽한 수식에 오차가 생기다니...!"
    | ENG, Delta -> if isBossWin then "Welcome to the world of inverted values." else "An error in my perfect formula...!"
    | KOR, Sylphid -> if isBossWin then "자연의 섭리를 거스를 순 없는 법." else "이 숲도 이제 끝이구나..."
    | ENG, Sylphid -> if isBossWin then "You cannot defy the laws of nature." else "This forest is finally at its end..."
    | KOR, Dante -> if isBossWin then "너의 패배가 이 페이지에 기록되었다." else "나의 기록에 없는 변수라니..."
    | ENG, Dante -> if isBossWin then "Your defeat has been recorded on this page." else "An unrecorded variable...!"
    | KOR, Behemoth -> if isBossWin then "(뼈를 씹어 삼키는 섬뜩한 파열음이 메아리친다...)" else "이 거대한 굶주림마저... 채워주는구나..."
    | ENG, Behemoth -> if isBossWin then "(A chilling sound of crunching bones echoes...)" else "Even this endless hunger... is finally satisfied..."
    | KOR, Aion -> if isBossWin then "벌써 시간이 다 되었네. 너의 미래는 여기까지야." else "나의 모래시계가... 역행하고 있어...!"
    | ENG, Aion -> if isBossWin then "Time is up. Your future ends here." else "My hourglass... is flowing backwards...!"
    | _, _ -> ""

let printGameOverSummary lang player enemy difficulty history =
    let pFire = if player.Score > enemy.Score then " 🔥" else ""
    let eFire = if enemy.Score > player.Score then " 🔥" else ""

    printfn "\n\n======================================================="
    if lang = ENG then
        printfn "                  GAME OVER SUMMARY                    "
        printfn "=======================================================\n"
        printfn "[ Match History ]"
        history |> List.iteri (fun i res -> printfn " Round %d : %s" (i + 1) res)
        
        let pAbilName = 
            if player.Abilities.IsEmpty then "None" 
            else player.Abilities |> List.map (fun a -> fst (abilityDetails ENG a)) |> String.concat "\n             "
        printfn "\n[ Player's Final Status ]"
        printfn " Abilities : %s" pAbilName
        printfn " Score     : %d%s" player.Score pFire
        printfn " Deck      : %d cards remaining" (List.length player.Deck)
        printCards ENG (sortCards player.Deck)

        let eAbilName = 
            if enemy.Abilities.IsEmpty then "None" 
            else enemy.Abilities |> List.map (fun a -> fst (abilityDetails ENG a)) |> String.concat "\n             "
        printfn "\n[ AI's Final Status ]"
        printfn " Abilities : %s" eAbilName
        printfn " Score     : %d%s" enemy.Score eFire
        printfn " Deck      : %d cards remaining" (List.length enemy.Deck)
        printCards ENG (sortCards enemy.Deck)

        printfn "\n======================================================="
        
        let bossQuote = getBossQuote ENG difficulty (enemy.Score > player.Score)
        let bossName = getBossName ENG difficulty
        if bossQuote <> "" then printfn " [Boss %s] : \"%s\"\n" bossName bossQuote

        if player.Score > enemy.Score then printfn "        WINNER: Player! Congratulations!"
        elif enemy.Score > player.Score then printfn "        WINNER: AI! Better luck next time..."
        else printfn "        It's a DRAW!"
    else
        printfn "                   최종 게임 결과 요약                 "
        printfn "=======================================================\n"
        printfn "[ 승부 결과 ]"
        history |> List.iteri (fun i res -> printfn " %d 라운드 : %s" (i + 1) res)
        
        let pAbilName = 
            if player.Abilities.IsEmpty then "없음" 
            else player.Abilities |> List.map (fun a -> fst (abilityDetails KOR a)) |> String.concat "\n             "
        printfn "\n[ 당신의 최종 상태 ]"
        printfn " 획득 능력 : %s" pAbilName
        printfn " 최종 승점 : %d점%s" player.Score pFire
        printfn " 최종 덱   : 총 %d장" (List.length player.Deck)
        printCards KOR (sortCards player.Deck)

        let eAbilName = 
            if enemy.Abilities.IsEmpty then "없음" 
            else enemy.Abilities |> List.map (fun a -> fst (abilityDetails KOR a)) |> String.concat "\n             "
        printfn "\n[ AI의 최종 상태 ]"
        printfn " 획득 능력 : %s" eAbilName
        printfn " 최종 승점 : %d점%s" enemy.Score eFire
        printfn " 최종 덱   : 총 %d장" (List.length enemy.Deck)
        printCards KOR (sortCards enemy.Deck)

        printfn "\n======================================================="
        
        let bossQuote = getBossQuote KOR difficulty (enemy.Score > player.Score)
        let bossName = getBossName KOR difficulty
        if bossQuote <> "" then printfn " [%s] : \"%s\"\n" bossName bossQuote

        if player.Score > enemy.Score then printfn "         최종 승리자: 당신입니다! 축하합니다!"
        elif enemy.Score > player.Score then printfn "         최종 승리자: AI입니다! 다음 기회에..."
        else printfn "         무승부로 게임이 종료되었습니다!"
    printfn "=======================================================\n"

let rec gameLoop lang round difficulty player enemy history =
    let maxRound = if difficulty = Aion then 3 else 5
    if round > maxRound then 
        let aCountP = if List.contains AChance player.Abilities then player.Deck |> List.filter (fun c -> c.Rank = Ace) |> List.length else 0
        let aCountE = if List.contains AChance enemy.Abilities then enemy.Deck |> List.filter (fun c -> c.Rank = Ace) |> List.length else 0
        
        let finalPlayer = { player with Score = player.Score + aCountP }
        let finalEnemy = { enemy with Score = enemy.Score + aCountE }
            
        printGameOverSummary lang finalPlayer finalEnemy difficulty history
        (finalPlayer, finalEnemy) 
    else
        printfn "\n===================="
        if lang = ENG then printfn "ROUND %d" round else printfn "%d 라운드" round
        printfn "===================="

        let player = 
            if difficulty = Behemoth then
                if lang = ENG then printfn "\n🐾 [Behemoth] The Beast King devours 4 cards from your deck!" 
                else printfn "\n🐾 [베히모스] 백수의 왕이 당신의 덱에서 4장의 카드를 무작위로 제거합니다!"
                
                let toRemove = player.Deck |> shuffle |> List.truncate 4
                let tempDeck = removeCards player.Deck toRemove
                
                let finalDeck = 
                    if tempDeck.Length < 8 then
                        if lang = ENG then printfn "🐾 [Behemoth] Replenishing deck to 8 cards..." 
                        else printfn "🐾 [베히모스] 덱이 8장 미만으로 감소하여 공용 덱에서 무작위 카드가 보충됩니다!"
                        let needed = 8 - tempDeck.Length
                        let wild = removeCardsFromPool (createDoubleDeck()) (tempDeck @ enemy.Deck) |> shuffle |> List.truncate needed
                        tempDeck @ wild
                    else tempDeck
                { player with Deck = finalDeck }
            else player

        if lang = ENG then printfn "--- Your Current Deck ---" else printfn "--- 현재 당신의 덱 목록 ---"
        printCards lang (sortCards player.Deck)

        let getViewCount () =
            if difficulty = Venus then
                if lang = ENG then printfn "\n👁️ [Venus] Your vision is blinded to 3 cards!" else printfn "\n👁️ [베누스] 심연의 왕에 의해 시야가 3장으로 제한됩니다!"
                3
            elif List.contains Prudence player.Abilities then 7
            elif List.contains HandOfFate player.Abilities then 8
            elif List.contains Minimalist player.Abilities then 4
            else 5

        let player, enemy = 
            if List.contains SpareChange player.Abilities then
                announceAbility lang true SpareChange
                if lang = ENG then printf "Skip Refine for 1 Score? (y/n): " else printf "정제를 건너뛰고 승점 1점을 얻을까요? (y/n): "
                if System.Console.ReadLine().ToLower() = "y" then { player with Score = player.Score + 1 }, enemy
                else 
                    let p1, _ = 
                        if difficulty = Sylphid then
                            if lang = ENG then printfn "\n🌿 [Sylphid] Remove phase is disabled by the Forest Queen!" else printfn "\n🌿 [실피드] 숲의 여왕에 의해 카드를 제거할 수 없습니다!"
                            player, []
                        else removePhase lang player (getViewCount ())
                    p1, enemy
            else 
                let p1, discardedCards = 
                    if difficulty = Sylphid then
                        if lang = ENG then printfn "\n🌿 [Sylphid] Remove phase is disabled by the Forest Queen!" else printfn "\n🌿 [실피드] 숲의 여왕에 의해 카드를 제거할 수 없습니다!"
                        player, []
                    else removePhase lang player (getViewCount ())
                
                let p2, e1 = 
                    if List.contains Steal p1.Abilities then
                        announceAbility lang true Steal
                        if lang = ENG then printfn "Steal from AI:" else printfn "AI의 덱에서 훔쳐올 카드를 선택하세요:"
                        let enemyShown = enemy.Deck |> shuffle |> List.truncate 5
                        printCards lang enemyShown
                        
                        let stealIdx = getSelections lang [] 0 1 (List.length enemyShown) |> List.tryHead
                        match stealIdx with
                        | Some i when i > 0 && i <= 5 -> 
                            let stolen = enemyShown.[i-1]
                            { p1 with Deck = p1.Deck @ [stolen] }, { enemy with Deck = removeCards enemy.Deck [stolen] }
                        | _ -> p1, enemy
                    else p1, enemy
                
                let pool = removeCardsFromPool (createDoubleDeck()) (p2.Deck @ e1.Deck)
                let sortedPool = 
                    if List.contains FourLeafClover p2.Abilities then pool |> List.sortByDescending (fun c -> if c.Suit = Clubs then 1 else 0)
                    elif List.contains RoyalFamily p2.Abilities then pool |> List.sortByDescending (fun c -> if c.Rank = King || c.Rank = Queen || c.Rank = Jack then 1 else 0)
                    else pool |> shuffle

                let commCount = if List.contains Stand p2.Abilities then 7 else (getViewCount ())
                let baseCommunity = sortedPool |> List.take commCount
                let finalCommunity = if List.contains Recycling p2.Abilities then discardedCards @ baseCommunity else baseCommunity
                
                let p3 = addPhase lang p2 finalCommunity
                p3, e1

        let enemyShuffled = enemy.Deck |> shuffle
        let enemyToRemove = chooseRemoveCards difficulty enemy.Abilities player.Abilities (enemyShuffled |> List.truncate 5) enemy.Deck
        let e2 = { enemy with Deck = removeCards enemy.Deck enemyToRemove }

        let ePool = removeCardsFromPool (createDoubleDeck()) (player.Deck @ e2.Deck) |> shuffle
        let eCommunity = ePool |> List.take 5
        let enemyToAdd = chooseAddCards difficulty e2.Abilities player.Abilities eCommunity e2.Deck
        let e3 = { e2 with Deck = e2.Deck @ enemyToAdd }

        let enemy, player = 
            if List.contains Steal e3.Abilities then
                announceAbility lang false Steal
                let pShown = player.Deck |> shuffle |> List.truncate 5
                if pShown.Length > 0 then
                    let stolen = pShown.[System.Random.Shared.Next(pShown.Length)]
                    { e3 with Deck = e3.Deck @ [stolen] }, { player with Deck = removeCards player.Deck [stolen] }
                else e3, player
            else e3, player

        let player =
            if List.contains AnotherPossibility player.Abilities then
                announceAbility lang true AnotherPossibility
                if lang = ENG then printfn "1. Remove\n2. Add\n3. Skip" else printfn "1. 한 번 더 제거\n2. 한 번 더 추가\n3. 건너뛰기"
                let rec getExtra() =
                    match System.Console.ReadLine() with
                    | "1" -> let p, _ = removePhase lang player 5 in p
                    | "2" -> let np = removeCardsFromPool (createDoubleDeck()) (player.Deck @ enemy.Deck) in addPhase lang player (np |> shuffle |> List.take 5)
                    | "3" -> player
                    | _ -> getExtra()
                getExtra()
            else player

        let player, enemy, roundRes = duel lang round difficulty player enemy
        let newHistory = history @ [roundRes]

        let abilityRound = if difficulty = Aion then 1 else 2
        let player =
            if round = abilityRound || (List.contains Marshmallow player.Abilities && (round = abilityRound + 1 || round = abilityRound + 2)) then
                if List.contains Marshmallow player.Abilities && round <> abilityRound then announceAbility lang true Marshmallow
                let ab = acquireRandomAbility lang player.Abilities
                
                let pWithAb = 
                    match ab with
                    | Embargo _ -> 
                        let choice = 
                            if lang = ENG then
                                printfn "\nSelect a hand rank to Embargo (make it the lowest):"
                                printfn "1. One Pair   2. Two Pair   3. Triple"
                                printfn "4. Straight   5. Flush      6. Full House"
                                printfn "7. Four Card  8. Straight Flush   9. Royal Flush"
                                printf "Choice (1-9): "
                            else
                                printfn "\n금지할 족보를 선택하세요 (가장 낮은 랭크로 강등됩니다):"
                                printfn "1. 원페어   2. 투페어   3. 트리플"
                                printfn "4. 스트레이트   5. 플러시   6. 풀하우스"
                                printfn "7. 포카드   8. 스트레이트 플러시   9. 로열 플러시"
                                printf "선택 (1-9): "
                            match System.Console.ReadLine() with
                            | "1" -> "OnePair" | "2" -> "TwoPair" | "3" -> "Triple" | "4" -> "Straight" | "5" -> "Flush"
                            | "6" -> "FullHouse" | "7" -> "FourCard" | "8" -> "StraightFlush" | "9" -> "RoyalFlush"
                            | _ -> "OnePair"
                        { player with Abilities = Embargo choice :: player.Abilities }
                    | _ -> { player with Abilities = ab :: player.Abilities }

                match ab with
                | LesMiserables -> 
                    announceAbility lang true LesMiserables
                    { pWithAb with Deck = pWithAb.Deck |> List.filter (fun c -> rankValue c.Rank < 11) }
                | SoulScissors ->
                    announceAbility lang true SoulScissors
                    let p, _ = removePhase lang pWithAb 5
                    p
                | GiantPouch ->
                    announceAbility lang true GiantPouch
                    let pool1 = removeCardsFromPool (createDoubleDeck()) (pWithAb.Deck @ enemy.Deck)
                    let p1 = addPhase lang pWithAb (pool1 |> shuffle |> List.take 5)
                    let pool2 = removeCardsFromPool (createDoubleDeck()) (p1.Deck @ enemy.Deck)
                    addPhase lang p1 (pool2 |> shuffle |> List.take 5)
                | Minimalist ->
                    announceAbility lang true Minimalist
                    let p, _ = removePhase lang pWithAb 5
                    p
                | Forging ->
                    announceAbility lang true Forging
                    if lang = ENG then printfn "--- Your Entire Deck ---" else printfn "--- 당신의 전체 덱 ---"
                    let sortedDeck = sortCards pWithAb.Deck
                    printCards lang sortedDeck

                    let rec getTwoIndices () =
                        if lang = ENG then printfn "Select 2 cards to replace (Enter 1st number -> Enter -> 2nd number -> Enter):"
                        else printfn "교체할 카드 2장의 번호를 입력하세요 (번호 하나씩 입력 후 엔터):"
                        let i1 = match System.Int32.TryParse(System.Console.ReadLine()) with true, v -> v | _ -> 0
                        let i2 = match System.Int32.TryParse(System.Console.ReadLine()) with true, v -> v | _ -> 0
                        if i1 > 0 && i1 <= List.length sortedDeck && i2 > 0 && i2 <= List.length sortedDeck && i1 <> i2 then (i1 - 1, i2 - 1)
                        else getTwoIndices ()
                    
                    let idx1, idx2 = getTwoIndices ()
                    let toRemoveIndices = Set.ofList [idx1; idx2]

                    let rec getSuit num =
                        if lang = ENG then printf "[Card %d] Suit (1:Hearts, 2:Diamonds, 3:Clubs, 4:Spades): " num
                        else printf "[%d번째 카드] 문양 선택 (1:하트, 2:다이아, 3:클로버, 4:스페이드): " num
                        match System.Console.ReadLine() with
                        | "1" -> Hearts | "2" -> Diamonds | "3" -> Clubs | "4" -> Spades | _ -> getSuit num

                    let rec getRank num =
                        if lang = ENG then printf "[Card %d] Rank (2~10, J, Q, K, A): " num
                        else printf "[%d번째 카드] 숫자 선택 (2~10, J, Q, K, A): " num
                        match System.Console.ReadLine().ToUpper() with
                        | "J" -> Jack | "Q" -> Queen | "K" -> King | "A" -> Ace
                        | "2" -> Two | "3" -> Three | "4" -> Four | "5" -> Five | "6" -> Six
                        | "7" -> Seven | "8" -> Eight | "9" -> Nine | "10" -> Ten | _ -> getRank num

                    let c1 = { Suit = getSuit 1; Rank = getRank 1 }
                    let c2 = { Suit = getSuit 2; Rank = getRank 2 }
                    let kept = sortedDeck |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> not (Set.contains i toRemoveIndices)) |> List.map snd
                    { pWithAb with Deck = kept @ [c1; c2] }
                | _ -> 
                    { pWithAb with Score = pWithAb.Score + (if ab = GoldPile then 2 else 0) }
            else player

        let enemy =
            if difficulty = Mirage && round = 2 then
                if lang = ENG then printfn "\n🪞 [Mirage] The Mirror Queen synchronizes with your soul!"
                else printfn "\n🪞 [미라쥬] 거울의 여왕이 당신의 덱과 능력을 완벽히 복제합니다!"
                { enemy with Deck = player.Deck; Abilities = player.Abilities }
                
            else
                let isMachinaTurn = difficulty = Machina
                let isNormalAITurn = round = abilityRound || (List.contains Marshmallow enemy.Abilities && (round = abilityRound + 1 || round = abilityRound + 2))
                
                if isMachinaTurn || isNormalAITurn then
                    if difficulty = Machina && round <> abilityRound then
                        if lang = ENG then printfn "\n⚙️ [Machina's Upgrade] Stacking new core module..." 
                        else printfn "\n⚙️ [마키나 진화] 기계의 왕이 새로운 코어 모듈을 중첩하여 장착합니다..."
                    elif List.contains Marshmallow enemy.Abilities && round <> abilityRound then announceAbility lang false Marshmallow
                    
                    let aiChoices = generateAbilityChoices enemy.Abilities
                    
                    let aiAb = 
                        match difficulty with
                        | Beginner -> aiChoices.[System.Random.Shared.Next(4)]
                        | Intermediate -> aiChoices |> List.sortByDescending getStars |> List.head
                        | _ -> 
                            let maxStar = aiChoices |> List.map getStars |> List.max
                            let candidates = aiChoices |> List.filter (fun ab -> getStars ab = maxStar)
                            
                            if candidates.Length = 1 then candidates.[0]
                            else
                                candidates |> List.maxBy (fun ab ->
                                    // 🌟 FIX: 안전하게 괄호 처리로 파이프라인 에러 방지
                                    let testEnemy = { enemy with Abilities = ab :: enemy.Abilities }
                                    let drawCount = if List.contains MakeFriends testEnemy.Abilities then 6 else 5
                                    let sims = 
                                        [1..15] |> List.map (fun _ ->
                                            let hand = testEnemy.Deck |> shuffle |> List.truncate drawCount
                                            let finalHand, _ = doMulliganAI Advanced testEnemy player.Abilities hand drawCount
                                            rankToScore difficulty (evaluateHand testEnemy.Abilities player.Abilities finalHand)
                                        )
                                    List.average sims
                                )
                    
                    announceAbility lang false aiAb
                    
                    let eWithAb = 
                        match aiAb with
                        | Embargo _ -> { enemy with Abilities = Embargo "OnePair" :: enemy.Abilities }
                        | _ -> { enemy with Abilities = aiAb :: enemy.Abilities }

                    match aiAb with
                    | LesMiserables -> 
                        { eWithAb with Deck = eWithAb.Deck |> List.filter (fun c -> rankValue c.Rank < 11) }
                    | SoulScissors -> 
                        let toRemove = chooseRemoveCards difficulty eWithAb.Abilities player.Abilities (eWithAb.Deck |> shuffle |> List.truncate 5) eWithAb.Deck
                        { eWithAb with Deck = removeCards eWithAb.Deck toRemove }
                    | GiantPouch -> 
                        let pool = removeCardsFromPool (createDoubleDeck()) (player.Deck @ enemy.Deck) |> shuffle
                        { eWithAb with Deck = eWithAb.Deck @ (pool |> List.truncate 10) }
                    | Minimalist ->
                        let toRemove = chooseRemoveCards difficulty eWithAb.Abilities player.Abilities (eWithAb.Deck |> shuffle |> List.truncate 8) eWithAb.Deck
                        let actualRemove = toRemove |> List.truncate 4
                        { eWithAb with Deck = removeCards eWithAb.Deck actualRemove }
                    | Forging ->
                        let sorted = sortCards eWithAb.Deck
                        let kept = sorted |> List.skip 2
                        let newCards = [{Suit=Spades; Rank=Ace}; {Suit=Hearts; Rank=Ace}]
                        { eWithAb with Deck = kept @ newCards }
                    | _ -> 
                        { eWithAb with Score = eWithAb.Score + (if aiAb = GoldPile then 2 else 0) }
                else enemy

        gameLoop lang (round + 1) difficulty player enemy newHistory