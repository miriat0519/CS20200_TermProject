module Ability

open Types
open System

let abilityDetails lang ab =
    match lang, ab with
    | KOR, ColorPlay -> "색칠놀이 [★ ★ ★ ★ ]", "스페이드와 클로버 / 하트와 다이아몬드를 같은 문양으로 취급합니다."
    | ENG, ColorPlay -> "Color Play [★ ★ ★ ★ ]", "Spades/Clubs and Hearts/Diamonds act as the same suit."
    | KOR, Gemstone -> "보석과 원석 [★ ★ ★ ★ ]", "내 패의 '다이아몬드'는 모든 문양으로 취급합니다."
    | ENG, Gemstone -> "Gemstone [★ ★ ★ ★ ]", "Your Diamonds act as wildcards for suits."
    | KOR, FlushBros -> "플러시 4형제 [★ ★ ★ ]", "같은 문양 4장으로도 플러시가 완성됩니다."
    | ENG, FlushBros -> "Flush Bros [★ ★ ★ ]", "A Flush can be made with only 4 cards."
    | KOR, StraightBros -> "스트레이트 4형제 [★ ★ ★ ]", "연속된 4장으로도 스트레이트가 완성됩니다."
    | ENG, StraightBros -> "Straight Bros [★ ★ ★ ]", "A Straight can be made with only 4 consecutive cards."
    | KOR, Promotion -> "프로모션 [★ ★ ★ ★ ★ ]", "숫자 카드(2~10)를 원래 숫자 또는 한 단계 높은 숫자로 유리하게 취급합니다."
    | ENG, Promotion -> "Promotion [★ ★ ★ ★ ★ ]", "Number cards (2-10) can be treated as their original rank or +1 rank."
    | KOR, Arithmetic -> "등차수열 [★ ★ ]", "스트레이트가 '등차수열' 5장으로도 완성됩니다."
    | ENG, Arithmetic -> "Arithmetic [★ ★ ]", "A Straight is valid with an arithmetic progression of 5 cards."
    | KOR, LastHope -> "마지막 희망 [★ ★ ★ ]", "로열/스트레이트 플러시 승리 시 즉시 최종 승리합니다."
    | ENG, LastHope -> "Last Hope [★ ★ ★ ]", "Instantly win the entire game if you win a duel with a Royal/Straight Flush."
    | KOR, Protagonist -> "주인공 [★ ★ ★ ★ ★ ]", "하이카드를 투페어보다 높은 족보로 취급합니다."
    | ENG, Protagonist -> "Protagonist [★ ★ ★ ★ ★ ]", "High Card acts as a rank higher than Two Pair."
    | KOR, Footstool -> "발받침 [★ ]", "동점 상황 시 내가 무조건 승리합니다."
    | ENG, Footstool -> "Footstool [★ ]", "You win all tie-breakers."
    | KOR, RoyalDignity -> "왕족의 품위 [★ ★ ]", "K, Q, J로만 족보 완성 시 한 단계 상승합니다."
    | ENG, RoyalDignity -> "Royal Dignity [★ ★ ]", "Hands made only of K, Q, J are upgraded by one rank."
    | KOR, Gift -> "하사품 [★ ★ ★ ]", "K, Q, J로만 승리 시 승점을 2배로 받습니다."
    | ENG, Gift -> "Gift [★ ★ ★ ]", "Gain double score points if you win with only K, Q, J."
    | KOR, Twins -> "쌍둥이 [★ ★ ]", "원페어, 투페어로 승리 시 추가 승점 2점을 얻습니다."
    | ENG, Twins -> "Twins [★ ★ ]", "Win with One Pair or Two Pair to gain +2 extra points."
    | KOR, BonusPoint -> "보너스 포인트 [★ ]", "승리 시 0~2점의 무작위 추가 승점을 얻습니다."
    | ENG, BonusPoint -> "Bonus Point [★ ]", "Gain 0 to 2 random extra points upon winning."
    | KOR, Stand -> "가판대 [★ ★ ★ ]", "정제 페이즈에서 커뮤니티 카드를 7장 봅니다."
    | ENG, Stand -> "Stand [★ ★ ★ ]", "View 7 community cards instead of 5 during the Add phase."
    | KOR, SpareChange -> "잔돈 [★ ]", "정제를 포기하고 승점 1점을 얻을 수 있습니다."
    | ENG, SpareChange -> "Spare Change [★ ]", "You may skip the entire Refine phase to gain +1 point."
    | KOR, GoldPile -> "황금더미 [★ ]", "즉시 승점 2점을 획득합니다."
    | ENG, GoldPile -> "Gold Pile [★ ]", "Instantly gain +2 points upon acquiring this ability."
    | KOR, Housekeeper -> "환상의 호흡 [★ ★ ★ ]", "투페어를 플러시보다 높은 족보로 취급합니다."
    | ENG, Housekeeper -> "Fantastic Duo [★ ★ ★ ]", "Two Pair acts as a rank higher than Flush."
    | KOR, FifthStar -> "다섯번째 별 [★ ]", "5장이 모두 같은 숫자면 로열 플러시로 취급합니다."
    | ENG, FifthStar -> "Fifth Star [★ ]", "5 of a kind acts as a Royal Flush."
    | KOR, MakeFriends -> "친구 만들기 [★ ★ ★ ★ ]", "듀얼에서 6장을 뽑습니다."
    | ENG, MakeFriends -> "Make Friends [★ ★ ★ ★ ]", "Draw 6 cards instead of 5 during the Duel phase."
    | KOR, Tax -> "세금 [★ ★ ★ ★ ]", "상대가 승점 획득 시 1점을 빼앗아 옵니다."
    | ENG, Tax -> "Tax [★ ★ ★ ★ ]", "Steal 1 point from the opponent whenever they score."
    | KOR, LuckySeven -> "럭키 세븐 [★ ★ ]", "하이 카드로 승리 시 승점 7점을 얻습니다."
    | ENG, LuckySeven -> "Lucky Seven [★ ★ ]", "Win with a High Card to gain +7 extra points."
    | KOR, Jackpot -> "잭팟 [★ ★ ]", "7 트리플을 로열 플러시로 취급합니다."
    | ENG, Jackpot -> "Jackpot [★ ★ ]", "A Triple of 7s acts as a Royal Flush."
    | KOR, RoyalFamily -> "황족 [★ ★ ]", "덱에 추가될 카드에 K, Q, J가 우선 등장합니다."
    | ENG, RoyalFamily -> "Royal Family [★ ★ ]", "K, Q, J are prioritized in the community card pool."
    | KOR, SmallEvolution -> "소형 진화 [★ ★ ]", "투페어를 트리플로 취급합니다."
    | ENG, SmallEvolution -> "Small Evolution [★ ★ ]", "Two Pair acts as a Triple."
    | KOR, AnotherPossibility -> "또다른 가능성 [★ ★ ★ ★ ★ ]", "제거 및 추가 진행 후, 한 번 더 행동합니다."
    | ENG, AnotherPossibility -> "Another Possibility [★ ★ ★ ★ ★ ]", "Take one extra refine action (Remove or Add) after the normal refine phase."
    | KOR, Housekeeper2 -> "하우스키퍼 [★ ★ ★ ★ ★ ]", "손패가 정확히 두 가지 문양으로만 이루어져 있다면 플러시로 취급합니다."
    | ENG, Housekeeper2 -> "Housekeeper [★ ★ ★ ★ ★ ]", "If your hand has exactly two suits, it acts as a Flush."
    | KOR, Steal -> "슬쩍하기 [★ ★ ★ ★ ]", "정제 후 상대 덱을 5장 보고 1장을 훔쳐옵니다. (0으로 취소)"
    | ENG, Steal -> "Steal [★ ★ ★ ★ ]", "Look at 5 cards from AI's deck and steal 1 after refine. (0 to cancel)"
    | KOR, GiantPouch -> "거대한 주머니 [★ ★ ★ ★ ]", "획득 즉시 추가 페이즈를 2회 진행합니다."
    | ENG, GiantPouch -> "Giant Pouch [★ ★ ★ ★ ]", "Immediately gain 2 extra Add phases upon acquiring."
    | KOR, SoulScissors -> "영혼가위 [★ ★ ★ ★ ]", "획득 즉시 제거 페이즈를 1회 진행합니다."
    | ENG, SoulScissors -> "Soul Scissors [★ ★ ★ ★ ]", "Immediately gain 1 extra Remove phase upon acquiring."
    | KOR, SecondChance -> "세컨드 찬스 [★ ★ ★ ]", "듀얼 페이즈에서 멀리건(교체)을 2번 진행합니다."
    | ENG, SecondChance -> "Second Chance [★ ★ ★ ]", "Allows 2 mulligans instead of 1 during the Duel phase."
    | KOR, DisguisedJack -> "변장술사 잭 [★ ★ ★ ★ ]", "내 J를 가장 유리한 숫자로 취급합니다."
    | ENG, DisguisedJack -> "Disguised Jack [★ ★ ★ ★ ]", "Your J acts as the most advantageous rank in your hand."
    | KOR, FirstImpression -> "첫인상 [★ ]", "멀리건을 진행하지 않고 승리하면 추가 승점 2점을 얻습니다."
    | ENG, FirstImpression -> "First Impression [★ ]", "Gain +2 extra points if you win without using a mulligan."
    | KOR, Disconnection -> "단절 [★ ]", "듀얼에서 교체(멀리건)한 카드는 덱에서 영구히 제거됩니다."
    | ENG, Disconnection -> "Disconnection [★ ]", "Cards replaced during mulligan are permanently removed from your deck."
    | KOR, LoneWolves -> "고독한 늑대들 [★ ★ ★ ★ ★ ]", "내 패의 카드 숫자가 전부 다르다면 트리플 랭크로 취급합니다."
    | ENG, LoneWolves -> "Lone Wolves [★ ★ ★ ★ ★ ]", "If all 5 cards have different ranks, it acts as a Triple."
    | KOR, Minimalist -> "미니멀리스트 [★ ★ ★ ★ ]", "정제를 4장만 봅니다. 획득 즉시 덱에서 4장을 골라 제거합니다."
    | ENG, Minimalist -> "Minimalist [★ ★ ★ ★ ]", "View only 4 refine cards. Instantly remove 4 chosen cards from your deck."
    | KOR, LesMiserables -> "레 미제라블 [★ ★ ★ ★ ]", "획득 즉시 덱에서 J, Q, K, A를 전부 제거합니다."
    | ENG, LesMiserables -> "Les Misérables [★ ★ ★ ★ ]", "Immediately remove all J, Q, K, and A from your deck upon acquiring."
    | KOR, BackToTheWall -> "배수의 진 [★ ]", "서로 듀얼 페이즈에서 멀리건을 진행할 수 없습니다."
    | ENG, BackToTheWall -> "Back to the Wall [★ ]", "Neither you nor the AI can use mulligan during duels."
    | KOR, Prudence -> "신중함 [★ ★ ★ ]", "제거 단계에서 내 덱을 5장이 아닌 7장 확인하고 제거합니다."
    | ENG, Prudence -> "Prudence [★ ★ ★ ]", "View 7 cards instead of 5 during your Remove phase."
    | KOR, AChance -> "A Chance [★ ★ ]", "게임 종료 시 내 덱의 A 카드 1장당 승점 1점을 얻습니다."
    | ENG, AChance -> "A Chance [★ ★ ]", "Gain +1 final score for every Ace remaining in your deck at the end of the game."
    | KOR, Marshmallow -> "마시멜로우 [★ ★ ★ ★ ★ ]", "3라운드, 4라운드 종료 시 추가 능력을 또 선택합니다."
    | ENG, Marshmallow -> "Marshmallow [★ ★ ★ ★ ★ ]", "Gain an extra ability selection after Rounds 3 and 4."
    | KOR, BlackAndWhite -> "흑과 백 [★ ★ ★ ★ ]", "5장이 모두 짝수거나 홀수면 플러시로 취급합니다."
    | ENG, BlackAndWhite -> "Black and White [★ ★ ★ ★ ]", "If all 5 cards are even or all are odd, it acts as a Flush."
    | KOR, FourLeafClover -> "네잎클로버 [★ ★ ]", "커뮤니티 카드에 클로버가 우선 등장합니다."
    | ENG, FourLeafClover -> "Four-Leaf Clover [★ ★ ]", "Clubs are prioritized to appear in the community pool."
    | KOR, GrandFinale -> "대단원 [★ ★ ]", "마지막 5라운드 승리 시 추가 승점 4점을 얻습니다."
    | ENG, GrandFinale -> "Grand Finale [★ ★ ]", "Gain +4 extra points if you win the final 5th round duel."
    | KOR, Joker -> "조커 [★ ★ ★ ★ ★ ]", "내 패의 첫 번째 카드는 나에게 가장 유리한 조커로 변환됩니다."
    | ENG, Joker -> "Joker [★ ★ ★ ★ ★ ]", "The first card in your hand automatically transforms into the best possible Joker."
    | KOR, Recycling -> "재활용 [★ ★ ★ ]", "제거 단계에 버린 카드가 다음 내 추가 단계 풀에 커뮤니티 카드 5장과 함께 합류합니다."
    | ENG, Recycling -> "Recycling [★ ★ ★ ]", "Cards discarded during Remove phase join your Add phase community pool."
    | KOR, Oblivion -> "망각 [★ ]", "듀얼에서 최종적으로 사용한 카드가 덱에서 제거됩니다."
    | ENG, Oblivion -> "Oblivion [★ ]", "Cards used in your final hand during a Duel are permanently removed from your deck."
    | KOR, CrowIdol -> "까마귀 우상 [★ ★ ★ ]", "듀얼 시 상대 덱에서 1장을 뽑아 내 패로 씁니다."
    | ENG, CrowIdol -> "Crow Idol [★ ★ ★ ]", "Draw and use 1 card from AI's deck during Duel."
    | KOR, Embargo _ -> "금지령 [★ ★ ★ ]", "획득 시 족보 1개를 선택하여 해당 족보를 가장 낮은 랭크로 만듭니다."
    | ENG, Embargo _ -> "Embargo [★ ★ ★ ]", "Select one hand rank upon acquiring; it becomes the lowest possible rank."
    | KOR, HandOfFate -> "운명의 패 [★ ★ ★ ]", "멀리건 불가. 대신 정제 시 카드를 5장 아닌 8장을 봅니다."
    | ENG, HandOfFate -> "Hand of Fate [★ ★ ★ ]", "Mulligan is disabled. Instead, view 8 cards during Refine phases."
    | KOR, Forging -> "단조 [★ ★ ★ ★ ★ ]", "획득 즉시 내 덱 전체에서 원하는 카드 2장을 골라 각각 원하는 카드로 교체합니다."
    | ENG, Forging -> "Forging [★ ★ ★ ★ ★ ]", "Instantly choose 2 cards from your entire deck and replace them with any cards you want."

let allAbilities = 
    [ ColorPlay; Gemstone; FlushBros; StraightBros; Promotion; Arithmetic; LastHope; Protagonist;
      Footstool; RoyalDignity; Gift; Twins; BonusPoint; Stand; SpareChange; GoldPile; Housekeeper;
      FifthStar; MakeFriends; Tax; LuckySeven; Jackpot; RoyalFamily; SmallEvolution; AnotherPossibility;
      Housekeeper2; Steal; GiantPouch; SoulScissors; SecondChance; DisguisedJack; FirstImpression;
      Disconnection; LoneWolves; Minimalist; LesMiserables; BackToTheWall; Prudence; AChance;
      Marshmallow; BlackAndWhite; FourLeafClover; GrandFinale; Joker; Recycling; Oblivion;
      CrowIdol; Embargo ""; HandOfFate; Forging ]

let getStars ab =
    match ab with
    | LoneWolves | Protagonist | Joker | Promotion | Housekeeper2 | AnotherPossibility | Marshmallow | Forging -> 5
    | ColorPlay | MakeFriends | Gemstone | BlackAndWhite | DisguisedJack | LesMiserables | Minimalist | Steal | GiantPouch | SoulScissors | Tax -> 4
    | Housekeeper | FlushBros | StraightBros | Arithmetic | LastHope | RoyalDignity | Gift | Stand | Recycling | CrowIdol | Embargo "" | HandOfFate | SecondChance | Prudence -> 3
    | SmallEvolution | Jackpot | RoyalFamily | Twins | LuckySeven | AChance | FourLeafClover | GrandFinale -> 2
    | FifthStar | Footstool | BonusPoint | SpareChange | GoldPile | FirstImpression | Disconnection | BackToTheWall | Oblivion -> 1
    | _ -> 3

let getAbilitiesByStar star =
    allAbilities |> List.filter (fun ab -> getStars ab = star)

let getRandomStar () =
    let r = Random.Shared.NextDouble() * 100.0
    if r < 5.0 then 5
    elif r < 20.0 then 4
    elif r < 50.0 then 3
    elif r < 85.0 then 2
    else 1

let announceAbility lang isPlayer ab =
    let name, desc = abilityDetails lang ab
    if lang = ENG then
        let owner = if isPlayer then "Your" else "AI's"
        printfn "\n♛ %s ability \"%s\" has been activated!\n -> %s" owner name desc
    else
        let owner = if isPlayer then "당신의" else "AI의"
        printfn "\n♛ %s 능력 \"%s\"가 발동했습니다!\n -> %s" owner name desc

let rec chooseAbility lang choices =
    if lang = ENG then printf "Enter choice (1, 2, 3, 4): "
    else printf "번호를 입력하여 선택하세요 (1, 2, 3, 4): "
    
    match System.Console.ReadLine() with
    | "1" -> List.item 0 choices
    | "2" -> List.item 1 choices
    | "3" -> List.item 2 choices
    | "4" -> List.item 3 choices
    | _ ->
        if lang = ENG then printfn "Invalid input." else printfn "잘못된 입력입니다."
        chooseAbility lang choices

// 🌟 FIX: 금지령(Embargo)처럼 내부 텍스트가 달라도 같은 능력으로 판별하는 함수
let hasAbility ab ownedList =
    match ab with
    | Embargo _ -> ownedList |> List.exists (function Embargo _ -> true | _ -> false)
    | _ -> List.contains ab ownedList

// 🌟 FIX: 보유 중인 능력(owned)을 확인하여 절대로 중복되지 않게 후보 생성
let generateAbilityChoices (owned: Ability list) =
    let rec selectUnique acc count =
        if count = 0 then acc
        else
            let star = getRandomStar ()
            let pool = getAbilitiesByStar star
            let available = pool |> List.filter (fun ab -> not (hasAbility ab acc) && not (hasAbility ab owned))
            
            let selected = 
                if List.isEmpty available then 
                    // 1차 방어: 3성 능력 중 없는 것 배정
                    let fallback = getAbilitiesByStar 3 |> List.filter (fun ab -> not (hasAbility ab acc) && not (hasAbility ab owned))
                    if fallback.IsEmpty then 
                        // 2차 방어: 전체 능력 중 없는 것 무작위 배정
                        let ult = allAbilities |> List.filter (fun ab -> not (hasAbility ab acc) && not (hasAbility ab owned))
                        if ult.IsEmpty then Stand else ult.[0] // 극단적 예외 상황 방어
                    else fallback.[Random.Shared.Next(List.length fallback)]
                else available.[Random.Shared.Next(List.length available)]
            selectUnique (selected :: acc) (count - 1)
    selectUnique [] 4

let acquireRandomAbility lang owned =
    let choices = generateAbilityChoices owned

    if lang = ENG then
        printfn "\n=============================================="
        printfn "♛ Select Special Ability (Choose 1 of 4) ♛"
        printfn "=============================================="
    else
        printfn "\n=============================================="
        printfn "♛ 특수 능력 선택 (4가지 중 1개 획득) ♛"
        printfn "=============================================="
    
    choices |> List.iteri (fun i ab ->
        let name, desc = abilityDetails lang ab
        printfn "[%d] %s\n    -> %s\n" (i + 1) name desc
    )
    chooseAbility lang choices