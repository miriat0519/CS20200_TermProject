open Game
open AI
open Types
open System

let rec getLanguage () =
    printfn "=== Select Language / 언어 선택 ==="
    printfn "1. English"
    printfn "2. 한국어"
    printf "Enter choice (1 or 2): "
    match Console.ReadLine() with
    | "1" -> ENG | "2" -> KOR | _ -> getLanguage ()

let rec getDifficulty lang =
    if lang = ENG then
        printfn "\n=== Select AI Difficulty ==="
        printfn "1. Beginner (Random)"
        printfn "2. Intermediate (Simulated)"
        printfn "3. Advanced (Calculated)\n\n=== CHALLENGE BOSS ==="
        printfn "4. Boss: Machina (Enemy starts with a new random ability each turn)"
        printfn "5. Boss: Stella (Enemy starts with 4 points and a special ability)"
        printfn "6. Boss: Adalan (Enemy drafts 20 cards from 40 to create starting deck)"
        printfn "7. Boss: Mirage (Enemy copies your deck & abilities at end of round)"
        printfn "8. Boss: Goliath (Enemy always shows 7 cards to decide)"
        printfn "9. Boss: Venus (Enemy only shows 3 cards during refine phase)"
        printfn "10. Boss: Goldwing (Enemy wins 2 points per round but loses points on mulligan)"
        printfn "11. Boss: Delta (Hand rankings are completely reversed)"
        printfn "12. Boss: Sylphid (Player cannot remove cards)"
        printfn "13. Boss: Dante (Decks are made only of Spades and Clubs)"
        printfn "14. Boss: Behemoth (Enemy devours 3 cards from your deck each round)"
        printfn "15. Boss: Aion (Game ends at Round 3, abilities at Round 1)\n"
        printf "Choice (1-15): "
    else
        printfn "\n=== AI 난이도 ==="
        printfn "1. 초급 (무작위 정제)"
        printfn "2. 중급 (시뮬레이션 정제)"
        printfn "3. 고급 (확률 계산 정제)\n\n=== CHALLENGE BOSS ==="
        printfn "4. 특별 - 기계의 왕 마키나 (상대가 매턴 새로운 능력을 중첩 장착합니다)"
        printfn "5. 특별 - 별의 여왕 스텔라 (상대가 승점 4점 보유 시작합니다)"
        printfn "6. 특별 - 현자 아달란 (서로 40장 중 20장을 드래프트해서 시작 덱을 선택합니다)"
        printfn "7. 특별 - 거울의 여왕 미라쥬 (라운드 종료시 상대가 자신의 덱과 능력을 완벽 복제합니다)"
        printfn "8. 특별 - 거인의 왕 골리앗 (상대가 항상 7장의 카드를 보고 결정합니다)"
        printfn "9. 특별 - 심연의 왕 베누스 (플레이어가 정제시 카드가 3장만 공개됩니다)"
        printfn "10. 특별 - 황금의 왕 골드윙 (라운드 승점을 2배 획득하지만 멀리건 시 승점이 감소합니다)"
        printfn "11. 특별 - 뒤틀린 왕 델타 (포커 족보의 가치와 서열이 완벽하게 역전됩니다)"
        printfn "12. 특별 - 숲의 여왕 실피드 (상대는 정제가 자유로우나, 플레이어는 제거가 봉인됩니다)"
        printfn "13. 특별 - 기록의 왕 단테 (서로 검은색 카드인 스페이드와 클로버만으로 게임을 진행합니다)"
        printfn "14. 특별 - 백수의 왕 베히모스 (매 턴 시작 시 내 덱의 카드 4장을 파괴하며, 8장 미만 시 보충됩니다)"
        printfn "15. 특별 - 시간의 여왕 아이온 (1라운드 종료 시 능력을 얻으며, 총 3라운드까지만 진행됩니다)\n"
        printf "선택 (1-15): "
        
    match Console.ReadLine() with
    | "1" -> Beginner | "2" -> Intermediate | "3" -> Advanced
    | "4" -> Machina | "5" -> Stella | "6" -> Adalan
    | "7" -> Mirage | "8" -> Goliath | "9" -> Venus | "10" -> Goldwing
    | "11" -> Delta | "12" -> Sylphid | "13" -> Dante | "14" -> Behemoth | "15" -> Aion
    | _ -> getDifficulty lang

[<EntryPoint>]
let main _ =
    let lang = getLanguage ()
    let diff = getDifficulty lang
    let p, e = setupGame lang diff
    gameLoop lang 1 diff p e [] |> ignore
    0