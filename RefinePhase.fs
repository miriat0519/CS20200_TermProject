module RefinePhase

open Types
open Card
open Deck

// 🌟 FIX: 번호 입력 시 최소 선택량(minReq)과 보여진 카드 장수(maxIndex)를 검증합니다.
let rec getSelections lang acc minReq maxAllow maxIndex =
    if lang = ENG then printf "Select number (0 to finish): "
    else printf "선택할 번호를 입력하세요 (0을 입력하면 종료): "
    
    match System.Int32.TryParse(System.Console.ReadLine()) with
    | true, 0 ->
        // 중복된 입력을 제외한 실제 유효 선택 개수 계산
        let uniqueCount = acc |> Set.ofList |> Set.count
        
        if uniqueCount > maxAllow then
            if lang = ENG then printfn "Error: Max %d cards can be selected. Start over." maxAllow
            else printfn "오류: 최대 %d장까지만 선택 가능합니다. 처음부터 다시 선택하세요." maxAllow
            getSelections lang [] minReq maxAllow maxIndex
            
        elif uniqueCount < minReq then
            if lang = ENG then printfn "Error: Select at least %d more card(s) to maintain min deck size." (minReq - uniqueCount)
            else printfn "오류: 덱을 8장 이상으로 유지하려면 %d장을 더 선택해야 합니다. 계속 번호를 입력하세요." (minReq - uniqueCount)
            getSelections lang acc minReq maxAllow maxIndex // 지금까지 고른 것을 유지하고 다시 물어봄
            
        else
            List.rev acc
            
    // 🌟 FIX: 화면에 보여준 카드(maxIndex) 내의 번호만 배열에 등록되도록 방어
    | true, n when n > 0 && n <= maxIndex -> 
        getSelections lang (n :: acc) minReq maxAllow maxIndex
        
    | _ ->
        if lang = ENG then printfn "Invalid input. Please enter a valid card number." 
        else printfn "잘못된 입력입니다. 화면에 있는 유효한 카드 번호를 입력하세요."
        getSelections lang acc minReq maxAllow maxIndex

let removePhase lang player viewCount =
    if lang = ENG then printfn "\n=== Remove Phase ===" else printfn "\n=== 제거 페이즈 ==="
    
    let shuffledDeck = player.Deck |> shuffle
    let shown = shuffledDeck |> List.truncate viewCount
    
    let sortedShown = sortCards shown
    printCards lang sortedShown
    
    let maxRemovable = max 0 (List.length player.Deck - 8)
    let maxIdx = List.length sortedShown // 화면에 보여진 카드 수
    
    // 제거 단계: 최소 0장 ~ 최대 maxRemovable 장까지 선택 가능
    let indicesToRemove = getSelections lang [] 0 maxRemovable maxIdx |> List.map (fun i -> i - 1) |> Set.ofList
    
    let kept = sortedShown |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> not (Set.contains i indicesToRemove)) |> List.map snd
    let discarded = sortedShown |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> Set.contains i indicesToRemove) |> List.map snd
    
    let remainingDeck = shuffledDeck |> List.skip (List.length shown)
    ({ player with Deck = kept @ remainingDeck }, discarded)

let addPhase lang player community =
    if lang = ENG then printfn "\n=== Add Phase ===" else printfn "\n=== 추가 페이즈 ==="
    
    let sortedCommunity = sortCards community
    printCards lang sortedCommunity
    
    // 🌟 FIX: 현재 덱이 8장 미만일 경우, 8장이 될 때까지 강제로 카드를 고르게 만듭니다.
    let minRequired = max 0 (8 - List.length player.Deck)
    let minReqClamped = min (List.length sortedCommunity) minRequired
    let maxIdx = List.length sortedCommunity
    
    // 추가 단계: 최소 minReqClamped 장 ~ 최대 무제한 선택 가능
    let indicesToAdd = getSelections lang [] minReqClamped System.Int32.MaxValue maxIdx |> List.map (fun i -> i - 1) |> Set.ofList
    let added = sortedCommunity |> List.mapi (fun i c -> i, c) |> List.filter (fun (i, _) -> Set.contains i indicesToAdd) |> List.map snd
    
    { player with Deck = player.Deck @ added }