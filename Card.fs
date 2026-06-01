module Card

open Types

let rankToString lang rank =
    match lang, rank with
    | ENG, Jack -> "Jack" | KOR, Jack -> "잭"
    | ENG, Queen -> "Queen" | KOR, Queen -> "퀸"
    | ENG, King -> "King" | KOR, King -> "킹"
    | ENG, Ace -> "Ace" | KOR, Ace -> "에이스"
    | _, Two -> "2" | _, Three -> "3" | _, Four -> "4" | _, Five -> "5"
    | _, Six -> "6" | _, Seven -> "7" | _, Eight -> "8" | _, Nine -> "9" | _, Ten -> "10"

let suitToString lang suit =
    match lang, suit with
    | ENG, Hearts -> "♥ Hearts" | KOR, Hearts -> "♥ 하트"
    | ENG, Diamonds -> "♦ Diamonds" | KOR, Diamonds -> "♦ 다이아몬드"
    | ENG, Clubs -> "♣ Clubs" | KOR, Clubs -> "♣ 클로버"
    | ENG, Spades -> "♠ Spades" | KOR, Spades -> "♠ 스페이드"

let cardToString lang card =
    match lang with
    | ENG -> sprintf "%s of %s" (rankToString ENG card.Rank) (suitToString ENG card.Suit)
    | KOR -> sprintf "%s %s" (suitToString KOR card.Suit) (rankToString KOR card.Rank)

let printCards lang cards =
    cards |> List.iteri (fun i c ->
        printfn "%02d. %s" (i + 1) (cardToString lang c) // 🌟 %02d 적용
    )

// 🌟 NEW: 문양(Suit) -> 숫자(Rank) 순으로 카드를 정렬하는 함수
let sortCards cards =
    cards |> List.sortBy (fun c -> c.Suit, c.Rank)