module Types

type Language = ENG | KOR
type Suit = Hearts | Diamonds | Clubs | Spades
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Suit: Suit; Rank: Rank }
type Ability =
    | ColorPlay | Gemstone | FlushBros | StraightBros | Promotion | Arithmetic | LastHope | Protagonist
    | Footstool | RoyalDignity | Gift | Twins | BonusPoint | Stand | SpareChange | GoldPile | Housekeeper
    | FifthStar | MakeFriends | Tax | LuckySeven | Jackpot | RoyalFamily | SmallEvolution | AnotherPossibility
    | Housekeeper2 | Steal | GiantPouch | SoulScissors | SecondChance | DisguisedJack | FirstImpression
    | Disconnection | LoneWolves | Minimalist | LesMiserables | BackToTheWall | Prudence | AChance
    | Marshmallow | BlackAndWhite | FourLeafClover | GrandFinale | Joker | Recycling | Oblivion
    | CrowIdol | Embargo of string | HandOfFate | Forging

// 🌟 FIX: 새로운 특별 AI 5명 추가
type Difficulty = Beginner | Intermediate | Advanced | Machina | Stella | Adalan | Mirage | Goliath | Venus | Goldwing | Delta | Sylphid | Dante | Behemoth | Aion

type Player = { Name: string; Deck: Card list; Abilities: Ability list; Score: int }