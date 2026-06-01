# ♠️ Refine Poker

## 📖 Overview

Refine Poker is a roguelike deck-building card game based on traditional poker hand rankings.

Both the player and the AI opponent begin with a 20-card deck drawn from a shared Community Deck consisting of two combined standard decks (104 cards total). Throughout the game, players refine their decks by removing unwanted cards and acquiring stronger ones, then compete in poker duels to earn Victory Points.

The game lasts for five rounds. Each round consists of three phases:

* Refine Phase
* Duel Phase
* Special Phase

The player with the highest total Victory Points at the end of the game wins.

---

# 📝 How to Run

## Option 1: .NET 10 SDK Installed

1. Click the **Code** button on the GitHub repository page.
2. Select **Download ZIP**.
3. Extract the downloaded archive.
4. Run `run.bat`.

## Option 2: .NET 10 SDK Not Installed

1. Click the **Code** button on the GitHub repository page.
2. Select **Download ZIP**.
3. Extract the downloaded archive.
4. Navigate to:

```text
bin/Debug/net10.0/
```

5. Run:

```text
RefinePoker.exe
```

---

# 🎮 Gameplay

## Before the Game

### Language Selection

Choose one of the following languages:

* KOR (Korean)
* ENG (English)

All game text will be displayed in the selected language.

---

### AI Difficulty Selection

The game provides four AI difficulty levels.

#### 🟢 Beginner

All decisions are made randomly.

#### 🟡 Intermediate

Simulates each available option 10 times and chooses the option with the best result.

#### 🔴 Advanced

Calculates the expected value of all available options and chooses the optimal action.

#### 👑 Challenge

Uses the same decision-making algorithm as the Advanced AI while applying additional boss-specific rules.

---

# 🔄 Round Structure

Each round consists of the following phases.

## ① Refine Phase

Improve your deck by removing weak cards and obtaining stronger ones.

### Remove

* Five random cards from your deck are revealed.
* You may remove any number of those cards.
* Removed cards are returned to the Community Deck and shuffled.
* Your deck may never contain fewer than 8 cards.

### Add

* Five cards from the Community Deck are revealed.
* You may add any number of those cards to your deck.

The AI performs the same actions according to its difficulty level.

---

## ② Duel Phase

Compete using your refined deck.

### Draw

Both players draw five cards from their decks.

### Mulligan

Once per duel, a player may:

1. Select any number of cards from their hand.
2. Return those cards to the deck.
3. Draw the same number of replacement cards.

### Hand Evaluation

Texas Hold'em hand rankings are used.

Certain abilities and boss effects may allow players to draw 6 or 7 cards. In such cases, the game automatically evaluates the strongest possible 5-card poker hand.

### Victory Points

The winner of the duel gains Victory Points equal to the current round number.

Examples:

* Round 1 Victory → 1 VP
* Round 3 Victory → 3 VP
* Round 5 Victory → 5 VP

Certain abilities may grant additional Victory Points.

---

## ③ Special Phase

Occurs only once, after Round 2.

Four random abilities are presented, and the player chooses one.

### Ability Rarity

| Rarity | Probability |
| ------ | ----------- |
| ★      | 5%          |
| ★★     | 15%         |
| ★★★    | 30%         |
| ★★★★   | 35%         |
| ★★★★★  | 15%         |

The AI also gains abilities according to its difficulty level.

---

# 👑 Challenge Boss System

Challenge Mode features 12 unique bosses that modify the core rules of the game.

### ⚙️ Machina, King of Machines

Gains a new ability at the end of every round.

### ✨ Stella, Queen of Stars

Begins the game with 4 Victory Points.

### 🧙 Adalan, the Sage

Before the game starts, both players draft 20 cards from a pool of 40 cards.

### 🪞 Mirage, Queen of Mirrors

Copies the player's deck and abilities after Round 2.

### 🧌 Goliath, King of Giants

Always draws 7 cards during the Duel Phase.

### 👁️ Venus, King of the Abyss

The player may only view 3 cards during the Refine Phase.

### 💰 Goldwing, King of Gold

Earns double Victory Points, but loses points whenever a Mulligan is performed.

### 🌀 Delta, the Twisted King

Completely reverses poker hand rankings.

### 🌿 Sylphid, Queen of the Forest

The player cannot remove cards during the Refine Phase.

### 📜 Dante, King of Records

All red-suited cards are removed. Only Spades and Clubs remain.

### 🐾 Behemoth, King of Beasts

Destroys 4 random cards from the player's deck at the start of every round.

### ⏳ Aion, Queen of Time

Starts with a special ability in Round 1 and forces the game to end after Round 3.

---

# 📝 Requirement Change Log

The final implementation differs from the original proposal in several ways.

## Change 1: Expanded AI System

### Original Requirement

The player competes against a random AI for five rounds.

### Final Implementation

* Beginner AI
* Intermediate AI
* Advanced AI
* 12 Challenge Bosses

### Justification

The original AI became repetitive during testing. Additional difficulty levels and unique boss mechanics significantly improved replayability and strategic depth.

---

## Change 2: Ability Selection System

### Original Requirement

The player receives one random ability after Round 2.

### Final Implementation

The player chooses one ability from four randomly generated options.

### Justification

A completely random reward often disrupted the player's strategy. Allowing players to choose from multiple options creates more meaningful decisions and rewards planning.

---

## Change 3: Mulligan System

### Original Requirement

Players draw five cards and immediately compare poker hands.

### Final Implementation

Players may perform one Mulligan before hand evaluation.

### Justification

Without a Mulligan system, even a well-constructed deck could fail due to a single unlucky draw. The Mulligan mechanic adds strategic depth while reducing frustration caused by bad luck.

---

# 🤖 LLM Usage Experience

## What the LLM Was Used For

1. Designing the overall project structure and module organization.
2. Running simulations to balance abilities and identify edge cases.
3. Assisting with the design of the Advanced AI algorithm.
4. Drafting the initial version of this README.

## What Required Additional Prompting

The Advanced AI occasionally evaluated certain abilities as having zero value. Additional prompts were required to identify missing evaluation factors and improve the decision-making model.

## Limitations Encountered

1. The LLM occasionally confused this project with another card game project discussed in a separate conversation and produced irrelevant suggestions.
2. The LLM sometimes generated code using programming techniques that had not been covered in class, making future maintenance and modification more difficult.

---

Thank you for playing Refine Poker!
