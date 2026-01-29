# Terminal Habit Tracker (Haskell)

Habit trackers are conceptually simple, but for some reason many of them end up hidden behind paywalls, subscriptions, or bloated mobile apps.

This project is a **lightweight habit tracker that runs entirely in your terminal**, written in Haskell. It focuses on the core idea of habit tracking — daily consistency and streaks — without accounts, cloud sync, or unnecessary features.

The tracker is still actively being worked on, with the goal of staying simple, transparent, and easy to extend.

---

## Features

- Terminal-based UI
- Daily habits (calendar-based, not timer-based)
- Toggle habits for _today_
- Automatic streak calculation
- Persistent local storage (JSON)
- Simple keybindings:
  - `j` / `k` — move selection
  - `space` — toggle today
  - `q` — quit

---

## How it works (high level)

- **Haskell** handles:
  - terminal rendering
  - keyboard input
  - habit logic (streaks, toggles)
  - reading and writing data
- **JSON** is used as a small local data store
- **Nix** provides a reproducible development environment
- A small **bash launcher** (`habit`) makes it usable like a normal CLI command
