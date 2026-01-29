{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day, getCurrentTime, utctDay, addDays, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

-- =========================
-- Data model (matches data.json)
-- =========================

data HabitFile = HabitFile
  { habits :: [Habit]
  } deriving (Show, Generic)

data Habit = Habit
  { name :: String
  , checks :: [Day]   -- days checked, stored as YYYY-MM-DD in JSON
  } deriving (Show, Generic)

instance FromJSON HabitFile
instance ToJSON HabitFile
instance FromJSON Habit
instance ToJSON Habit

-- =========================
-- Terminal helpers
-- =========================

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

-- =========================
-- Habit logic
-- =========================

toSet :: Habit -> Set Day
toSet = Set.fromList . checks

toggleToday :: Day -> Habit -> Habit
toggleToday today h =
  let s = toSet h
      s' =
        if Set.member today s
          then Set.delete today s
          else Set.insert today s
  in h { checks = Set.toList s' }

streakFrom :: Day -> Set Day -> Int
streakFrom today s = go 0 today
  where
    go n d
      | Set.member d s = go (n + 1) (addDays (-1) d)
      | otherwise = n

dayMarks :: Int -> Day -> Set Day -> String
dayMarks n today s =
  let days = [ addDays (fromIntegral (-i)) today | i <- reverse [0 .. (n - 1)] ]
      mark d = if Set.member d s then 'x' else '.'
  in intercalate " " (map (\d -> [mark d]) days)

fmtDay :: Day -> String
fmtDay d = formatTime defaultTimeLocale "%Y-%m-%d" d

-- =========================
-- Rendering
-- =========================

render :: Day -> Int -> HabitFile -> IO ()
render today selected hf = do
  clearScreen
  putStrLn "habit-tracker  (j/k move, space toggle today, q quit)"
  putStrLn ("today: " <> fmtDay today)
  putStrLn ""

  if null (habits hf)
    then putStrLn "No habits found. Edit the JSON file to add habits."
    else do
      mapM_ (putStrLn . renderRow today selected) (zip [0 ..] (habits hf))
      putStrLn ""
      let h = habits hf !! selected
          s = toSet h
      putStrLn ("Selected: " <> name h)
      putStrLn ("Last 14 days: " <> dayMarks 14 today s)
      putStrLn ("Streak: " <> show (streakFrom today s))

renderRow :: Day -> Int -> (Int, Habit) -> String
renderRow today selected (i, h) =
  let cursor = if i == selected then ">" else " "
      doneToday = if Set.member today (toSet h) then "[x]" else "[ ]"
      st = streakFrom today (toSet h)
  in cursor <> " " <> doneToday <> " " <> name h <> "  (streak " <> show st <> ")"

-- =========================
-- Main loop
-- =========================

loop :: FilePath -> Day -> Int -> HabitFile -> IO ()
loop dataPath today selected hf = do
  let count = length (habits hf)
      selected' = if count == 0 then 0 else max 0 (min (count - 1) selected)

  render today selected' hf
  c <- getChar

  case c of
    'q' -> exitSuccess
    'j' -> loop dataPath today (selected' + 1) hf
    'k' -> loop dataPath today (selected' - 1) hf
    ' ' ->
      case habits hf of
        [] -> loop dataPath today selected' hf
        hs ->
          case splitAt selected' hs of
            (_, []) -> loop dataPath today selected' hf
            (before, h:after) -> do
              let h'  = toggleToday today h
                  hf' = hf { habits = before <> [h'] <> after }
              encodeFile dataPath hf'
              loop dataPath today selected' hf'
    _ -> loop dataPath today selected' hf

-- =========================
-- Entry point
-- =========================

main :: IO ()
main = do
  args <- getArgs
  dataPath <- case args of
    [p] -> pure p
    _ -> fail "usage: habit-tui /path/to/data.json"

  hfEither <- eitherDecodeFileStrict' dataPath
  hf <- case hfEither of
    Left err -> fail ("Failed to parse JSON: " <> err)
    Right v -> pure v

  now <- getCurrentTime
  let today = utctDay now

  bracket_
    (do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      hideCursor
    )
    (do
      showCursor
      hSetEcho stdin True
    )
    (loop dataPath today 0 hf)

