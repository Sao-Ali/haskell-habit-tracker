{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import Data.Char (isPrint, isSpace, toLower)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
  ( Day
  , addDays
  , defaultTimeLocale
  , diffDays
  , formatTime
  , getZonedTime
  , localDay
  , zonedTimeToLocalTime
  )
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)

-- Data model (matches data.json)

-- | Root JSON payload stored on disk.
data HabitFile = HabitFile
  { habits :: [Habit]
  } deriving (Show, Generic)

-- | A single habit and its completed days.
data Habit = Habit
  { name :: String
  , checks :: [Day]   -- days checked, stored as YYYY-MM-DD in JSON
  } deriving (Show, Generic)

instance FromJSON HabitFile
instance ToJSON HabitFile
instance FromJSON Habit
instance ToJSON Habit

-- Terminal helpers

-- | Clear the terminal and move the cursor home.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- | Hide the terminal cursor.
hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

-- | Show the terminal cursor.
showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

-- Habit logic

-- | Convert a habit's checked days into a set for fast lookup.
toSet :: Habit -> Set Day
toSet = Set.fromList . checks

-- | Toggle completion for the given day.
toggleToday :: Day -> Habit -> Habit
toggleToday today h =
  let s = toSet h
      s' =
        if Set.member today s
          then Set.delete today s
          else Set.insert today s
  in h { checks = Set.toList s' }

-- | Count consecutive checked days ending at the given day.
streakFrom :: Day -> Set Day -> Int
streakFrom today s = go 0 today
  where
    go n d
      | Set.member d s = go (n + 1) (addDays (-1) d)
      | otherwise = n

-- | Render the last @n@ days as "x" and "." marks.
dayMarks :: Int -> Day -> Set Day -> String
dayMarks n today s =
  let days = [ addDays (fromIntegral (-i)) today | i <- reverse [0 .. (n - 1)] ]
      mark d = if Set.member d s then 'x' else '.'
  in intercalate " " (map (\d -> [mark d]) days)

-- | Format a day as YYYY-MM-DD.
fmtDay :: Day -> String
fmtDay d = formatTime defaultTimeLocale "%Y-%m-%d" d

-- | Most recent checked day, if any.
lastCheckedDay :: Habit -> Maybe Day
lastCheckedDay h =
  case checks h of
    [] -> Nothing
    ds -> Just (maximum ds)

-- | Days since the last check (0 if today), when available.
daysSinceLastCheck :: Day -> Habit -> Maybe Integer
daysSinceLastCheck today h =
  diffDays today <$> lastCheckedDay h

-- Rendering

-- | Draw the full screen view.
render :: Day -> Int -> HabitFile -> IO ()
render today selected hf = do
  clearScreen
  putStrLn "habit-tracker  (j/k move, space toggle today, a add habit, d delete habit, q quit)"
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
      renderMissedNotice today h

-- | Render a single habit row for the list view.
renderRow :: Day -> Int -> (Int, Habit) -> String
renderRow today selected (i, h) =
  let cursor = if i == selected then ">" else " "
      doneToday = if Set.member today (toSet h) then "[x]" else "[ ]"
      st = streakFrom today (toSet h)
  in cursor <> " " <> doneToday <> " " <> name h <> "  (streak " <> show st <> ")"

renderMissedNotice :: Day -> Habit -> IO ()
renderMissedNotice today h =
  case daysSinceLastCheck today h of
    Just delta
      | delta >= 2 -> do
          let missedDays = delta - 1
              dayLabel = if missedDays == 1 then "day" else "days"
          putStrLn ""
          putStrLn ("Missed " <> show missedDays <> " " <> dayLabel <> " since your last check-in.")
          putStrLn "It's okay to miss one day as long as you pick up where you left off."
          putStrLn "Don't let one day turn into many. Consistency, consistency, consistency."
    _ -> pure ()

-- Main loop

-- | UI mode for the main loop.
data Mode
  = Normal
  | Adding String (Maybe String)
  | ConfirmDelete Int String

-- | Maximum length for a new habit name.
maxHabitLength :: Int
maxHabitLength = 48

-- | Trim leading and trailing whitespace.
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Check if a habit name already exists, case-insensitive.
isDuplicate :: String -> [Habit] -> Bool
isDuplicate candidate hs =
  let norm = map toLower (trim candidate)
  in any ((== norm) . map toLower . trim . name) hs

-- | Render the add-habit prompt when in add mode.
renderAddPrompt :: Mode -> IO ()
renderAddPrompt Normal = pure ()
renderAddPrompt (Adding draft err) = do
  putStrLn ""
  putStrLn "Add habit (Enter save, Esc cancel)"
  putStrLn ("> " <> draft)
  case err of
    Nothing -> pure ()
    Just msg -> putStrLn ("Error: " <> msg)
renderAddPrompt (ConfirmDelete _ _) = pure ()

renderDeletePrompt :: Mode -> IO ()
renderDeletePrompt Normal = pure ()
renderDeletePrompt (Adding _ _) = pure ()
renderDeletePrompt (ConfirmDelete _ habitName) = do
  putStrLn ""
  putStrLn ("Delete habit '" <> habitName <> "'? (y confirm, n cancel)")

-- | Main event loop handling input and rendering.
loop :: FilePath -> Day -> Int -> HabitFile -> Mode -> IO ()
loop dataPath today selected hf mode = do
  let count = length (habits hf)
      selected' = if count == 0 then 0 else max 0 (min (count - 1) selected)

  render today selected' hf
  renderAddPrompt mode
  renderDeletePrompt mode
  c <- getChar

  case mode of
    Normal ->
      case c of
        'q' -> exitSuccess
        'j' -> loop dataPath today (selected' + 1) hf Normal
        'k' -> loop dataPath today (selected' - 1) hf Normal
        'a' -> loop dataPath today selected' hf (Adding "" Nothing)
        'd' ->
          case habits hf of
            [] -> loop dataPath today selected' hf Normal
            hs ->
              case splitAt selected' hs of
                (_, []) -> loop dataPath today selected' hf Normal
                (_, h:_) -> loop dataPath today selected' hf (ConfirmDelete selected' (name h))
        ' ' ->
          case habits hf of
            [] -> loop dataPath today selected' hf Normal
            hs ->
              case splitAt selected' hs of
                (_, []) -> loop dataPath today selected' hf Normal
                (before, h:after) -> do
                  let h'  = toggleToday today h
                      hf' = hf { habits = before <> [h'] <> after }
                  encodeFile dataPath hf'
                  loop dataPath today selected' hf' Normal
        _ -> loop dataPath today selected' hf Normal
    Adding draft err ->
      case c of
        '\ESC' -> loop dataPath today selected' hf Normal
        '\ETX' -> loop dataPath today selected' hf Normal
        '\n' ->
          let trimmed = trim draft
              tooLong = length trimmed > maxHabitLength
              dup = isDuplicate trimmed (habits hf)
              errMsg
                | null trimmed = Just "Name cannot be empty"
                | tooLong = Just ("Max length is " <> show maxHabitLength)
                | dup = Just "Habit already exists"
                | otherwise = Nothing
          in case errMsg of
            Just msg -> loop dataPath today selected' hf (Adding trimmed (Just msg))
            Nothing -> do
              let newHabit = Habit { name = trimmed, checks = [] }
                  hf' = hf { habits = habits hf <> [newHabit] }
                  newIndex = length (habits hf)
              encodeFile dataPath hf'
              loop dataPath today newIndex hf' Normal
        '\r' -> loop dataPath today selected' hf (Adding draft err)
        '\DEL' ->
          let draft' = if null draft then draft else init draft
          in loop dataPath today selected' hf (Adding draft' Nothing)
        '\b' ->
          let draft' = if null draft then draft else init draft
          in loop dataPath today selected' hf (Adding draft' Nothing)
        _
          | isPrint c && length draft < maxHabitLength ->
              loop dataPath today selected' hf (Adding (draft <> [c]) Nothing)
          | otherwise -> loop dataPath today selected' hf (Adding draft err)
    ConfirmDelete idx habitName ->
      case c of
        'y' -> doDelete idx
        'Y' -> doDelete idx
        'n' -> loop dataPath today selected' hf Normal
        'N' -> loop dataPath today selected' hf Normal
        '\ESC' -> loop dataPath today selected' hf Normal
        '\ETX' -> loop dataPath today selected' hf Normal
        _ -> loop dataPath today selected' hf (ConfirmDelete idx habitName)
      where
        doDelete deleteIndex =
          case habits hf of
            [] -> loop dataPath today selected' hf Normal
            hs ->
              case splitAt deleteIndex hs of
                (_, []) -> loop dataPath today selected' hf Normal
                (before, _:after) -> do
                  let hf' = hf { habits = before <> after }
                      newCount = length (habits hf')
                      newSelected = if newCount == 0 then 0 else min deleteIndex (newCount - 1)
                  encodeFile dataPath hf'
                  loop dataPath today newSelected hf' Normal

-- Entry point

-- | Program entry point.
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

  now <- getZonedTime
  let today = localDay (zonedTimeToLocalTime now)

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
    (loop dataPath today 0 hf Normal)
