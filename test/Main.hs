module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List (sort)
import Lunar.Codegen (codeText, songCode, sourceMini)
import Lunar.Compile (Compiled (..), arrangeSong, cError, compileSequence, eventsIn)
import Lunar.Model
import Lunar.Project (songFromText, songToText)
import Lunar.Refactor
import Sound.Tidal.Pattern (eventHasOnset, wholeStart)
import System.Exit (exitFailure)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: Text -> Bool -> IO ()
      check name ok = unless ok $ do
        T.putStrLn ("FAIL: " <> name)
        modifyIORef failures (+ 1)
      spelled pitched sound steps expect = do
        let got = rfText (refactorSteps pitched sound steps)
        check (T.unwords [literalSteps pitched sound steps, "=>", got, "expected", expect]) (got == expect)
      hitsAt :: Int -> [Int] -> [Maybe Int]
      hitsAt n is = [if i `elem` is then Just 0 else Nothing | i <- [0 .. n - 1]]

  spelled False "bd" (hitsAt 16 [0, 4, 8, 12]) "bd*4"
  spelled False "sn" (euclidSteps 3 8 2 0) "sn(3,8,2)"
  spelled False "bd" (euclidSteps 3 8 0 0) "bd(3,8)"
  spelled False "hh" (replicate 8 (Just 0)) "hh*8"
  spelled False "cp" (hitsAt 8 []) "~"
  spelled False "bd" (hitsAt 4 [0, 1, 2]) "bd!3 ~"
  spelled False "sn" (hitsAt 16 [4, 12]) "~ sn ~ sn"
  -- Pitched notes keep their lengths, so the grid is not coarsened.
  spelled True "superpiano" [Just 0, Nothing, Just 7, Nothing] "0 ~ 7 ~"
  spelled True "superpiano" [Just 3, Just 3, Just 3, Just 3] "3*4"
  -- An irregular grid stays literal rather than nesting brackets.
  let irregular = [if i `elem` [0, 3, 5, 6, 11, 13, 17, 20, 22, 23, 27, 30] then Just 0 else Nothing | i <- [0 .. 31 :: Int]]
  spelled False "hh" irregular (literalSteps False "hh" irregular)

  check "miniToSteps bd(3,8)" (fmap snd (miniToSteps False "bd(3,8)") == Right (euclidSteps 3 8 0 0))
  check "miniToSteps rejects <a b>" (either (const True) (const False) (miniToSteps False "<bd sn>"))
  check "miniToSteps reads samples" (miniToSteps False "hh:2 ~ hh ~" == Right ("hh", resizeSteps 8 [Just 2, Nothing, Just 0, Nothing]))

  let groove = concatMap seqTracks (take 1 (songSequences demoSong))
      demoMinis = map sourceMini groove
  check ("demo minis " <> T.pack (show demoMinis)) (take 3 demoMinis == ["bd*4", "sn(3,8,2)", "[hh hh:2 hh [hh hh:1]]*2"])
  check "demo compiles" (all ((== Nothing) . cError) (concatMap compileSequence (songSequences demoSong)))

  -- The sequence being edited plays on a channel per track.
  let sequenceText = codeText (songCode PlaySequence 1 demoSong)
  check "sequence code has a channel per track" (all (`T.isInfixOf` sequenceText) ["d1 $ every 4 (fast 2)", "d4 $ off 0.125 (fast 2)"])
  T.putStrLn sequenceText

  -- The playlist binds each placed sequence and places it with seqP.
  let songText = codeText (songCode PlaySong 1 demoSong)
  check "song code binds the sequences" (all (`T.isInfixOf` songText) ["let groove = stack", "    intro = stack", "    break = stack"])
  check "song code loops at the end of the playlist" ("d1 $ timeLoop 24 $ seqP" `T.isInfixOf` songText)
  check "song code places every clip" (all (`T.isInfixOf` songText) ["[ (0, 4, intro)", ", (4, 12, groove)", ", (12, 14, break)", ", (14, 24, groove)", ", (22, 24, break)"])
  check "a track's lines continue under it" ("      [ every 4 (fast 2)\n        $ s \"bd*4\"\n        # gain 1.1" `T.isInfixOf` songText)
  T.putStrLn songText
  let emptyText = codeText (songCode PlaySong 1 demoSong {songPlaylist = []})
  check "an empty playlist is silent" ("d1 $ silence" `T.isInfixOf` emptyText)
  let muted = mapSequence 3 (mapTrack 1 (\t -> t {trackMuted = True})) demoSong
      mutedText = codeText (songCode PlaySong 1 muted)
  check "a muted track is commented out of its stack" ("      -- s \"bd ~ ~ bd\"" `T.isInfixOf` mutedText)
  let clashing = mapSequence 2 (\sq -> sq {seqName = "Groove"}) (mapSequence 3 (\sq -> sq {seqName = "stack"}) demoSong)
      clashText = codeText (songCode PlaySong 1 clashing)
  check ("binding names stay distinct and clear of Tidal's: " <> clashText) (all (`T.isInfixOf` clashText) ["let groove = stack", "    groove_2 = stack", "    stack_3 = stack"])

  -- A clip plays its sequence from the sequence's own cycle 0, and nothing
  -- plays where the sequence has no clip.
  let arranged = arrangeSong demoSong
      onsets from to pat = sort [fromRational (wholeStart e) :: Double | e <- eventsIn from to pat, eventHasOnset e]
  case (lookup 1 arranged, concatMap compileSequence (take 1 (songSequences demoSong))) of
    (Just (arrangedKick : _), rawKick : _) -> do
      check "groove's kick is silent during the intro" (null (onsets 0 4 (cPattern arrangedKick)))
      check "groove's kick starts from its cycle 0 at cycle 4" (onsets 4 6 (cPattern arrangedKick) == map (+ 4) (onsets 0 2 (cPattern rawKick)))
      check "the song loops at cycle 24" (onsets 28 30 (cPattern arrangedKick) == map (+ 24) (onsets 4 6 (cPattern arrangedKick)))
    _ -> check "groove arranges its kick" False
  check "arranged channels are distinct" $
    let chans = map cChannel (concatMap snd arranged) in length chans == length (foldr (\c acc -> if c `elem` acc then acc else c : acc) [] chans)

  -- Songs survive a trip through a file.
  check "a song reads back as written" (songFromText (songToText demoSong) == Right demoSong)
  check "a foreign file is refused" (either (const True) (const False) (songFromText "d1 $ s \"bd\""))
  n <- readIORef failures
  unless (n == 0) exitFailure
