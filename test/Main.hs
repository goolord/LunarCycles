module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List (find, sort)
import Lunar.Codegen (codeText, songCode, sourceMini)
import Lunar.Compile (Compiled (..), arrangeSong, byChannel, cError, compileSequence, eventsIn)
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

  let groove = concatMap (sequenceTracks demoSong) (take 1 (songSequences demoSong))
      demoMinis = map sourceMini groove
  check ("demo minis " <> T.pack (show demoMinis)) (take 3 demoMinis == ["bd*4", "sn(3,8,2)", "[hh hh:2 hh [hh hh:1]]*2"])
  check "demo compiles" (all ((== Nothing) . cError) (concatMap (compileSequence demoSong) (songSequences demoSong)))

  -- A channel's sound and knobs are shared; its rhythm is the sequence's own.
  let tracksOf sid song = maybe [] (sequenceTracks song) (findSequence sid song)
      bassIn sid song = find ((== 4) . trackId) (tracksOf sid song)
      resounded = mapTrack 1 4 (\t -> t {trackSound = "superfm"}) demoSong
  check "changing a channel's sound in one sequence changes it in the others" (fmap trackSound (bassIn 2 resounded) == Just "superfm")
  check "the other sequence keeps its own rhythm" (fmap trackSource (bassIn 2 resounded) == fmap trackSource (bassIn 2 demoSong))
  let rerhythmed = mapTrack 1 4 (\t -> t {trackSource = Mini "0"}) demoSong
  check "changing a part leaves the other sequences' parts" (fmap trackSource (bassIn 2 rerhythmed) == fmap trackSource (bassIn 2 demoSong))
  let renamedClap = mapTrack 2 5 (\t -> t {trackName = "claps"}) demoSong
  check "a channel edit adds no part where it had none" (fmap (partsIn `flip` 5) (findSequence 2 renamedClap) == Just False)
  check "every sequence shows every channel" (all (\sq -> length (sequenceTracks demoSong sq) == length (songChannels demoSong)) (songSequences demoSong))

  -- The sequence being edited plays on a channel per track.
  let sequenceText = codeText (songCode PlaySequence 1 demoSong)
  check "sequence code has a channel per track" (all (`T.isInfixOf` sequenceText) ["d1 $ every 4 (fast 2)", "d4 $ off 0.125 (fast 2)"])
  T.putStrLn sequenceText
  let introText = codeText (songCode PlaySequence 2 demoSong)
  check ("a channel keeps its number in every sequence: " <> introText) (all (`T.isInfixOf` introText) ["d3 $ every 3 rev", "d4 $ note"] && not ("d1" `T.isInfixOf` introText))

  -- The playlist binds each placed sequence and places it with seqP.
  let songText = codeText (songCode PlaySong 1 demoSong)
  check "song code binds the sequences" (all (`T.isInfixOf` songText) ["let groove = stack", "    intro = stack", "    break = stack"])
  check "song code loops at the end of the playlist" ("d1 $ timeLoop 24 $ seqP" `T.isInfixOf` songText)
  check "song code places every clip" (all (`T.isInfixOf` songText) ["[ (0, 4, intro)", ", (4, 12, groove)", ", (12, 14, break)", ", (14, 24, groove)", ", (22, 24, break)"])
  check "a track's lines continue under it" ("      [ every 4 (fast 2)\n        $ s \"bd*4\"\n        # gain 1.1" `T.isInfixOf` songText)
  T.putStrLn songText
  let emptyText = codeText (songCode PlaySong 1 demoSong {songPlaylist = []})
  check "an empty playlist is silent" ("d1 $ silence" `T.isInfixOf` emptyText)
  let muted = mapTrack 3 1 (\t -> t {trackMuted = True}) demoSong
      mutedText = codeText (songCode PlaySong 1 muted)
  check "a muted track is commented out of its stack" ("      -- s \"bd ~ ~ bd\"" `T.isInfixOf` mutedText)
  let clashing = mapSequence 2 (\sq -> sq {seqName = "Groove"}) (mapSequence 3 (\sq -> sq {seqName = "stack"}) demoSong)
      clashText = codeText (songCode PlaySong 1 clashing)
  check ("binding names stay distinct and clear of Tidal's: " <> clashText) (all (`T.isInfixOf` clashText) ["let groove = stack", "    groove_2 = stack", "    stack_3 = stack"])

  -- A clip plays its sequence from the sequence's own cycle 0, and nothing
  -- plays where the sequence has no clip.
  let arranged = arrangeSong demoSong
      onsets from to pat = sort [fromRational (wholeStart e) :: Double | e <- eventsIn from to pat, eventHasOnset e]
  case (lookup 1 arranged, concatMap (compileSequence demoSong) (take 1 (songSequences demoSong))) of
    (Just (arrangedKick : _), rawKick : _) -> do
      check "groove's kick is silent during the intro" (null (onsets 0 4 (cPattern arrangedKick)))
      check "groove's kick starts from its cycle 0 at cycle 4" (onsets 4 6 (cPattern arrangedKick) == map (+ 4) (onsets 0 2 (cPattern rawKick)))
      check "the song loops at cycle 24" (onsets 28 30 (cPattern arrangedKick) == map (+ 24) (onsets 4 6 (cPattern arrangedKick)))
    _ -> check "groove arranges its kick" False
  let channels = byChannel (map snd arranged)
  check "the song plays one pattern per channel" (map cChannel channels == [1 .. length (songChannels demoSong)])
  case (find ((== 4) . cChannel) channels, lookup 2 arranged, lookup 1 arranged) of
    (Just bass, Just intro, Just grooveArranged) -> do
      let partOf comps = maybe [] (onsets 0 24 . cPattern) (find ((== 4) . cChannel) comps)
      check "a channel plays its part in each sequence's clips" (onsets 0 24 (cPattern bass) == sort (partOf intro <> partOf grooveArranged))
    _ -> check "the song arranges the bass" False

  -- Songs survive a trip through a file.
  check "a song reads back as written" (songFromText (songToText demoSong) == Right demoSong)
  check "a foreign file is refused" (either (const True) (const False) (songFromText "d1 $ s \"bd\""))
  let v1Track tid name sound mini = "Track {trackId = " <> tid <> ", trackName = \"" <> name <> "\", trackSound = \"" <> sound <> "\", trackSource = Mini \"" <> mini <> "\", trackChain = [], trackParams = fromList [], trackMuted = False, trackSolo = False}"
      v1Seq sid name tracks = "Sequence {seqId = " <> sid <> ", seqName = \"" <> name <> "\", seqCycles = 4, seqTracks = [" <> T.intercalate ", " tracks <> "]}"
      v1 =
        T.unlines
          [ "LunarCycles song 1"
          , "Song {songCps = 0.5, songSequences = ["
              <> v1Seq "1" "a" [v1Track "1" "kick" "bd" "bd*4", v1Track "2" "bass" "superpiano" "0 3"]
              <> ", "
              <> v1Seq "2" "b" [v1Track "4" "bass" "superpiano" "7"]
              <> "], songPlaylist = []}"
          ]
  case songFromText v1 of
    Right old -> do
      check "a first-format song merges its tracks into channels" (map chanName (songChannels old) == ["kick", "bass"])
      check "a first-format song keeps each sequence's rhythm" (map sourceMini (filter ((== "bass") . trackName) (concatMap (sequenceTracks old) (songSequences old))) == ["0 3", "7"])
    Left e -> check ("a first-format song reads: " <> e) False
  n <- readIORef failures
  unless (n == 0) exitFailure
