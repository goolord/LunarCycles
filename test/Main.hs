module Main (main) where

import Control.Monad (filterM, forM_, unless)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.List (find, nub, sort)
import Lunar.Catalog (isPitched)
import Lunar.Codegen (codeText, songCode, sourceMini)
import Lunar.Compile (Compiled (..), arrangeSong, byChannel, cError, compileSequence, eventsIn)
import Lunar.Model
import Lunar.Project (songFromText, songToText)
import Lunar.Refactor
import Lunar.Sampler
import GHC.Clock (getMonotonicTime)
import Sound.Tidal.Pattern (EventF (..), Value (..), eventHasOnset, wholeStart)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removePathForcibly)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))

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
  samplerChecks check
  exampleChecks check
  n <- readIORef failures
  unless (n == 0) exitFailure

-- | The sampler, rendered offline from WAV files written here: a mono and a
-- stereo @bd@, and a @bd@ at the highest frequency, to filter.
samplerChecks :: (Text -> Bool -> IO ()) -> IO ()
samplerChecks check = do
  tmp <- getTemporaryDirectory
  let bank = tmp </> "lunar-cycles-samples"
      projectDir = tmp </> "lunar-cycles-project"
      projectBank = projectDir </> "samples"
      projectPath = projectDir </> "song.lunar"
  removePathForcibly bank
  removePathForcibly projectDir
  mapM_ (createDirectoryIfMissing True . (bank </>)) ["bd", "hh"]
  BL.writeFile (bank </> "bd" </> "0.wav") (wav 44100 1 (replicate 4410 0.5))
  BL.writeFile (bank </> "bd" </> "1.wav") (wav 48000 2 (concat (replicate 4800 [0.25, 0.5])))
  BL.writeFile (bank </> "bd" </> "2.wav") (wav 48000 1 (take 4800 (cycle [0.5, -0.5])))
  writeFile (bank </> "hh" </> "notes.txt") ""
  createDirectoryIfMissing True (projectBank </> "bd")
  BL.writeFile (projectBank </> "bd" </> "0.wav") (wav 48000 1 (replicate 4800 0.5))
  resolvedBank <- projectSampleFolder projectPath
  check "a project resolves its sibling samples folder" (resolvedBank == Just projectBank)
  offlineSampler 48000 bank >>= \case
    Left e -> check ("the sampler opens a folder: " <> e) False
    Right smp -> do
      let near a b = abs (a - b) < 0.01
          -- Play now, and render the next 6000 frames.
          render ps = do
            now <- getMonotonicTime
            playEvent smp now (Map.fromList (("s", VS "bd") : ps))
            renderFrames smp 6000
          sounding = length . filter (\(l, r) -> abs l > 1e-4 || abs r > 1e-4)
      check "folders without WAV files are not sounds" (samplerSoundCount smp == 1)
      mono <- render []
      check "a mono sample plays at SuperDirt's level, centred" (near (fst (mono !! 100)) 0.1414 && near (snd (mono !! 100)) 0.1414)
      check ("a sample is resampled to the mixer's rate: " <> T.pack (show (sounding mono))) (abs (sounding mono - 4800) < 30)
      left <- render [("pan", VF 0)]
      check "pan 0 is hard left" (all ((< 1e-6) . abs . snd) left && near (fst (left !! 100)) 0.2)
      stereo <- render [("n", VF 1)]
      check "n picks the file, and stereo stays stereo" (near (fst (stereo !! 100)) 0.1 && near (snd (stereo !! 100)) 0.2)
      wrapped <- render [("n", VF 4)]
      check "n wraps around the folder" (near (fst (wrapped !! 100)) 0.1)
      fast <- render [("speed", VF 2)]
      check "speed 2 plays in half the time" (abs (sounding fast - 2400) < 30)
      octave <- render [("note", VF 12)]
      check "a note transposes by semitones" (abs (sounding octave - 2400) < 30)
      back <- render [("speed", VF (-1))]
      check "negative speed plays backwards" (abs (sounding back - 4800) < 30)
      half <- render [("begin", VF 0.5)]
      check "begin skips into the sample, fading in" (abs (sounding half - 2400) < 30 && take 1 (map fst half) == [0])
      shaped <- render [("shape", VF 0.5)]
      check "shape distorts as SuperDirt's does" (near (fst (shaped !! 100)) 0.212)
      open <- render [("n", VF 2)]
      closed <- render [("n", VF 2), ("cutoff", VF 200)]
      let peak = maximum . map (abs . fst) . take 3000 . drop 1000
      check "cutoff filters high frequencies" (peak open > 0.1 && peak closed < 0.01)
      loud <- render [("gain", VF 1.2)]
      check "gain rises with its fourth power" (near (fst (loud !! 100)) (0.1414 * 1.2 ^ (4 :: Int)))
      now <- getMonotonicTime
      playEvent smp (now + 0.01) (Map.fromList [("s", VS "bd")])
      later <- renderFrames smp 1000
      let start = length (takeWhile ((< 1e-4) . abs . fst) later)
      check ("a note starts at its time: frame " <> T.pack (show start)) (start >= 380 && start <= 481)
      _ <- renderFrames smp 6000
      playEvent smp (now + 0.01) (Map.fromList [("s", VS "bd")])
      cancelPending smp
      cancelled <- renderFrames smp 2000
      check "cancelling drops notes not yet started" (sounding cancelled == 0)
      _ <- render [("s", VS "superpiano")]
      missing <- missingSounds smp
      check "sounds without samples are listed" (missing == ["superpiano"])
      closeSampler smp
  removePathForcibly bank
  removePathForcibly projectDir

-- | The example song and its sample folder: the song reads and compiles,
-- sample sounds have files, and its other sounds are stock pitched synths.
exampleChecks :: (Text -> Bool -> IO ()) -> IO ()
exampleChecks check = do
  let dir = "examples" </> "synth-percussion"
      samples = dir </> "samples"
  songFromText <$> T.readFile (dir </> "synth-percussion.lunar") >>= \case
    Left e -> check ("the example song reads: " <> e) False
    Right song -> do
      let comps = concatMap snd (arrangeSong song)
          played = nub [s | c <- comps, e <- eventsIn 0 (fromIntegral (songLength song)) (cPattern c), eventHasOnset e, Just (VS s) <- [Map.lookup "s" (value e)]]
      check "the example compiles" (all ((== Nothing) . cError) comps)
      sounds <- filterM (doesDirectoryExist . (samples </>)) =<< listDirectory samples
      let synths = filter (isPitched . T.pack) played
          unsupported = filter (\s -> s `notElem` sounds && not (isPitched (T.pack s))) played
      check ("the example plays bundled samples and stock synths: " <> T.pack (show played)) (any (`elem` sounds) played && null unsupported)
      check "the example uses bass3 and superpiano" (all (`elem` synths) ["bass3", "superpiano"])
      forM_ sounds $ \sound -> do
        files <- filter ((== ".wav") . map toLower . takeExtension) <$> listDirectory (samples </> sound)
        forM_ [0 .. length files - 1] $ \i ->
          -- A sampler of its own, so each file is heard alone.
          offlineSampler 48000 samples >>= \case
            Left e -> check ("the example's samples open: " <> e) False
            Right smp -> do
              now <- getMonotonicTime
              playEvent smp now (Map.fromList [("s", VS sound), ("n", VF (fromIntegral i))])
              out <- renderFrames smp 2400
              check ("the example's " <> T.pack sound <> ":" <> T.pack (show i) <> " sounds") (any ((> 1e-3) . abs . fst) out)
              closeSampler smp

-- | A 16-bit PCM WAV file of interleaved samples between -1 and 1.
wav :: Int -> Int -> [Double] -> BL.ByteString
wav rate channels xs =
  B.toLazyByteString $
    mconcat
      [ B.string7 "RIFF"
      , B.word32LE (36 + bytes)
      , B.string7 "WAVEfmt "
      , B.word32LE 16
      , B.word16LE 1
      , B.word16LE (fromIntegral channels)
      , B.word32LE (fromIntegral rate)
      , B.word32LE (fromIntegral (rate * channels * 2))
      , B.word16LE (fromIntegral (channels * 2))
      , B.word16LE 16
      , B.string7 "data"
      , B.word32LE bytes
      , foldMap (B.int16LE . round . (* 32767)) xs
      ]
  where
    bytes = fromIntegral (2 * length xs)
