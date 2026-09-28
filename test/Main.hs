module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lunar.Codegen (songCodeText, sourceMini)
import Lunar.Compile (compileSong, cError)
import Lunar.Model
import Lunar.Refactor
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

  let demoMinis = map sourceMini (songTracks demoSong)
  check ("demo minis " <> T.pack (show demoMinis)) (take 3 demoMinis == ["bd*4", "sn(3,8,2)", "[hh hh:2 hh [hh hh:1]]*2"])
  check "demo compiles" (all ((== Nothing) . cError) (compileSong demoSong))
  T.putStrLn (songCodeText demoSong)
  n <- readIORef failures
  unless (n == 0) exitFailure
