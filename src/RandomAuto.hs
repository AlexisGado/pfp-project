module RandomAuto (rolls) where
import qualified Data.List     as List
import qualified System.Random as Random

-- Inspired from https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html
rolls :: Int -> Int -> [Int]
rolls n seed = take n . List.unfoldr (Just . Random.uniformR (1, 6)) $ Random.mkStdGen seed
