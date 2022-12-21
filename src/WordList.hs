module WordList (buildList) where
import           Data.Char    (isAlpha)
import qualified Data.Set     as Set
import qualified Data.Text    as T
import           Data.Text.IO as TIO (readFile)

buildList :: FilePath -> IO [[Char]]
buildList filename = do
 h <- TIO.readFile filename
 let l = T.words h
 let lnorm = map normalize l
 let lnormset = Set.fromList lnorm
 let lnormunique = Set.toList lnormset
 return lnormunique

normalize :: T.Text -> [Char]
normalize string = [ x | x <- a, isAlpha x ] where a = T.unpack (T.toLower string)
