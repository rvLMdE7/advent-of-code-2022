module Common
    ( Parser

    , readFileUtf8
    , readInputFileUtf8

    , show'
    , readMaybe'

    , (<<$>>)
    ) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.ByteString qualified as Byte
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Read (readMaybe)

import Paths_adventofcode2022 (getDataFileName)


type Parser = Parsec Void Text

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = Byte.readFile >>> fmap Text.Enc.decodeUtf8

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8

show' :: Show a => a -> Text
show' = show >>> Text.pack

readMaybe' :: Read a => Text -> Maybe a
readMaybe' = Text.unpack >>> readMaybe

-- | Same as '<$>' but works inside two levels of functors.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <<$>> xs = fmap f <$> xs
