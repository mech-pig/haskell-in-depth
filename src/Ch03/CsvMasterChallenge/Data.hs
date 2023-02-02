{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch03.CsvMasterChallenge.Data where

import Control.Applicative (pure, (<*>))
import Control.Monad.Fail (fail)
import Data.Bool (Bool (..))
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (FromField (..), FromRecord (..), HasHeader, decode, index)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Foldable (length)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Text (Text, pack)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Vector (Vector)
import Text.Show (Show, show)
import Prelude (otherwise, (.), (<=), (==))

newtype Name = Name Text deriving (Eq, Show)

data Gender = Female | Male deriving (Eq, Show)

data Person = Person {name :: Name, gender :: Gender, birthDate :: Day} deriving (Eq, Show)

data Stats = Stats {statsFemalesCount :: Int, statsOldestPerson :: Maybe Person} deriving (Eq, Show)

instance FromField Day where
  parseField = unpack |> parseTimeM True defaultTimeLocale "%Y-%m-%d"

instance FromField Gender where
  parseField "Female" = pure Female
  parseField "Male" = pure Male
  parseField s = fail ("Cannot parse gender:" <> unpack s)

instance FromField Name where
  parseField = unpack |> Data.Text.pack |> Name |> pure

instance FromRecord Person where
  parseRecord v
    | length v == 3 = Person <$> index v 0 <*> index v 1 <*> index v 2
    | otherwise = fail ("Cannot parse record: " <> show v)

decodePerson :: HasHeader -> ByteString -> Either String (Vector Person)
decodePerson = decode

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f g = g . f