-- Used in PersistLiteralTestSQL
module Timestamps where

import Init

import Data.Proxy (Proxy(..))
import Data.Time (UTCTime(..), fromGregorian)

newtype CreatedAt = CreatedAt UTCTime
  deriving (Eq, Show)

instance PersistField CreatedAt where
  toPersistValueInsert _ = PersistLiteral "now()"
  toPersistValue (CreatedAt utct) = toPersistValue utct
  fromPersistValue utct = CreatedAt <$> fromPersistValue utct

instance PersistFieldSql CreatedAt where
  sqlType _ = sqlType (Proxy :: Proxy UTCTime)

fakeCreatedAt :: CreatedAt
fakeCreatedAt = CreatedAt (UTCTime (fromGregorian 1970 1 1) 0)

newtype UpdatedAt = UpdatedAt UTCTime
  deriving (Eq, Show)

instance PersistField UpdatedAt where
  toPersistValueInsert _ = PersistLiteral "now()"
  toPersistValueUpdate _ = PersistLiteral "now()"
  toPersistValue (UpdatedAt utct) = toPersistValue utct
  fromPersistValue utct = UpdatedAt <$> fromPersistValue utct

instance PersistFieldSql UpdatedAt where
  sqlType _ = sqlType (Proxy :: Proxy UTCTime)

fakeUpdatedAt :: UpdatedAt
fakeUpdatedAt = UpdatedAt (UTCTime (fromGregorian 1970 1 1) 0)
