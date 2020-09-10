{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Used in PersistLiteralTestSQL
module NullableGenerated where

import qualified Data.Proxy as P (Proxy(..))
import Init

newtype NullableGenerated a = NullableGenerated (Maybe a) deriving (Show, Eq)

instance forall a. PersistField a => PersistField (NullableGenerated a) where
  toPersistValueInsert _ = PersistLiteral "DEFAULT"
  toPersistValueUpdate _ = PersistLiteral "DEFAULT"
  toPersistValue (NullableGenerated a) = toPersistValue a
  fromPersistValue g = NullableGenerated <$> fromPersistValue g

instance forall a. PersistFieldSql a => PersistFieldSql (NullableGenerated a) where
  sqlType _ = sqlType (P.Proxy @a)

unNullableGenerated :: NullableGenerated a -> Maybe a
unNullableGenerated (NullableGenerated a) = a

fakeNullableGenerated :: NullableGenerated a
fakeNullableGenerated = NullableGenerated Nothing
