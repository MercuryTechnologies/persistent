{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module PersistLiteralTestSQL (specsWith) where

import Init
import NullableGenerated
import Timestamps

import Control.Concurrent (threadDelay)

import qualified Data.Text as T

share [mkPersist sqlSettings { mpsGeneric = True }] [persistLowerCase|
  PersistLiteralFieldTestTable
    fieldOne Text Maybe
    fieldTwo Text Maybe
    fieldThree (NullableGenerated Text)
    createdAt CreatedAt
    updatedAt UpdatedAt
    deriving Show Eq
|]

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDB = describe "PersistLiteral field" $ do
  it "should read a GENERATED column" $ runDB $ do
    -- we have to manage the table manually because of the generated field
    _ <- rawExecute "CREATE TABLE persist_literal_field_test_table (id serial primary key, field_one text, field_two text, field_three text GENERATED ALWAYS AS (COALESCE(field_one, field_two)) STORED, created_at TIMESTAMP WITH TIME ZONE NOT NULL, updated_at TIMESTAMP WITH TIME ZONE NOT NULL);" []

    insert_ $ PersistLiteralFieldTestTable (Just "ValueForFieldOne") (Just "ValueForFieldTwo") fakeNullableGenerated fakeCreatedAt fakeUpdatedAt

    Just (Entity k (PersistLiteralFieldTestTable _ _ g c u)) <- selectFirst [] []

    liftIO $ threadDelay 1000000 -- Gotta find a better way to do this.

    update k $ [PersistLiteralFieldTestTableFieldTwo =. Just "NewValueForFieldTwo"]

    Just (Entity _ (PersistLiteralFieldTestTable _ _ g' c' u')) <- selectFirst [] []

    _ <- rawExecute "DROP TABLE persist_literal_field_test_table;" []

    liftIO $ unNullableGenerated g @?= Just "ValueForFieldOne"
    liftIO $ assertBool "Should not insert fake data." $
      fakeCreatedAt `notElem` [c, c'] && fakeUpdatedAt `notElem` [u, u']
    liftIO $ assertBool "Generated fields should be the same." (g == g')
    liftIO $ assertBool "CreatedAt fields should be the same." (c == c')
    liftIO $ assertBool "UpdatedAt fields should not be the same." (u /= u')
