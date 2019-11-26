{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deity ( Deity(..), dbDeity) where

import           Data.Morpheus.Kind     (OBJECT)
import           Data.Morpheus.Types    (GQLType (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

data Deity = Deity
  { fullName :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  } deriving (Generic)

instance GQLType Deity where
  type KIND Deity = OBJECT
  description _ = Just "Custom Description for Client Defined User Type"

dbDeity :: Text -> Maybe Text -> IO Deity
dbDeity name _ = do
--                 user <- unliftIO $ runDB $ getJustEntity userId
                 return $ Deity {fullName = "Hi, " <> name, power = Just "Shapeshifting"}

