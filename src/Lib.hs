{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Graphql (api)
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Morpheus                  ( Interpreter(..) )
import           Data.Morpheus.Document         ( toGraphQLDocument )
import           Data.Morpheus.Server           ( GQLState
                                                , gqlSocketApp
                                                , initGQLState
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWs
import           Network.WebSockets             ( defaultConnectionOptions )
import           Web.Scotty                     ( body
                                                , file
                                                , get
                                                , post
                                                , raw
                                                , scottyApp
                                                , scotty
                                                )

someFunc :: IO ()
someFunc = scotty 3000 $ post "/graphql" $ raw =<< (liftIO . api =<< body)