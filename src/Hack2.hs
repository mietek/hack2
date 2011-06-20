{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Hack2 where

import Data.Default (def, Default)
import System.IO (stderr)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString as Strict

import Data.Enumerator (Enumerator, enumEOF)

type Application = Env -> IO Response
type Middleware  = Application -> Application

data RequestMethod =
     OPTIONS
  |  GET
  |  HEAD
  |  POST
  |  PUT
  |  DELETE
  |  TRACE
  |  CONNECT
  deriving (Show, Read, Eq)

data HackUrlScheme = HTTP | HTTPS deriving (Show, Read, Eq)

newtype HackErrors = HackErrors { unHackErrors :: ByteString -> IO () }

instance Show HackErrors where
  show _ = "HackErrors"

instance Default HackErrors where
  def = HackErrors (B.hPutStr stderr)

newtype HackEnumerator = HackEnumerator { unHackEnumerator :: (forall a. Enumerator Strict.ByteString IO a) }

instance Show HackEnumerator where
  show _ = "HackEnumerator"

instance Default HackEnumerator where
  def = HackEnumerator enumEOF

data Env = Env 
  {  requestMethod  :: RequestMethod
  ,  scriptName     :: ByteString
  ,  pathInfo       :: ByteString
  ,  queryString    :: ByteString
  ,  serverName     :: ByteString
  ,  serverPort     :: Int
  ,  httpHeaders    :: [(ByteString, ByteString)]
  ,  hackVersion    :: (Int, Int, Int)
  ,  hackUrlScheme  :: HackUrlScheme
  ,  hackInput      :: HackEnumerator
  ,  hackErrors     :: HackErrors
  ,  hackHeaders    :: [(ByteString, ByteString)]
  }
  deriving (Show)

data Response = Response
  {  status   :: Int
  ,  headers  :: [(ByteString, ByteString)]
  ,  body     :: HackEnumerator
  }
  deriving (Show)

instance Default RequestMethod where
  def = GET

instance Default HackUrlScheme where
  def = HTTP

instance Default Response where
  def = Response 
    {
      status  = 200
    , headers = []
    , body    = def
    }

instance Default Env where
  def = Env 
    {
        requestMethod = def
      , scriptName    = B.empty
      , pathInfo      = B.empty
      , queryString   = B.empty
      , serverName    = B.empty
      , serverPort    = def
      , httpHeaders   = def
      , hackVersion   = currentVersion
      , hackUrlScheme = def
      , hackInput     = def
      , hackErrors    = def
      , hackHeaders   = def
    }
    where
      currentVersion = (2011, 6, 19)
