{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | A client for scrobbling, based upon the Audioscrobbler Realtime
-- Submission protocol v1.2
-- <http://www.audioscrobbler.net/development/protocol/>

module Scrobble.Client
  where

import Scrobble.Types

import Control.Arrow
import Control.Exception
import Data.Hash.MD5 (Str(..),md5s)
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Network.Curl
import Network.URI
import System.Locale

-- | Get a session token.
--
-- The algorithm for generating this token is as follows:
-- token := md5(md5(password) + timestamp)
--
-- The md5() function takes a string and returns the 32-byte ASCII
-- hexadecimal representation of the MD5 hash, using lower case
-- characters for the hex values.
getToken :: String    -- ^ The password.
         -> IO (String,String) -- ^ A session token and timestamp.
getToken password = do
  timestamp <- getCurrentTime
  let et = epoch timestamp
  return (md5 (md5 password ++ et),et)

    where md5 = md5s . Str

-- | Create a client session. Throws "ScrobblerError".
newClient :: Details -> IO Client
newClient Details{..} = withCurlDo $ do
  (token,timestamp) <- getToken detPassword
  let params = [("hs","true")
               ,("p","1.2")
               ,("c",detClient)
               ,("v",detVersion)
               ,("u",detUsername)
               ,("t",timestamp)
               ,("a",token)]
  response <- curlGrab (setQuery detServer params)
           [CurlHttpHeaders [ "Host: " ++ host
                            | Just host <- [fmap uriRegName (uriAuthority detServer)] ]]
  parseAuth response

-- | Parse the auth response.
parseAuth :: CurlGrab -> IO Client
parseAuth CurlGrab{..} =
  case lines grabBody of
    ["OK",token,parseURI -> Just nowplaying,parseURI -> Just submit] ->
      return (Client token nowplaying submit)
    ["BANNED"] -> throw ScrobblerBanned
    ["BADAUTH"] -> throw ScrobblerBadAuth
    ["BADTIME"] -> throw ScrobblerBadTime
    [other] | isPrefixOf failed other ->
              throw (ScrobblerFailed (drop (length failed) other))
            | otherwise -> throw ScrobblerHardFail
  where failed = "FAILED "

-- | Default Audioscrobbler server: http://post.audioscrobbler.com/
defaultServer :: URI
defaultServer = fromJust (parseURI "http://post.audioscrobbler.com/")

-- | Send a now playing message.
nowPlaying :: Client -> NowPlaying -> IO CurlGrab
nowPlaying client@Client{..} nowplaying = do
  curlGrab cliNowPlaying
           [CurlPost True
           ,CurlPostFields (map keyval (makeNowPlaying client nowplaying))]

-- | Make a now playing query.
makeNowPlaying :: Client -> NowPlaying -> [(String,String)]
makeNowPlaying Client{..} NowPlaying{..} =
  [("s",cliToken)
  ,("a",npArtist)
  ,("t",npTrack)
  ,("b",fromMaybe "" npAlbum)
  ,("l",maybe "" show npLength)
  ,("n",maybe "" show npPosition)
  ,("m",fromMaybe "" npMusicBrainz)]
  
-- | Submit a track.
submitTrack :: Client -> [Submission] -> IO CurlGrab
submitTrack client@Client{..} submissions = do
  print (intercalate "&" (map keyval params))
  curlGrab cliSubmit
           [CurlPost True
           ,CurlPostFields (map keyval params)]
  
  where params = [("s",cliToken)] ++
                 concat (zipWith (makeSubmission client) [0..] submissions)

-- | Make a now playing query.
makeSubmission :: Client -> Integer -> Submission -> [(String,String)]
makeSubmission Client{..} i Submission{..} = map hookup
  [("a",subArtist)
  ,("t",subTrack)
  ,("i",epoch subTimestamp)
  ,("o",fromMaybe "U" (lookup subSource sources))
  ,("r",fromMaybe "" (subRating >>= \r -> lookup r ratings))
  ,("l",maybe "" show subLength)  
  ,("b",fromMaybe "" subAlbum)
  ,("n",maybe "" show subPosition)
  ,("m",fromMaybe "" subMusicBrainz)]
  
  where sources = [(UserChosen,"P")
                  ,(NonPersonlizedBroadcast,"R")
                  ,(Personalized,"E")
                  ,(LastFm,"L")]
        ratings = [(Love,"L"),(Ban,"B"),(Skip,"S")]
        hookup (k,v) = (k ++ "[" ++ show i ++ "]",v)

--------------------------------------------------------------------------------
-- Utilities

-- | Encode post parameters to a string.
encodePost :: [(String,String)] -> String
encodePost = intercalate "&" . map (keyval . (encode *** encode)) where
  encode = escapeURIString isUnescapedInURI

-- | Make a key=val string.
keyval :: (String,String) -> String
keyval (key,val) = key ++ "=" ++ val

-- | Set a URI's query.
setQuery :: URI -> [(String,String)] -> URI
setQuery uri assoc = uri { uriQuery = "?" ++ encodePost assoc }

-- | Format a time to UNIX number.
epoch :: UTCTime -> String
epoch = formatTime defaultTimeLocale "%s"

--------------------------------------------------------------------------------
-- Make Curl's API not crappy.

-- | Grab a URL with curl.
curlGrab :: URI -> [CurlOption] -> IO CurlGrab 
curlGrab url options = do
  CurlResponse{..} <- curlGetResponse_ (show url) options
  return $ CurlGrab respCurlCode respStatus respStatusLine respHeaders respBody

-- | A sane data type.
data CurlGrab = CurlGrab
  { grabCode       :: CurlCode
  , grabStatus     :: Int
  , grabStatusLine :: String
  , grabHeaders    :: [(String,String)]
  , grabBody       :: String
  } deriving (Show)
