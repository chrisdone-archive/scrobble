{-# LANGUAGE DeriveDataTypeable #-}

-- | Scrobbling data types.

module Scrobble.Types where

import Control.Exception
import Data.Data
import Data.Time
import Network
import Network.URI (URI)

-- | Server configuration.
data Config = Config
  { cfgPort   :: PortNumber      -- ^ Port to listen on.
  , cfgHost   :: String          -- ^ Host name used for server (probably just localhost).
  , cfgExpire :: NominalDiffTime -- ^ Number of seconds of inactivity before a session expires.
  } deriving (Show)

-- | Event handlers.
data Handlers = Handlers
  { handleHandshake   :: Session -> IO ()                   -- ^ Initial connection hand-shake.
  , handleExpire      :: Session -> IO ()                   -- ^ Session expiry.
  , handleNowPlaying  :: Session -> NowPlaying -> IO ()     -- ^ Now-playing notification.
  , handleSubmissions :: Session -> [Submission] -> IO Bool -- ^ Played tracks submission.
  }

-- | A scrobbling session.
data Session = Session
  { sesHandshake :: Bool    -- ^ Does the session require handshake?
  , sesVersion   :: String  -- ^ Version of the protocol.
  , sesClientId  :: String  -- ^ Client (music player's) id.
  , sesClientVer :: String  -- ^ Client version.
  , sesUser      :: String  -- ^ Username.
  , sesTimestamp :: UTCTime -- ^ Timestamp of connection.
  , sesToken     :: String  -- ^ Session token.
  } deriving (Show)

-- | A now playing track.
data NowPlaying = NowPlaying
  { npArtist      :: String        -- ^ Artist name.
  , npTrack       :: String        -- ^ Track title.
  , npAlbum       :: Maybe String  -- ^ Album name (if any).
  , npLength      :: Maybe Integer -- ^ Track length in seconds (if known).
  , npPosition    :: Maybe Integer -- ^ Track position (if known).
  , npMusicBrainz :: Maybe String  -- ^ MusicBrainz track id (if known).
  } deriving (Show)

-- | A track submission.
data Submission = Submission
  { subArtist      :: String        -- ^ Artist name.
  , subTrack       :: String        -- ^ Track title.
  , subTimestamp   :: UTCTime       -- ^ Track timestamp.
  , subSource      :: Source        -- ^ Source of track.
  , subRating      :: Maybe Rating  -- ^ Rating (if any).
  , subLength      :: Maybe Integer -- ^ Track length (if any).
  , subAlbum       :: Maybe String  -- ^ Album (if any).
  , subPosition    :: Maybe Integer -- ^ Track position in album (if any).
  , subMusicBrainz :: Maybe String  -- ^ MusicBrainz track id (if any).
  } deriving (Show)

-- | A rating of a track.
--
-- Note: Currently, a web-service must also be called to set love/ban
-- status. We anticipate that this will be phased out soon, and the
-- submission service will handle the whole process.
data Rating
  = Love -- ^ Love (on any mode if the user has manually loved the
         -- track). This implies a listen.

  | Ban -- ^ Ban (only if source=L). This implies a skip, and the
        -- client should skip to the next track when a ban happens.

  | Skip -- ^ Skip (only if source=L).

  deriving (Enum,Eq,Show,Read)

-- | The source of the track. Required, must be one of the following
-- codes:
--
-- Please note, for the time being, sources other than P and L are not
-- supported.
data Source
  = UserChosen -- ^ Chosen by the user

  | NonPersonlizedBroadcast -- ^ Non-personalised broadcast
                            -- (e.g. Shoutcast, BBC Radio 1)

  | Personalized -- ^ Personalised recommendation except Last.fm
                 -- (e.g. Pandora, Launchcast)

  | LastFm -- ^ Last.fm (any mode). In this case, the 5-digit Last.fm
           -- recommendation key must be appended to this source ID to
           -- prove the validity of the submission (for example,
           -- "o[0]=L1b48a").

  | Unknown -- ^ Source unknown.

  deriving (Show,Enum,Eq,Read)

-- | Server response.
data Response = OK | BANNED | BADAUTH | FAILED String | BADSESSION
  deriving Show

-- | A scrobbling client.
data Client = Client
  { cliToken      :: String -- ^ Session token.
  , cliNowPlaying :: URI    -- ^ Now playing URL to submit to.
  , cliSubmit     :: URI    -- ^ URL to submit listened tracks to.
  } deriving (Show)

-- | Details for creating a scrobbling client.
data Details = Details
  { detPassword :: String
  , detUsername :: String
  , detClient :: String -- ^ E.g. “qlb”.
  , detVersion :: String -- ^ E.g. “0.9.2”.
  , detServer :: URI
  } deriving (Show)

-- | Scrobble exception.
data ScrobblerError
  = ScrobblerBanned
  | ScrobblerBadAuth
  | ScrobblerBadTime
  | ScrobblerFailed String
  | ScrobblerHardFail
  deriving (Show,Typeable,Data)
instance Exception ScrobblerError
