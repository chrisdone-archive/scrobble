## What this package is

This package comes as a library and demo server (named
`scrobble-server`) for scrobbling via the procotol of
Audioscrobbler. The demo is merely for a demonstration, you should use
the server as a library and do what you want with the scrobbled
tracks.

See [the Audioscrobbler documentation for the
protocol.](http://www.audioscrobbler.net/development/protocol/)

## Example usage

    $ scrobble-server 8910
    New session: Session {sesHandshake = True, sesVersion = "1.2",
      sesClientId = "qlb", sesClientVer = "0.9.2",
      sesUser = "christopherdone",sesTimestamp = 2012-06-10 18:38:40 UTC,
      sesToken = "f8bfeb761205fd72abf78b4dc54724f2"}
    Now playing: NowPlaying {npArtist = "Aphex Twin",
      npTrack = "XMD5A",npAlbum = Just "Analord 10",
      npLength = Just 478, npPosition = Just 2, npMusicBrainz = Nothing}
    Listened: Submission {subArtist = "Aphex Twin",
      subTrack = "XMD5A", subTimestamp = 2012-06-10 18:42:39 UTC,
      subSource = UserChosen, subRating = Nothing, subLength = Just 478,
      subAlbum = Just "Analord 10", subPosition = Just 2,
      subMusicBrainz = Nothing}

## How to use with your music player

To use, configure your music player and change the
Last.fm/Audioscrobbling plugin you have to point to your localhost.

### Clementine

For Clementine, there is no such option. But you can instead go to the
proxy settings and choose HTTP proxy, and enter the details of the
server instead. Scrobble-server ignores the proxying part of the
request.

### Quod Libet

Go to Music → Plugins and check the AudioScrobbler plugin. Choose
“Other” and enter the server/port of the server. The username and
password don't matter.
