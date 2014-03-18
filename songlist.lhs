rockband-songlist, to generate a printable list of songs in Rock Band.
Copyright (C) 2014 Jason Owen <jason.a.owen@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

---

The names and values of nodes and attributes are various non-string types that,
by default, require painstaking construction which I found to obscure the
readability of the code. This directive allows for simple string literals in
the program to take the place of complicated constructors.

> {-# LANGUAGE OverloadedStrings #-}

> import Data.ByteString.Char8 as S (unpack)
> import Data.ByteString.Lazy.Char8 as B (toStrict, fromStrict)
> import Data.ByteString.Lazy.Internal (ByteString)
> import Data.ByteString.Search (replace)
> import Data.Maybe (fromJust, fromMaybe)
> import Data.Text as T (Text, splitOn, init, unpack, pack)
> import Data.Yaml.YamlLight (lookupYL, parseYamlBytes, unStr, YamlLight(YStr))
> import Network.HTTP
> import Network.URI (parseRelativeReference, parseURI, relativeTo, URI)
> import System.Environment (getArgs)
> import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
> import Text.HTML.DOM as H (parseLBS)
> import Text.XML.Cursor

Parsing the Beatles: Rock Band song list
========================================

The website dynamically loads the list of songs to display by updating the DOM
with a fragment of HTML fetched via AJAX. There are three lists: the songs
available on the game disc, the songs available for download, and the
combination of both.

> rootUri = fromJust
>         . parseURI
>         $ "http://www.thebeatlesrockband.com/"
> allUri  = buildUri
>         . fromJust
>         . parseRelativeReference
>         $ "/music/songFilter/ALL"
> discUri = buildUri
>         . fromJust
>         . parseRelativeReference
>         $ "/music/songFilter/DISC"
> dlcUri  = buildUri
>         . fromJust
>         . parseRelativeReference
>         $ "/music/songFilter/DLC"

The list of songs has some but not all of the needed data, as well as the
relative URL which will be needed to fetch more of the data.

> data SongListElement = SongListElement { list_name  :: Text,
>                                          list_album :: Text,
>                                          list_year  :: Int,
>                                          list_band  :: Int,
>                                          list_uri   :: URI }
>                        deriving Show

Each row of the song list is in the following format:

<tr class="song-row" rel="/music/example">
  <td class="song">An Example Song</td>
  <td class="album">The Examplest Album Ever (1970)</td>
  <td class="difficulty"><p>Band</p> <div class="rating three-stars"><span>Three stars</span></div></td>
</tr>

The year the album was released is part of the album `td`, and the overall
difficulty of the song is specified both in the `div` class and as text in the
inner `span` element.

> parseSongNode  :: Cursor -> SongListElement
> parseSongNode c = SongListElement {
>                     list_name  = classContent "song",
>                     list_album = head albumElements,
>                     list_year  = read
>                                . T.unpack
>                                . T.init
>                                . last
>                                $ albumElements,
>                     list_band  = parseDifficulty difficulty,
>                     list_uri   = fromJust
>                                . parseRelativeReference
>                                . T.unpack
>                                . head
>                                $ (c $| attribute "rel")}
>                   where classChild name = attributeIs "class" name
>                                       >=> child
>                         classContent name = head (c $/ (classChild name)
>                                                     >=> content)
>                         albumElements = splitOn " (" (classContent "album")
>                         difficulty = head (c $/ (classChild "difficulty")
>                                              >=> element "div")

I found matching on the `class` attribute of the `div` element to be simpler
than getting the content of the child of the `div` element.

> parseDifficulty  :: Cursor -> Int
> parseDifficulty c | rating == "rating zero-stars"  = 0
>                   | rating == "rating one-star"    = 1
>                   | rating == "rating two-stars"   = 2
>                   | rating == "rating three-stars" = 3
>                   | rating == "rating four-stars"  = 4
>                   | rating == "rating five-stars"  = 5
>                   | rating == "rating six-stars"   = 6
>                     where rating = head (c $| attribute "class")

Apostrophies are represented in the song list by &rsquo; aka right single
quotation mark, which gets translated poorly. Before parsing the document,
clean it up by replacing them with regular quotation marks.

> sanitizeSongList  :: ByteString -> ByteString
> sanitizeSongList l = replace "&rsquo;"
>                              ("&apos;" :: ByteString)
>                              (B.toStrict l)

Parsing the Beatles: Rock Band song details
===========================================

The relative URL in the song list points at a JSON-like document. It has
elements we want - the various instrument difficulties and the source of the
song - as well as several we don't want, such as leaderboard stats and IDs to
find the song on various other services.

> data SongDetail = SongDetail { detail_source :: Text,
>                                detail_guitar :: Int,
>                                detail_bass   :: Int,
>                                detail_drum   :: Int,
>                                detail_vocal  :: Int }
>                     deriving (Show)

The detail document is not actually well-formed JSON: it does not surround the
object keys with double quotation marks, and it variously uses single and
double quotation marks around the values.

Rather than trying to fix these issues to allow the documents to be parsed as
JSON, I found it easier to clean them up enough to parse them as YAML. To do
so, all that needs to be done is to convert the tabs into spaces.

> sanitizeSongDetailDocument  :: ByteString -> ByteString
> sanitizeSongDetailDocument d = replace "\t"
>                                        ("  " :: ByteString)
>                                        (B.toStrict d)

> songDetailDocumentToYaml :: ByteString -> IO YamlLight
> songDetailDocumentToYaml = parseYamlBytes . B.toStrict

> parseSongDetailYaml  :: YamlLight -> SongDetail
> parseSongDetailYaml d = SongDetail {
>                           detail_source = T.pack
>                                         . S.unpack
>                                         . value
>                                         $ "type",
>                           detail_guitar = difficulty "guitar_rating",
>                           detail_bass   = difficulty "bass_rating",
>                           detail_drum   = difficulty "drum_rating",
>                           detail_vocal  = difficulty "vocal_rating" }
>                         where value name = fromMaybe "" (lookupYL (YStr name) d >>= unStr)
>                               cur        = fromDocument
>                                          . H.parseLBS
>                                          . B.fromStrict
>                                          . value
>                               difficulty = parseDifficulty
>                                          . cur

Now that we have all the data about a song, it can be composed into a single object.

> data Song = Song { name   :: Text,
>                    album  :: Text,
>                    year   :: Int,
>                    source :: Text,
>                    band   :: Int,
>                    guitar :: Int,
>                    bass   :: Int,
>                    drum   :: Int,
>                    vocal  :: Int }
>             deriving Show

> song    :: SongListElement -> SongDetail -> Song
> song l d = Song { name   = list_name  l,
>                   album  = list_album l,
>                   year   = list_year  l,
>                   source = detail_source d,
>                   band   = list_band  l,
>                   guitar = detail_guitar d,
>                   bass   = detail_bass   d,
>                   drum   = detail_drum   d,
>                   vocal  = detail_vocal  d }

Rendering a song list
=====================

A comma separated value file is a simple format that can be parsed by several
tools, including, most importantly, spreadsheet software. Once the data is in
a spreadsheet, it can easily be sorted on any of its fields, and it can be
prepared and printed simply.

To ease sorting, the first line should name each of the fields.

> csvHeader :: String
> csvHeader = "Song Name,Album,Year,Source,Band Difficulty,"
>          ++ "Guitar Difficulty,Bass Difficulty,Drum Difficulty,"
>          ++ "Vocal Difficulty"

Then, each line needs to have the data, separated by commas.

> csv :: Song -> String
> csv s =           show (name   s)
>         ++ "," ++ show (album  s)
>         ++ "," ++ show (year   s)
>         ++ "," ++ show (source s)
>         ++ "," ++ show (band   s)
>         ++ "," ++ show (guitar s)
>         ++ "," ++ show (bass   s)
>         ++ "," ++ show (drum   s)
>         ++ "," ++ show (vocal  s)

Fetching data
=============

Each of the rows in the song list contain a relative URL used to fetch the
song detail. These relative URLs cannot be used alone, but instead need to be
anchored to an absolute URL to be fetched.

> buildUri    :: URI -> URI
> buildUri rel = relativeTo rel rootUri

One of the idiosyncracies of the Beatles: Rock Band web site is that the
request to fetch a song detail document needs to include a particular header
that browsers fill in automatically. Without it, the web service surrounds
the content with the standard HTML headers and footers, making it impossible
to parse the result.

> customHeader = Header (HdrCustom "X-Requested-With") "XMLHttpRequest"

Fetching a URI involves first building the request containing the URI, the
method (GET), and our custom header, and then calling the library to carry out
the request.

> buildReq uri = Request uri GET [customHeader] ("" :: ByteString)
> fetch = simpleHTTP . buildReq

> main = do
>      songListResponse <- getArgs
>                      >>= parseArgs
>                      >>= getResponseBody
>      let songList = (map parseSongNode)
>                   . ($// element "tr")
>                   . fromDocument
>                   . H.parseLBS
>                   . sanitizeSongList
>                   $ songListResponse
>      songDetailYamls <- mapM ( fetch
>                              . buildUri
>                              . list_uri )
>                              songList
>                     >>= mapM getResponseBody
>                     >>= mapM ( songDetailDocumentToYaml
>                              . sanitizeSongDetailDocument )
>      let songDetails = map parseSongDetailYaml songDetailYamls
>          songs = zipWith song songList songDetails
>      putStrLn csvHeader
>      putStrLn . unlines
>               . (map csv)
>               $ songs

> parseArgs ["-h"  ] = usage   >> exit
> parseArgs ["-v"  ] = version >> exit
> parseArgs ["all" ] = fetch allUri
> parseArgs ["disc"] = fetch discUri
> parseArgs ["dlc" ] = fetch dlcUri
> parseArgs (_:_   ) = usage   >> die
> parseArgs [      ] = usage   >> die

> usage   = putStrLn "Usage: songlist (all|disc|dlc)"
> version = putStrLn "rockband-songlist 0.1"
> exit    = exitWith ExitSuccess
> die     = exitWith (ExitFailure 1)
