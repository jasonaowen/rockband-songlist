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

The names and values of nodes and attributes are custom types that, by default,
require painstaking construction which I found to obscure the readability of
the code. This directive allows for simple string literals in the program to
take the place of complicated constructors.

> {-# LANGUAGE OverloadedStrings #-}

> import Data.ByteString.Char8 as S (unpack)
> import Data.ByteString.Internal
> import Data.ByteString.Lazy.Char8 as B (pack, unpack, readFile, toStrict, fromStrict)
> import Data.ByteString.Lazy.Internal
> import Data.ByteString.Search (replace)
> import Data.Maybe
> import Data.Text as T (Text, splitOn, init, unpack, pack)
> import Data.Yaml.YamlLight
> import System.Environment
> import System.Exit
> import Text.HTML.DOM as H
> import Text.XML
> import Text.XML.Cursor

Parsing the Beatles: Rock Band song list
========================================

The website dynamically loads the list of songs to display by updating the DOM
with a fragment of HTML fetched via AJAX. There are three lists: the songs
available on the game disc, the songs available for download, and the
combination of both. Here we parse these lists.

The list of songs has some but not all of the data needed data, as well as the
relative URL which will be needed to fetch more of the data.

> data SongListElement = SongListElement { list_name  :: Text,
>                                          list_album :: Text,
>                                          list_year  :: Int,
>                                          list_band  :: Int,
>                                          list_url   :: Text }
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
>                     list_year  = (read . T.unpack . T.init . last) albumElements,
>                     list_band  = parseDifficulty difficulty,
>                     list_url   = head (c $| attribute "rel")}
>                   where classChild name = attributeIs "class" name
>                                        >=> child
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

> sanitizeSongDetailDocument  :: Data.ByteString.Lazy.Internal.ByteString -> Data.ByteString.Lazy.Internal.ByteString
> sanitizeSongDetailDocument d = replace "\t"
>                                        ("  " :: Data.ByteString.Lazy.Internal.ByteString)
>                                        (B.toStrict d)

> songDetailDocumentToYaml :: Data.ByteString.Lazy.Internal.ByteString -> IO YamlLight
> songDetailDocumentToYaml = parseYamlBytes . B.toStrict

> parseSongDetailYaml  :: YamlLight -> SongDetail
> parseSongDetailYaml d = SongDetail {
>                           detail_source = (T.pack . S.unpack . value) "type",
>                           detail_guitar = difficulty "guitar_rating",
>                           detail_bass   = difficulty "bass_rating",
>                           detail_drum   = difficulty "drum_rating",
>                           detail_vocal  = difficulty "vocal_rating" }
>                         where value name = fromMaybe "" (lookupYL (YStr name) d >>= unStr)
>                               cur        = fromDocument . H.parseLBS . B.fromStrict . value
>                               difficulty = parseDifficulty . cur

Now that we have all the data about a song, it can be composed into a single object.

> data Song = Song { name  :: Text,
>                    album :: Text,
>                    year  :: Int,
>                    source :: Text,
>                    band  :: Int,
>                    guitar :: Int,
>                    bass :: Int,
>                    drum :: Int,
>                    vocal :: Int }
>             deriving Show

> song    :: SongListElement -> SongDetail -> Song
> song l d = Song {
>              name = list_name l,
>              album = list_album l,
>              year = list_year l,
>              source = detail_source d,
>              band = list_band l,
>              guitar = detail_guitar d,
>              bass = detail_bass d,
>              drum = detail_drum d,
>              vocal = detail_vocal d}

> localFileName  :: Text -> Text
> localFileName = last . (T.splitOn "/")

> main = do
>     lbs <- getArgs >>= parseArgs
>     let doc = H.parseLBS lbs
>         cur = fromDocument doc
>         songNodes = cur $// element "tr"
>         songList = map parseSongNode songNodes
>     songFiles <- mapM (B.readFile . T.unpack . localFileName . list_url) songList
>     songYamls <- mapM (songDetailDocumentToYaml . sanitizeSongDetailDocument) songFiles
>     let songDetails = map parseSongDetailYaml songYamls
>         songs = zipWith song songList songDetails
>     putStrLn (unlines (map show songs))

> parseArgs ["-h"] = usage   >> exit
> parseArgs ["-v"] = version >> exit
> parseArgs [file] = B.readFile file
> parseArgs (_:_ ) = usage   >> die

> usage   = putStrLn "Usage: songlist file"
> version = putStrLn "rockband-songlist 0.1"
> exit    = exitWith ExitSuccess
> die     = exitWith (ExitFailure 1)
