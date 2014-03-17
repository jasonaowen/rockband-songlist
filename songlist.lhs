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

> import System.Environment
> import System.Exit
> import Data.ByteString.Lazy.Char8 as B (pack, unpack, readFile)
> import Text.HTML.DOM as H
> import Text.XML
> import Text.XML.Cursor
> import Data.Text as T (Text, splitOn, init, unpack)

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

> main = do
>     lbs <- getArgs >>= parseArgs
>     let doc = H.parseLBS lbs
>         cur = fromDocument doc
>         songNodes = cur $// element "tr"
>         songs = map parseSongNode songNodes
>     putStrLn (unlines (map show songs))

> parseArgs ["-h"] = usage   >> exit
> parseArgs ["-v"] = version >> exit
> parseArgs [file] = B.readFile file
> parseArgs (_:_ ) = usage   >> die

> usage   = putStrLn "Usage: songlist file"
> version = putStrLn "rockband-songlist 0.1"
> exit    = exitWith ExitSuccess
> die     = exitWith (ExitFailure 1)
