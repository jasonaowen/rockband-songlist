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

> main = do
>     lbs <- getArgs >>= parse
>     let doc = H.parseLBS lbs
>         cur = fromDocument doc
>         songNodes = cur $// attributeIs "class" "song-row"
>     putStr (show songNodes)

> parse ["-h"] = usage   >> exit
> parse ["-v"] = version >> exit
> parse [file] = B.readFile file
> parse (_:_ ) = usage   >> die

> usage   = putStrLn "Usage: songlist file"
> version = putStrLn "rockband-songlist 0.1"
> exit    = exitWith ExitSuccess
> die     = exitWith (ExitFailure 1)
