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

> import System.Environment
> import System.Exit

> main = getArgs >>= parse >>= putStr

> parse ["-h"] = usage   >> exit
> parse ["-v"] = version >> exit
> parse [file] = readFile file
> parse (_:_ ) = usage   >> die

> usage   = putStrLn "Usage: songlist file"
> version = putStrLn "rockband-songlist 0.1"
> exit    = exitWith ExitSuccess
> die     = exitWith (ExitFailure 1)
