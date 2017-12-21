module Day21input exposing (..)


testInput =
    """../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"""


testInput2 =
    """../.# => ##./#../...
#../#.#/##. => #..#/..../..../#..#
../## => #../#.#/##.
.#/## => #../#.#/##.
../.. => #../#.#/##."""


input =
    """../.. => ###/.#./.##
#./.. => .../###/#.#
##/.. => #.#/..#/..#
.#/#. => ##./.##/..#
##/#. => ###/.../###
##/## => ###/##./.#.
.../.../... => .#../..##/...#/....
#../.../... => .#../..##/.##./####
.#./.../... => .#../###./.#../#..#
##./.../... => #.##/#..#/...#/.#..
#.#/.../... => ##.#/.#.#/#.#./..##
###/.../... => #..#/#.##/..../#.##
.#./#../... => .#.#/.#../..../.#.#
##./#../... => ###./..../.##./###.
..#/#../... => .#../###./####/.#..
#.#/#../... => ..##/.#../#.#./##.#
.##/#../... => #..#/##../.###/#...
###/#../... => #.##/##../.#.#/####
.../.#./... => ####/.###/..#./###.
#../.#./... => ###./..../#.../#...
.#./.#./... => .#../###./.#.#/....
##./.#./... => #.##/#..#/.#.#/##..
#.#/.#./... => #.../..../##../....
###/.#./... => ..../...#/##../####
.#./##./... => ..../.###/.#.#/#...
##./##./... => ..##/.##./###./#.##
..#/##./... => ...#/#.#./#.#./#..#
#.#/##./... => ..##/###./#.##/..#.
.##/##./... => .###/..../##../#.##
###/##./... => .#../...#/..##/##..
.../#.#/... => ...#/#.##/#.../####
#../#.#/... => .##./.#../###./.###
.#./#.#/... => ##.#/.#.#/#.../.##.
##./#.#/... => ####/#..#/..#./....
#.#/#.#/... => #.##/.##./####/.#..
###/#.#/... => ..##/..#./#..#/.#..
.../###/... => #..#/#.../.##./.##.
#../###/... => ##../###./#.##/####
.#./###/... => .#../..##/#..#/...#
##./###/... => ..#./#..#/.###/..#.
#.#/###/... => #..#/#.#./#.#./#.##
###/###/... => #.../.##./..../.##.
..#/.../#.. => .###/.##./.##./#.##
#.#/.../#.. => #.../..#./.###/...#
.##/.../#.. => #..#/..../.##./.#.#
###/.../#.. => .##./##.#/.#.#/##..
.##/#../#.. => ...#/#.##/.#../.#..
###/#../#.. => ##.#/#.#./#.../##..
..#/.#./#.. => .#../#.../#.../####
#.#/.#./#.. => .##./.##./#.##/.#.#
.##/.#./#.. => ##../.#.#/#.../.#..
###/.#./#.. => ..#./.#../..#./.###
.##/##./#.. => #.../#..#/..##/###.
###/##./#.. => ..../#..#/.#../####
#../..#/#.. => ..#./#.#./####/#...
.#./..#/#.. => .##./.###/#.../#.#.
##./..#/#.. => ##../.#.#/...#/#.##
#.#/..#/#.. => ####/###./##.#/...#
.##/..#/#.. => ##.#/###./#..#/###.
###/..#/#.. => .###/#..#/...#/.#.#
#../#.#/#.. => ##../##../#.../##.#
.#./#.#/#.. => #.../.###/...#/..#.
##./#.#/#.. => .#../..../#..#/..##
..#/#.#/#.. => ##../##.#/..#./#..#
#.#/#.#/#.. => .#../###./#.##/#.##
.##/#.#/#.. => ..../..#./#..#/####
###/#.#/#.. => ####/.#.#/...#/###.
#../.##/#.. => .#.#/#.##/##.#/.###
.#./.##/#.. => ##.#/#.#./.#.#/.##.
##./.##/#.. => .##./#.#./..../.#..
#.#/.##/#.. => ###./.#../#.../....
.##/.##/#.. => #.##/##../#.##/...#
###/.##/#.. => .##./..../...#/##..
#../###/#.. => #..#/#..#/#..#/####
.#./###/#.. => .#.#/#.#./.#.#/####
##./###/#.. => ##../#.#./#..#/....
..#/###/#.. => .##./##../..../###.
#.#/###/#.. => ..##/#.../#.../#.#.
.##/###/#.. => ..##/##.#/#.##/#.##
###/###/#.. => .#.#/..##/###./.#..
.#./#.#/.#. => ..../.#../.###/.#..
##./#.#/.#. => ...#/#.../.#.#/...#
#.#/#.#/.#. => ..../..##/..../.#..
###/#.#/.#. => #.#./#.##/##../###.
.#./###/.#. => #.##/..#./.#../###.
##./###/.#. => .#../..##/...#/#.#.
#.#/###/.#. => #.../...#/###./#...
###/###/.#. => ..##/##.#/..#./#.#.
#.#/..#/##. => .#.#/#.#./####/..#.
###/..#/##. => #..#/##.#/..../#...
.##/#.#/##. => #..#/...#/#.##/.#..
###/#.#/##. => .#.#/###./#.../#.##
#.#/.##/##. => .#../#.#./.#../..#.
###/.##/##. => ..#./##../##../.###
.##/###/##. => .###/#.##/##../.##.
###/###/##. => ..##/#.../.#.#/..##
#.#/.../#.# => .#../.#../##.#/.##.
###/.../#.# => .#.#/...#/.#../#.#.
###/#../#.# => ...#/#..#/..#./.###
#.#/.#./#.# => ##../##.#/####/...#
###/.#./#.# => .#.#/...#/..#./#..#
###/##./#.# => .###/##.#/.#../#.##
#.#/#.#/#.# => #.../#.../.#.#/...#
###/#.#/#.# => .#../#.#./##.#/..#.
#.#/###/#.# => .###/#..#/####/####
###/###/#.# => ####/#..#/.##./#...
###/#.#/### => #.#./..##/#.../#.#.
###/###/### => .###/.##./#.#./...#
"""