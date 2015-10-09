for each of the 9 possible directions,

first, if the direction takes us off the board, return false, recurse, next direction

second, detect if there is the opposite color in that space already
if NOT, return true (or otherwise bottom out the recursion)
if YES, recurse, same direction values and color data (us vs them) with new starting point

pick a direction
move in that direction
if it's on the board
  and it has opposite color
    move in that direction again
  but if it has the same color
    bottom out recursion with true
  and if it's empty (0) return false
otherwise return false
