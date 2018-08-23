module Kyu2.Postfix where

begin action = action []
push acc n action = action (n:acc)
add (x1:x2:xs) next = next $ x1 + x2 : xs
end = head
