module StrawPoll.ListHelpers where

dropTrailing :: (Eq a) => a -> [a] -> [a]
dropTrailing item = reverse . (dropWhile (== item)) . reverse

-- Add an empty item to the end of the list if we're editing the last item
-- This is slow since it repeatedly calls length.
padToMinLength :: Int -> a -> [a] -> [a]
padToMinLength l val arr = if length arr < l
                           then padToMinLength l val (arr ++ [val])
                           else arr

