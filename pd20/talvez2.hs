-- data Maybe a = Nothing | Just a

cabeza :: [a] -> Maybe a
cabeza [] = Nothing
cabeza (x:_) = Just x

ultimo :: [a] -> Maybe a
ultimo [] = Nothing
ultimo [x] = Just x
ultimo (_:xs) = ultimo xs

cola :: [a] -> Maybe [a]
cola [] = Nothing
cola (_:xs) = Just xs

depurar :: [Maybe a] -> [a]
depurar [] = []
depurar (Nothing:xs) = depurar xs
depurar (Just x:xs) = x:depurar xs

principio :: [a] -> Maybe [a]
principio [] = Nothing
principio xs = go [] xs
 where go l [x] = Just l
       go l (x:xs) = go (l++[x]) xs
