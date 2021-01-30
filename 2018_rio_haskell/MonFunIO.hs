module MonoFunIO where

import Data.Monoid ( (<>) )

{- Sobre la instancia Monoid IO:

  Buscar en: https://www.haskell.org/hoogle/  

  Recordar que:
  > :t readFile
  readFile :: FilePath -> IO String

  class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

  Y que existe la instancia siguiente (no hace falta buscar su implementación):
  instance Monoid a => Monoid (IO a)

  -}

-- Definir la funciones siguientes usando readFile, <> y mconcat

-- | Lee dos archivos y devuelve sus contenidos concatenados.
read2Files :: FilePath -> FilePath -> IO String
read2Files = undefined

-- | Lee todos los archivos de la lista dada, y devuelve sus contenidos concatenados.
readFiles :: [FilePath] -> IO String
readFiles = undefined

-- Ahora necesitamos <$> para definir las funciones siguientes *en una sola línea*
-- Usar las funciones <$> , lines
readAsLines :: FilePath -> IO [String]
readAsLines = undefined

-- devuelve la cantidad de líneas (no usar readAsLines, usar dos <$> si necesario)
countLines :: FilePath -> IO Int
countLines = undefined
