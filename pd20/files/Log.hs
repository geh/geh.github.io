module Log where

import Control.Applicative ( (<$>)     )
import Text.Read           ( readMaybe )

-- | Los distintos tipos de mensajes
data MessageType = Info
                 | Warning
                 | Error Int    -- ^ Este parametro indica la gravedad del error
  deriving (Show, Eq)
   -- esta linea permite comparar valores de este tipo e imprimirlos en GHCi

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
  deriving (Show, Eq)

data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM String
  deriving (Show, Eq)

data MaybeInt = ValidInt Int
              | InvalidInt
  deriving (Show, Eq)

-- Las funciones siguientes son utiles para completar este practico,
-- pero no hace falta que entiendas su funcionamiento por ahora.

-- | Converte una @String@ a un @Int@, con posibilidad de falla.
readInt :: String -> MaybeInt
readInt s
  | Just i <- readMaybe s = ValidInt i
  | otherwise             = InvalidInt

-- | @testParse p n f@ chequea el analizador de archivo de log @p@
--   ejecutandolo sobre la primeras @n@ lineas del archivo @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO ()
testParse parse n file = do
  messages <- take n . parse <$> readFile file
  mapM_ (putStrLn . show) messages

-- | @testWhatWentWrong p w f@ chequea el analizador de archivo de log @p@
--   y el extractor de mensaje de advertencia @w@ ejecutandolos sobre el
--   archivo de log @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO ()
testWhatWentWrong parse whatWentWrong file = do
  strings <- whatWentWrong . parse <$> readFile file
  mapM_ putStrLn strings
