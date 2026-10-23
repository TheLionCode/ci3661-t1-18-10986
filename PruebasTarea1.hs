-- Pruebas públicas de la Tarea 1 (CI-3661, Sep–Dic 2026).
--
-- Uso, en la carpeta donde está tu Tarea1.hs:
--     runghc PruebasTarea1.hs
--
-- Que pasen todas NO garantiza la nota: hay pruebas privadas, se revisa
-- que respetes las restricciones de cada parte y el estilo.
module Main (main) where

import Tarea1
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import System.Exit (exitFailure)

caso :: String -> Bool -> IO Bool
caso nombre prop = do
  r <- try (evaluate prop) :: IO (Either SomeException Bool)
  case r of
    Right True  -> return True
    Right False -> putStrLn ("  FALLA  " ++ nombre) >> return False
    Left e      -> putStrLn ("  ERROR  " ++ nombre ++ ": " ++ takeWhile (/= '\n') (show e))
                     >> return False

cerca :: Maybe Double -> Double -> Bool
cerca (Just x) y = abs (x - y) < 1e-9
cerca Nothing  _ = False

esLeft :: Either a b -> Bool
esLeft (Left _) = True
esLeft _        = False

pruebas :: [(String, Bool)]
pruebas =
  [ ("A1 frase con espacios",  esPalindromo "Anita lava la tina")
  , ("A1 signos y mayúsculas", esPalindromo "¿Acaso hubo búhos acá?" == False)
  , ("A1 dígitos",             esPalindromo "1 2 3 2 1")
  , ("A1 cadena vacía",        esPalindromo "")
  , ("A1 no palíndromo",       not (esPalindromo "Haskell"))
  , ("A2 vacía",               mayorYMenor ([] :: [Int]) == Nothing)
  , ("A2 un elemento",         mayorYMenor [7 :: Int] == Just (7, 7))
  , ("A2 varios",              mayorYMenor [3, 9, -2, 5 :: Int] == Just (9, -2))
  , ("A2 cadenas",             mayorYMenor ["mami", "homura", "sayaka"] == Just ("sayaka", "homura"))
  , ("A3 hora válida",         leerHora "07:45" == Right (7, 45))
  , ("A3 medianoche",          leerHora "00:00" == Right (0, 0))
  , ("A3 formato",             leerHora "7:45" == Left "formato inválido")
  , ("A3 hora",                leerHora "24:10" == Left "hora fuera de rango")
  , ("A3 minutos",             leerHora "13:60" == Left "minutos fuera de rango")
  , ("A3 duración simple",     duracion "08:30" "10:05" == Right 95)
  , ("A3 cruza medianoche",    duracion "23:50" "00:20" == Right 30)
  , ("A3 duración con error",  esLeft (duracion "08:30" "1005"))
  , ("B1 promedio",            cerca (promedioSobreUmbral 2 [1, 3, 5, 2]) 4)
  , ("B1 ninguno",             promedioSobreUmbral 10 [1, 2, 3] == Nothing)
  , ("B2 esquina",             movimientosCaballo (1, 1) == [(2, 3), (3, 2)])
  , ("B2 centro",              length (movimientosCaballo (4, 4)) == 8)
  , ("B2 dos saltos esquina",  enDosSaltos (1, 1) == 10)
  , ("B3 miMap",               miMap (* 2) [1, 2, 3 :: Int] == [2, 4, 6])
  , ("B3 miFilter",            miFilter even [1 .. 10 :: Int] == [2, 4, 6, 8, 10])
  , ("B3 miFoldl",             miFoldl (-) 10 [1, 2, 3 :: Int] == 4)
  , ("B3 miFoldl orden",       miFoldl (flip (:)) [] "abc" == "cba")
  ]

main :: IO ()
main = do
  rs <- forM pruebas (uncurry caso)
  let ok = length (filter id rs)
  putStrLn (show ok ++ " de " ++ show (length rs) ++ " pruebas públicas pasan.")
  if ok == length rs then return () else exitFailure
