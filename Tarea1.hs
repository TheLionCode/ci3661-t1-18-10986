-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Gabriel Carballo
--  Carnet: 18-10986
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Use Gemini en el navegador y el Modo IA de la busqueda de Google (que tambien es Gemini)
--  para entender como se escribe Haskell y para salir de errores de tipado, logicos y de compilado.
--  Tambien le pedi escribir la traza.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo s = limpia s == voltear (limpia s)
  where
    limpia [] = []
    limpia (c:cs)
      | isAlphaNum c = toLower c : limpia cs
      | otherwise    = limpia cs

    voltear [] = []
    voltear (x:xs) = voltear xs ++ [x]

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Tomo el primer elemento como mayor y como menor.
-- En cada llamada miro solo el siguiente y si hace falta cambio el mayor o el menor.
-- No recorro otra vez la lista para calcular el mínimo aparte.
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (recorrer x x xs)
  where
    recorrer mayor menor [] = (mayor, menor)
    recorrer mayor menor (y:ys)
      | y > mayor = recorrer y menor ys
      | y < menor = recorrer mayor y ys
      | otherwise = recorrer mayor menor ys

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | not (isDigit a && isDigit b && isDigit c && isDigit d) = Left "formato inválido"
  | hora > 23 = Left "hora fuera de rango"
  | minutos > 59 = Left "minutos fuera de rango"
  | otherwise = Right (hora, minutos)
  where
    hora = digitToInt a * 10 + digitToInt b
    minutos = digitToInt c * 10 + digitToInt d
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion h1 h2 = case leerHora h1 of
  Left err -> Left err
  Right (hora1, min1) -> case leerHora h2 of
    Left err2 -> Left err2
    Right (hora2, min2) ->
      let total1 = hora1 * 60 + min1
          total2 = hora2 * 60 + min2
          diff = total2 - total1
      in if diff < 0 then Right (diff + 24 * 60) else Right diff

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs =
  let (suma, cantidad) = foldr paso (0, 0 :: Int) xs
      paso x (s, n) =
        if x > umbral then (s + x, n + 1) else (s, n)
  in if cantidad == 0 then Nothing else Just (suma / fromIntegral cantidad)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (fila, col) =
  [ (fila + df, col + dc)
  | (df, dc) <- [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
  , fila + df >= 1 && fila + df <= 8
  , col + dc >= 1 && col + dc <= 8
  ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos casilla = length (sinRepetidos dosSaltos)
  where
    unSalto = movimientosCaballo casilla
    dosSaltos = foldr (++) [] (map movimientosCaballo unSalto)
    sinRepetidos xs = foldr (\x acc -> if pertenece x acc then acc else x : acc) [] xs
    pertenece x ys = foldr (\y acc -> y == x || acc) False ys

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f lista = foldr (\x acc -> f x : acc) [] lista

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p lista = foldr (\x acc -> if p x then x : acc else acc) [] lista

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = (foldr paso (\acc -> acc) xs) z
  where
    paso x continuar acc = continuar (f acc x)
