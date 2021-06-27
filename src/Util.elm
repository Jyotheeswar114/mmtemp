module Util exposing (..)
import Array
import Html exposing (p)
import Html.Attributes exposing (class)
import Html exposing (text)
import Html exposing (div)

get_array1 : Array.Array Int
get_array1 =
    Array.fromList [8, 4 , 3, 7]

get_array2 : Array.Array Int
get_array2 = 
    Array.fromList [4, 5, 2 , 6]



dot_product : Array.Array Int -> Array.Array Int -> Int
dot_product array1 array2 =
    array_sum (get_products array1 array2)
get_products : Array.Array Int -> Array.Array Int -> Array.Array Int
get_products array1 array2 = 
    Array.indexedMap (\i x -> x * (Maybe.withDefault 0 (Array.get i array2))) array1

array_sum : Array.Array Int -> Int
array_sum array =
    Array.foldl (+) 0 array

single_multiply : Array.Array Int -> Int -> Array.Array Int -> Int -> Int
single_multiply array1 s1 array2 s2 = 
    (Maybe.withDefault 0 (Array.get s1 array1)) * (Maybe.withDefault 0 (Array.get s2 array2))
