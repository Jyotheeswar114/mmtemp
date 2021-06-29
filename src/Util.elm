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

get_m1 : Array.Array (Array.Array Int)
get_m1 = Array.fromList [
    Array.fromList [3, 0, 9, 4],
    Array.fromList [6, 5, 3, 4],
    Array.fromList [3, 9, 4, 6],
    Array.fromList [0, 8, 2, 4]]

get_m2 : Array.Array (Array.Array Int)
get_m2 = Array.fromList [
    Array.fromList [8, 6, 7, 0],
    Array.fromList [1, 2, 5, 1],
    Array.fromList [3, 9, 5, 7],
    Array.fromList [0, 7, 2, 5]]

multiply_matrices : Array.Array (Array.Array Int) -> Array.Array (Array.Array Int) -> Array.Array (Array.Array Int)
multiply_matrices a1 a2 =
    let
      temp_ = Array.fromList [0, 1, 2, 3]  
    in
    Array.map (\x -> Array.map (\y -> dot_product (Maybe.withDefault (Array.fromList [1,1,1,1]) (Array.get x a1)) (Maybe.withDefault (Array.fromList [1,1,1,1]) (Array.get y a2))) temp_ ) temp_

get_2d_zeroes : Int -> Int -> Array.Array (Array.Array Int)
get_2d_zeroes x y =
    Array.repeat x (Array.repeat y 0)

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

row_column_prods : Array.Array (Array.Array Int) -> Int -> Array.Array (Array.Array Int) -> Int -> Array.Array Int
row_column_prods m1 s1 m2 s2 = 
    get_products (Maybe.withDefault (Array.fromList [0, 0, 0, 0]) (Array.get s1 m1)) (Maybe.withDefault (Array.fromList [0, 0, 0, 0]) (Array.get s2 m2))

matrix_value_set : Array.Array (Array.Array Int) -> Int -> Int -> Int -> Array.Array (Array.Array Int)
matrix_value_set matrix value p1 p2 =
    Array.set p1 (Array.set p2 value (Maybe.withDefault (Array.repeat 4 0) (Array.get p1 matrix))) matrix

get_matrix_element : Int -> Int -> Array.Array (Array.Array Int) -> Int
get_matrix_element x y matrix =
    Maybe.withDefault 0 (Array.get y (Maybe.withDefault (Array.repeat 4 0) (Array.get x matrix)))