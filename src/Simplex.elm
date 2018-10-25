{-
   https://github.com/josephg/noisejs/blob/master/perlin.js
   https://github.com/eskimoblood/elm-simplex-noise/blob/1.2.3/src/Noise.elm#

   Took necessary parts from these two repos
-}


module Simplex exposing (permutationTable, simplex2D)

import Array exposing (Array)
import Bitwise
import List exposing (range)
import Random exposing (Generator, Seed)
import Random.Array exposing (shuffle)
import Random.Extra


f2 =
    0.5 * (sqrt 3 - 1)


g2 =
    (3 - sqrt 3) / 6


{-| Generates a noise value between `-1` and `1` based on the given x and y value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
-}
simplex2D : ( PermutationTable, Random.Seed ) -> ( Float, Float ) -> Float
simplex2D ( { perm, permMod12 }, seed_ ) ( x, y ) =
    let
        skew =
            -- hairy factor for 2D
            (x + y) * f2

        ( i, j ) =
            ( floor (x + skew), floor (y + skew) )

        t =
            toFloat (i + j) * g2

        ( x0, y0 ) =
            -- The x,y distances from the cell origin, unskewed.
            ( x - (toFloat i - t), y - (toFloat j - t) )

        ( i1, j1 ) =
            cornerOffset2d ( x0, y0 )

        { x1, y1, x2, y2 } =
            { x1 = x0 - toFloat i1 + g2
            , y1 = y0 - toFloat j1 + g2
            , x2 = x0 - 1 + 2 * g2
            , y2 = y0 - 1 + 2 * g2
            }

        ( i2, j2 ) =
            -- Work out the hashed gradient indices of the three simplex corners
            ( Bitwise.and i 255, Bitwise.and j 255 )

        n0 =
            getN2d x0 y0 i2 j2 perm permMod12

        n1 =
            getN2d x1 y1 (i2 + i1) (j2 + j1) perm permMod12

        n2 =
            getN2d x2 y2 (i2 + 1) (j2 + 1) perm permMod12
    in
    70 * (n0 + n1 + n2)


{-| For the 2D case, the simplex shape is an equilateral triangle.
Determine which simplex we are in.
Offsets for second (middle) corner of simplex in (i,j) coords
A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
A step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
-}
cornerOffset2d : ( Float, Float ) -> ( Int, Int )
cornerOffset2d ( x, y ) =
    if x > y then
        -- lower triangle, XY order: (0, 0) -> (1, 0) -> (1, 1)
        ( 1, 0 )

    else
        -- upper triangle, YX order: (0, 0) -> (0, 1) -> (1, 1)
        ( 0, 1 )


getN2d : Float -> Float -> Int -> Int -> Array Int -> Array Int -> Float
getN2d x y i j perm permMod12 =
    let
        t =
            0.5 - x * x - y * y
    in
    if t < 0 then
        0

    else
        let
            gi =
                get permMod12 (i + get perm j) * 3

            t_ =
                t * t
        in
        t_ * t_ * (get grad3 gi * x + get grad3 (gi + 1) * y)


grad3 : Array Float
grad3 =
    Array.fromList
        [ 1, 1, 0, -1, 1, 0, 1, -1, 0, -1, -1, 0, 1, 0, 1, -1, 0, 1, 1, 0, -1, -1, 0, -1, 0, 1, 1, 0, -1, 1, 0, 1, -1, 0, -1, -1 ]


get : Array a -> Int -> a
get arr i =
    case Array.get i arr of
        Just x ->
            x

        Nothing ->
            Debug.todo "Error getting item"


{-| Permutation table that is needed to generate the noise value.
-}
type alias PermutationTable =
    { perm : Array Int, permMod12 : Array Int }


{-| Generate the permutation tables that are needed to calculate the noise value.
The function takes a seed and returns the table and a new seed.
-}
permutationTable : Random.Seed -> ( PermutationTable, Random.Seed )
permutationTable seed =
    let
        ( perm, seed_ ) =
            Random.step permGenerator seed
                |> (\( list, s ) -> ( Array.append list (reverseArray list), s ))
    in
    ( { perm = perm, permMod12 = generatePermMod12 perm }, seed_ )


permGenerator : Random.Generator (Array Int)
permGenerator =
    range 0 255
        |> Array.fromList
        |> shuffle


generatePermMod12 : Array Int -> Array Int
generatePermMod12 perm =
    Array.map (\i -> modBy 12 i) perm


{-| Create a generator that always produces the value provided. This is useful
when creating complicated chained generators and you need to handle a simple
case. It's also useful for the base case of recursive generators.
-}
constant : a -> Generator a
constant value =
    Random.map (\_ -> value) Random.Extra.bool


reverseArray : Array a -> Array a
reverseArray array =
    Array.toList array |> List.reverse |> Array.fromList
