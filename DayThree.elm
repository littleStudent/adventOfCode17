module DayThree exposing (..)

import String exposing (toList, toInt)
import List.Extra exposing (..)
import Dict


input : Int
input =
    100


target : Int
target =
    347991



-- input = 347991


type Direction
    = Right
    | Left
    | Top
    | Bottom


type alias Row =
    { bottomRightX : Int
    , topRightY : Int
    , topLeftX : Int
    , bottomLeftY : Int
    , direction : Direction
    , current : { positionX : Int, positionY : Int }
    , values : Dict.Dict Position Int
    }


type alias Position =
    ( Int, Int )


calculateValueForPosition : Position -> Dict.Dict Position Int -> Int
calculateValueForPosition ( positionX, positionY ) values =
    Maybe.withDefault 0 (Dict.get ( positionX - 1, positionY ) values)
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX - 1, positionY - 1 ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX, positionY - 1 ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX + 1, positionY - 1 ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX + 1, positionY ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX + 1, positionY + 1 ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX, positionY + 1 ) values)))
        |> (\next -> next + (Maybe.withDefault 0 (Dict.get ( positionX - 1, positionY + 1 ) values)))
        |> (\next ->
                if next == 0 then
                    1
                else
                    next
           )


output : String
output =
    let
        range =
            List.range 1 (input)

        result =
            List.foldl
                (\element result ->
                    (case result.direction of
                        Right ->
                            let
                                newX =
                                    result.current.positionX + 1
                            in
                                { result
                                    | current =
                                        { positionX = newX
                                        , positionY = result.current.positionY
                                        }
                                    , bottomRightX =
                                        if newX > result.bottomRightX then
                                            result.bottomRightX + 1
                                        else
                                            result.bottomRightX
                                    , direction =
                                        if newX > result.bottomRightX then
                                            Top
                                        else
                                            Right
                                }

                        Top ->
                            let
                                newY =
                                    result.current.positionY + 1
                            in
                                { result
                                    | current =
                                        { positionX = result.current.positionX
                                        , positionY = newY
                                        }
                                    , topRightY =
                                        if newY > result.topRightY then
                                            result.topRightY + 1
                                        else
                                            result.topRightY
                                    , direction =
                                        if newY > result.topRightY then
                                            Left
                                        else
                                            Top
                                }

                        Left ->
                            let
                                newX =
                                    result.current.positionX - 1
                            in
                                { result
                                    | current =
                                        { positionX = newX
                                        , positionY = result.current.positionY
                                        }
                                    , topLeftX =
                                        if newX < result.topLeftX then
                                            result.topLeftX - 1
                                        else
                                            result.topLeftX
                                    , direction =
                                        if newX < result.topLeftX then
                                            Bottom
                                        else
                                            Left
                                }

                        Bottom ->
                            let
                                newY =
                                    result.current.positionY - 1
                            in
                                { result
                                    | current =
                                        { positionX = result.current.positionX
                                        , positionY = newY
                                        }
                                    , bottomLeftY =
                                        if newY < result.bottomLeftY then
                                            result.bottomLeftY - 1
                                        else
                                            result.bottomLeftY
                                    , direction =
                                        if newY < result.bottomLeftY then
                                            Right
                                        else
                                            Bottom
                                }
                    )
                        |> (\result ->
                                { result
                                    | values =
                                        (Dict.insert ( result.current.positionX, result.current.positionY )
                                            (calculateValueForPosition
                                                ( result.current.positionX, result.current.positionY )
                                                result.values
                                            )
                                            result.values
                                        )
                                }
                           )
                )
                (Row 0 0 0 0 Right { positionX = -1, positionY = 0 } Dict.empty)
                range
    in
        toString
            (List.head
                (List.sort
                    (List.filter (\item -> item > target)
                        (List.map (\( a, b ) -> b) (Dict.toList result.values))
                    )
                )
            )
