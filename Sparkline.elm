module Sparkline exposing (Point, line, dot, bar)

import Svg as Svg
    exposing
        ( Svg
        , Attribute
        , circle
        , rect
        )
import Svg.Attributes as A
    exposing
        ( fill
        , stroke
        , strokeWidth
        , d
        , cx
        , cy
        , r
        , x
        , y
        , width
        , height
        )


type alias Size =
    ( Float, Float, Float, Float )


type alias Point =
    ( Float, Float )


type alias DataSet =
    List ( Float, Float )


type alias DataStyleSet a =
    ( DataSet, AttributeSet a )


type alias AttributeSet a =
    List (Attribute a)


type alias Range =
    ( Float -> Float, Float -> Float )


type alias Domain =
    ( Point, Point )


type alias Interface a =
    Size -> List (DataStyleSet a) -> Svg a


(:=) : (String -> a) -> Float -> a
(:=) fun n =
    fun (toString n)


(*=) : (String -> a) -> List Float -> a
(*=) fun n =
    List.map toString n
        |> String.join " "
        |> fun


frame : Size -> List (Svg a) -> Svg a
frame ( w, h, ms, mt ) items =
    Svg.svg
        [ A.width := (w + 2 * ms), A.height := (h + 2 * mt), A.viewBox *= [ 0, 0, w + 2 * ms, h + 2 * mt ] ]
        [ Svg.g [ A.transform ("translate" ++ toString ( ms, mt )) ] items
        ]


line : Interface a
line size data =
    List.map (drawLine (range size (domain data))) data
        |> frame size


drawLine : Range -> DataStyleSet a -> Svg a
drawLine range ( data, attributes ) =
    Svg.path
        ([ fill "none"
         , stroke "#000"
         , strokeWidth := 1
         , d (path range data)
         ]
            ++ attributes
        )
        []


dot : Interface a
dot size data =
    List.concatMap (drawDot (range size (domain data))) data
        |> frame size


drawDot : Range -> DataStyleSet a -> List (Svg a)
drawDot range ( data, attributes ) =
    data
        |> scale range
        |> List.map
            (\( x, y ) ->
                circle ([ cx := x, cy := y, r := 2 ] ++ attributes) []
            )


bar : Interface a
bar size data =
    let
        d =
            domain data
    in
        (List.concatMap
            (drawBar d (range size d))
            data
        )
            |> frame size


drawBar : Domain -> Range -> DataStyleSet a -> List (Svg a)
drawBar ( ( xl, yl ), ( xh, yh ) ) ( mx, my ) ( data, attributes ) =
    data
        |> List.map
            (\( x, y ) ->
                let
                    w =
                        2

                    ( y_, h ) =
                        if y < 0 then
                            ( my y - (my y - my 0), my y - my 0 )
                        else
                            ( my y, my 0 - my y )
                in
                    rect
                        ([ A.x := (mx x)
                         , A.y := y_
                         , width := w
                         , height := h
                         ]
                            ++ attributes
                        )
                        []
            )


collect : Point -> String -> String
collect ( x, y ) path =
    let
        command =
            if path == "" then
                "M"
            else
                "L"
    in
        path
            ++ command
            ++ (toString x)
            ++ " "
            ++ (toString y)


path : Range -> DataSet -> String
path range data =
    data
        |> scale range
        |> List.foldr (collect) ""


domain : List (DataStyleSet a) -> Domain
domain dataset =
    dataset
        |> List.concatMap (\( set, _ ) -> set)
        |> List.foldr
            (\( x, y ) ( ( xlo, ylo ), ( xhi, yhi ) ) ->
                ( ( Basics.min xlo x, Basics.min ylo y )
                , ( Basics.max xhi x, Basics.max yhi y )
                )
            )
            ( ( 0, 0 ), ( 0, 0 ) )


range : Size -> Domain -> Range
range ( w, h, _, _ ) ( ( xl, yl ), ( xh, yh ) ) =
    ( (\x ->
        (x - xl) * (w / (xh - xl))
      )
    , (\y ->
        (yh - y) * (h / (yh - yl))
      )
    )


scale : Range -> DataSet -> DataSet
scale range data =
    let
        ( mx, my ) =
            range
    in
        data
            |> List.map (\( x, y ) -> ( mx x, my y ))
