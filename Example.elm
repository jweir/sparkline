module Example exposing (..)

import Svg.Attributes as Svg
import Sparkline exposing (dot, line, bar, Point)
import Html as Html
import Html.Attributes as HA


main =
    Html.div []
        [ line ( 100, 15, 5, 5 ) [ ( data, [ Svg.stroke "#F00" ] ), ( data2, [ Svg.stroke "#000" ] ) ]
        , dot ( 100, 15, 5, 5 ) [ ( data, [ Svg.r "1" ] ), ( data2, [ Svg.r "2", Svg.fill "#F00" ] ) ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "110px" ) ] ]
            [ bar ( 100, 15, 5, 15 ) [ ( datax, [] ) ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "110px" ) ] ]
            [ bar ( 100, 15, 5, 15 ) [ ( data2, [] ) ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ bar ( 200, 10, 0, 0 ) [ ( data3, [] ) ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ line ( 200, 10, 0, 5 ) [ ( data3, [] ) ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ dot ( 200, 10, 5, 5 ) [ ( data3, [] ) ]
            ]
        ]


data : List Point
data =
    [ ( -2, 10 )
    , ( 1, 10 )
    , ( 2, 11 )
    , ( 3, 5 )
    , ( 4, 8 )
    , ( 5, 20 )
    , ( 6, 15 )
    , ( 7, -5 )
    , ( 7, -10 )
    , ( 10, 44 )
    , ( 12, 10 )
    , ( 13, 10 )
    , ( 16, 10 )
    ]


data2 : List Point
data2 =
    [ ( 0, 1 )
    , ( 1, 2 )
    , ( 2, 3 )
    , ( 3, 4 )
    , ( 4, 5 )
    ]


datax : List Point
datax =
    [ ( 0, -10 )
    , ( 0.5, -8 )
    , ( 1, -5 )
    , ( 1.5, -2 )
    , ( 2, 0 )
    , ( 3, 5 )
    , ( 3.5, 10 )
    , ( 4, 10 )
    ]


data3 : List Point
data3 =
    [ ( 0, 1 )
    , ( 1, 1 )
    , ( 2, -1 )
    , ( 3, 1 )
    , ( 4, 1 )
    , ( 5, -1 )
    , ( 6, -1 )
    , ( 7, 1 )
    , ( 8, -1 )
    , ( 9, -1 )
    , ( 10, -1 )
    , ( 11, 1 )
    , ( 12, 1 )
    , ( 13, -1 )
    , ( 14, 1 )
    , ( 15, -1 )
    , ( 16, -1 )
    , ( 17, 1 )
    , ( 18, -1 )
    , ( 19, -1 )
    , ( 20, 1 )
    , ( 21, 1 )
    ]
