module Example exposing (..)

import Svg.Attributes as Svg
import Sparkline exposing (Point, sparkline, Param(..))
import Sparkline.Extras exposing (extent, Axes(..))
import Html as Html
import Html.Attributes as HA


main =
    Html.div []
        [ Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "110px" ) ] ]
            [ sparkline ( 100, 15, 5, 15 ) [ Bar 2 datax ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 20, 25, 5 )
                [ Dot data
                , Label
                    (List.map (\( x, y ) -> ( ( x, y ), [], toString y )) (data |> extent Y))
                    |> Style [ Svg.fill "black", Svg.fontFamily "arial", Svg.fontSize "10px", Svg.dx "2", Svg.dy "2" ]
                ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "110px" ) ] ]
            [ sparkline ( 100, 15, 5, 15 ) [ Bar 2 data2 ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ sparkline ( 100, 15, 5, 15 ) [ Bar 2 data3 ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ sparkline ( 100, 15, 5, 15 ) [ Line data3 ]
            ]
        , Html.div
            [ HA.style [ ( "margin", "2px" ), ( "background", "#EEE" ), ( "width", "auto" ) ] ]
            [ sparkline ( 100, 5, 5, 15 ) [ Dot data3 ] ]
        , Html.div []
            [ Html.h3 [] [ Html.text "filled" ]
            , sparkline ( 200, 10, 5, 5 )
                [ Area data |> Style [ Svg.fill "#CCC", Svg.stroke "none" ]
                , Dot datax |> Style [ Svg.fill "#F00" ]
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 10, 5, 5 )
                [ Independent (Bar 2 data3)
                , Independent (Style [ Svg.fill "#F00" ] (Dot datax))
                , Independent (Line data)
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 10, 5, 5 )
                [ Line [ ( 0, 10 ), ( 10, 10 ), ( 20, 30 ) ]
                , Line [ ( 0, 2 ), ( 10, 4 ), ( 20, 20 ), ( 30, 40 ) ]
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 10, 5, 5 )
                [ Independent (Line [ ( 0, 10 ), ( 10, 10 ), ( 20, 30 ) ])
                , Independent (Line [ ( 0, 2 ), ( 10, 4 ), ( 20, 20 ), ( 30, 40 ) ])
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 10, 5, 5 )
                [ Style [ Svg.stroke "#F00" ] ZeroLine
                , Line data
                , Style [ Svg.r "1" ] (Dot [ ( 8, -40 ), ( 8, 0 ), ( 8, 40 ) ])
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 20, 5, 5 )
                [ ZeroLine |> Style [ Svg.stroke "#CCC" ]
                , Dot [ ( 0, 0 ), ( 4, 10 ) ]
                , Bar 2 [ ( 8, 10 ), ( 10, 4 ), ( 12, -5 ) ]
                , Line [ ( 12, -5 ), ( 14, -2 ), ( 20, 5 ), ( 22, 30 ), ( 22, 40 ), ( 24, 30 ) ] |> Independent
                ]
            ]
        , Html.div []
            [ sparkline
                ( 200, 5, 5, 5 )
                [ Bar 20
                    [ ( 0, 2 )
                    , ( 10, 30 )
                    , ( 20, 10 )
                    ]
                , ZeroLine |> Style [ Svg.strokeWidth "0.2", Svg.stroke "rgba(0,0,0,0.3)" ]
                ]
            ]
        ]



-- Independent should use the same zero point as the Relative data


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
    , ( 7, -30 )
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
