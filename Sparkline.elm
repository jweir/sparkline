module Sparkline exposing (Size, DataSet, Point, Param(..), sparkline)

{-| This library is for generating inline graphs, called sparklines.

# Definition
@docs sparkline, Param

# Data types
@docs Point, DataSet, Size

-}

import Svg as Svg
    exposing
        ( Svg
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


{-| Drawing a sparkline occurs by passing params

          -- data is tuples with (x,y) values as floats
          data = [(0,0),(5,10),(10,12)]
          data2 = [(0,-5),(7,2),(12,14)]

          sparkline
              (100,10,5,5)    -- the width, height, top/bottom margin, left/right margin
              [ Line data ]   -- type of graph to draw with the data

Multiple params can be included to have muliple graphs. Each will have its data
scaled relatively to one another. The graphs are drawn in the order they are
given. So last graph will be drawn on top.

          sparkline
              (100,10,5,5)    -- the width, height, top/bottom margin, left/right margin
              [ Line data
              , Line data2
              ]

The three types of graphs are
* **Line** creates a line graph
* **Dot** draws a dot at each point.  Set the radius of the dot by styline it `|> Style [Svg.r "3"]`
* **Bar** <BarWidth> Draws a bar graph.  This requires defining what the width of the bar.

There are also some options which can be applied to each graph:
* **Independent** will scale this graph's dataset separately from the rest of the graphs.
* **Style** allows apply `Svg.Attribute` styles to the rendered svg.

Finally there is the **ZeroLine** param which will draw a line at the 0 y axis
for the data. *This does not apply to any graphs rendered with
**Independent.***

**Examples*

See Example.elm for more examples.

        -- style a ZeroLine to be very light
        sparkline
            ( 200, 5, 5, 5 )
            [ ZeroLine |> Style [ Svg.strokeWidth "0.5", Svg.stroke "rgba(0,0,0,0.3)" ]
            , Bar 20 [ ( 0, 2 ) , ( 10, 30 ) , ( 20, 10 ) ]
            ]

        -- graph the datasets to not be relative to one another.  The params can be piped.
        sparkline
            ( 200, 10, 5, 5 )
            [ Line data |> Independent
            , Line data2 |> Independent
            ]




-}
type Param a
    = Bar Float DataSet
    | Dot DataSet
    | Line DataSet
      -- options
    | ZeroLine
    | Independent (Param a)
    | Style (List (Svg.Attribute a)) (Param a)


{-| Defines the size of the graph (width, height, topBottomMargin, leftRightMargin)
-}
type alias Size =
    ( Float, Float, Float, Float )


{-| Tuple of (x,y) value
-}
type alias Point =
    ( Float, Float )


{-| The data to be rendered.
-}
type alias DataSet =
    List Point


type alias Range =
    ( Float -> Float, Float -> Float )


type alias Domain =
    ( Point, Point )


type alias Method a =
    DataSet
    -> List (Svg.Attribute a)
    -> Domain
    -> Range
    -> List (Svg a)



-- | Style (AttributeSet a)
-- | Absolute List (Param a)
-- by default data should be relative and in the same domain


{-| The entry point to create a graph. See Param.
-}
sparkline : Size -> List (Param a) -> Svg a
sparkline size params =
    let
        tokens : List ( Method a, DataSet, List (Svg.Attribute a), IndySet )
        tokens =
            List.map (tokenizer) params

        domain_ : Domain
        domain_ =
            tokens
                |> List.filter (\( _, _, _, indy ) -> indy /= True)
                |> List.concatMap (\( _, data, _, _ ) -> [ data ])
                |> domain

        -- calc the range in the method, bar needs the size before calcing the range
        range_ : Range
        range_ =
            range size domain_

        collector : ( Method a, DataSet, List (Svg.Attribute a), IndySet ) -> List (Svg a)
        collector ( method, data, attr, indy ) =
            let
                ( cdom, crange ) =
                    if indy == True then
                        ( domain [ data ]
                        , (range size (domain [ data ]))
                        )
                    else
                        ( domain_, range_ )
            in
                method
                    data
                    attr
                    cdom
                    crange
    in
        tokens
            |> List.concatMap collector
            |> frame size


type alias IndySet =
    Bool


tokenizer :
    Param a
    -> ( Method a, DataSet, List (Svg.Attribute a), IndySet )
tokenizer msg =
    case msg of
        Bar width data ->
            ( bar width, data, [], False )

        Dot data ->
            ( dot, data, [], False )

        Line data ->
            ( line, data, [], False )

        ZeroLine ->
            ( zeroLine, [], [], False )

        Independent msg_ ->
            let
                ( m, d, a, _ ) =
                    tokenizer msg_
            in
                ( m, d, a, True )

        Style attr msg_ ->
            let
                ( m, d, _, i ) =
                    tokenizer msg_
            in
                ( m, d, attr, i )


frame : Size -> List (Svg a) -> Svg a
frame ( w, h, ms, mt ) items =
    Svg.svg
        [ A.width := (w + 2 * ms)
        , A.height := (h + 2 * mt)
        , A.viewBox *= [ 0, 0, w + 2 * ms, h + 2 * mt ]
        ]
        [ Svg.g
            [ A.transform ("translate" ++ toString ( ms, mt ))
            ]
            items
        ]


zeroLine : Method a
zeroLine _ attr domain range =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            domain
    in
        line
            [ ( x1, 0 ), ( x2, 0 ) ]
            attr
            domain
            range


line : Method a
line data attr domain range =
    [ Svg.path
        ([ fill "none"
         , stroke "#000"
         , strokeWidth := 1
         , d (path range data)
         ]
            ++ attr
        )
        []
    ]


dot : Method a
dot data attr _ range =
    data
        |> scale range
        |> List.map
            (\( x, y ) ->
                circle
                    ([ cx := x
                     , cy := y
                     , r := 2
                     ]
                        ++ attr
                    )
                    []
            )


bar : Float -> Method a
bar w data attr ( ( x0, y0 ), ( x1, y1 ) ) ( mx, my ) =
    data
        |> List.map
            (\( x, y ) ->
                let
                    -- this positions the bar correctly within the frame based on the width of the bar
                    -- same should happen for dots
                    p =
                        w * (x / (x1 - x0))

                    ( y_, h ) =
                        if y < 0 then
                            ( my y - (my y - my 0), my y - my 0 )
                        else
                            ( my y, my 0 - my y )
                in
                    rect
                        ([ A.x := ((mx x) - p)
                         , A.y := y_
                         , width := w
                         , height := h
                         ]
                            ++ attr
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



-- FIXME BUG when domain min and max are equal


domain : List DataSet -> Domain
domain dataset =
    let
        seed =
            dataset
                |> List.concatMap (\s -> s)
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
        dataset
            |> List.concatMap (\s -> s)
            |> List.foldr
                (\( x, y ) ( ( xlo, ylo ), ( xhi, yhi ) ) ->
                    ( ( Basics.min xlo x, Basics.min ylo y )
                    , ( Basics.max xhi x, Basics.max yhi y )
                    )
                )
                ( seed, seed )
            |> ensure


{-| esures the domain along y is not identical
-}
ensure : Domain -> Domain
ensure ( ( x0, y0 ), ( x1, y1 ) ) =
    if y0 == y1 then
        ( ( x0, min 0 y0 ), ( x1, y1 ) )
    else
        ( ( x0, y0 ), ( x1, y1 ) )


range : Size -> Domain -> Range
range ( w, h, _, _ ) ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        (x - x0) * (w / (x1 - x0))
    , \y ->
        (y1 - y) * (h / (y1 - y0))
    )


scale : Range -> DataSet -> DataSet
scale range data =
    let
        ( mx, my ) =
            range
    in
        data
            |> List.map (\( x, y ) -> ( mx x, my y ))


(:=) : (String -> a) -> Float -> a
(:=) fun n =
    fun (toString n)


(*=) : (String -> a) -> List Float -> a
(*=) fun n =
    List.map toString n
        |> String.join " "
        |> fun
