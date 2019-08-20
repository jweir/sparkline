module Sparkline exposing
    ( sparkline, Param(..)
    , Point, DataSet, LabelSet, Size
    , Event(..), Selection, subscriptions
    , Layer(..), createSelection, visualize
    )

{-| This library is for generating inline graphs, called sparklines.


# Definition

@docs sparkline, Param


# Data types

@docs Point, DataSet, LabelSet, Size


# Event types

@docs Event, Selection, subscriptions

-}

import Array
import Browser.Events as Browser
import Json.Decode as Json
import Svg as Svg
    exposing
        ( Svg
        , circle
        , rect
        )
import Svg.Attributes as A
    exposing
        ( cx
        , cy
        , d
        , fill
        , height
        , r
        , stroke
        , strokeWidth
        , width
        , x
        , y
        )
import Svg.Events as E


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

  - **Line** creates a line graph
  - **Area** creates a graph meant to be filled
  - **Dot** draws a dot at each point. Set the radius of the dot by styling it `|> Style [Svg.r "3"]`
  - **Bar** <BarWidth> Draws a bar graph. This requires defining what the width of the bar.
  - **Label** plots text on the graph

There are also some options which can be applied to each graph:

  - **Domain** includes the given data into the graph's domain.
    This is useful when creating multiple graphs that need to have the same scale.
  - **Style** allows applying `Svg.Attribute` styles to the rendered svg.

Finally there is the **ZeroLine** param which will draw a line at the 0 y axis
for the data.

\*\*Examples\*

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
            [ Line data
            , Line data2
            ]

-}
type Param a
    = Bar Float DataSet
    | Dot DataSet
    | Line DataSet
    | Area DataSet
    | Domain DataSet
    | Label (LabelSet a)
      -- options
    | Highlight Selection
    | ZeroLine
    | Style (List (Svg.Attribute a)) (Param a)


{-| Defines the size of the graph (width, height, leftRightMargin, topBottomMargin)
-}
type alias Size =
    { width : Float
    , height : Float
    }


type alias Text =
    String


{-| Tuple of (x,y) value
-}
type alias Point =
    ( Float, Float )


{-| The data to be rendered.
-}
type alias DataSet =
    List Point


{-| The data and text to use for labeling
-}
type alias LabelSet a =
    List ( Point, List (Svg.Attribute a), Text )


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


type Layer a
    = Layer Size ( Float, Float ) (List (Event a)) (List (Param a))



-- | Style (AttributeSet a)
-- | Absolute List (Param a)
-- by default data should be relative and in the same domain


visualize : ( Float, Float ) -> List (Layer a) -> Svg a
visualize ( w, h ) layers =
    layers
        |> List.map (\(Layer size ( x, y ) events params) -> foo size ( x, y ) events params)
        |> frame (Size w h)


{-| The entry point to create a graph. See Param.
-}
sparkline : Size -> List (Event a) -> List (Param a) -> Svg a
sparkline size events params =
    frame size [ foo size ( 0, 0 ) events params ]


{-| The entry point to create a graph. See Param.
-}
foo : Size -> ( Float, Float ) -> List (Event a) -> List (Param a) -> Svg a
foo size offset events params =
    let
        commands : List (CommandSet a)
        commands =
            List.map toCommand params

        domain_ : Domain
        domain_ =
            commands
                |> List.concatMap (\token -> [ token.data ])
                |> domain

        -- calc the range in the method, bar needs the size before calcing the range
        range_ : Range
        range_ =
            range size domain_

        inverter =
            invert size domain_

        collector : CommandSet a -> List (Svg a)
        collector token =
            let
                ( cdom, crange ) =
                    ( domain_, range_ )
            in
            token.method token.data token.attributes cdom crange

        events_ =
            events
                |> List.concatMap
                    (\event ->
                        case event of
                            Select sel msg ->
                                selection size offset sel msg inverter [] [] domain_ range_
                    )
    in
    commands
        |> List.concatMap collector
        -- add the events to the end, so it is on top of the other elements
        |> (\list -> list ++ events_)
        |> layer offset


type alias IndySet =
    Bool


type alias CommandSet a =
    { method : Method a
    , data : DataSet
    , attributes : List (Svg.Attribute a)
    }


toCommand : Param a -> CommandSet a
toCommand msg =
    case msg of
        Bar width data ->
            CommandSet (bar width) data []

        Dot data ->
            CommandSet dot data []

        Line data ->
            CommandSet line data []

        Area data ->
            CommandSet area data []

        Domain data ->
            CommandSet noop data []

        Label labelSet ->
            let
                -- map out just the points to use as the underlying data
                data =
                    List.map (\( p, _, _ ) -> p) labelSet
            in
            CommandSet (label labelSet) data []

        -- options
        ZeroLine ->
            CommandSet zeroLine [] []

        Style attr msg_ ->
            let
                token =
                    toCommand msg_
            in
            { token | attributes = attr }

        Highlight sel ->
            CommandSet (highlight sel) [] []


frame : Size -> List (Svg a) -> Svg a
frame size children =
    Svg.svg
        [ setAttr A.width (size.width + 2)
        , setAttr A.height (size.height + 2)
        , joinAttr A.viewBox [ 0, 0, size.width + 2, size.height + 2 ]
        ]
        children


layer : ( Float, Float ) -> List (Svg a) -> Svg a
layer ( x, y ) children =
    Svg.g
        [ A.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
        ]
        children


zeroLine : Method a
zeroLine _ attr domainFunc rangeFunc =
    let
        ( ( x1, _ ), ( x2, _ ) ) =
            domainFunc
    in
    line
        [ ( x1, 0 ), ( x2, 0 ) ]
        attr
        domainFunc
        rangeFunc


noop : Method a
noop data attr _ _ =
    []


line : Method a
line data attr _ rangeFunc =
    [ Svg.path
        ([ fill "none"
         , stroke "#000"
         , setAttr strokeWidth 1
         , d (path rangeFunc data)
         ]
            ++ attr
        )
        []
    ]


area : Method a
area data attr domainFunc rangeFunc =
    let
        ( ( minx, miny ), ( maxx, maxy ) ) =
            domainFunc

        p0 =
            ( minx, miny )

        p1 =
            ( maxx, miny )

        cappedData =
            [ p0 ] ++ data ++ [ p1 ]
    in
    line cappedData attr domainFunc rangeFunc


dot : Method a
dot data attr _ rangeFunc =
    data
        |> scale rangeFunc
        |> List.map
            (\( x, y ) ->
                circle
                    ([ setAttr cx x
                     , setAttr cy y
                     , setAttr r 2
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
                        w * ((x - x0) / (x1 - x0))

                    ( y_, h ) =
                        if y < 0 then
                            ( my y - (my y - my 0), my y - my 0 )

                        else
                            ( my y, my 0 - my y )
                in
                rect
                    ([ setAttr A.x (mx x - p)
                     , setAttr A.y y_
                     , setAttr width w
                     , setAttr height h
                     ]
                        ++ attr
                    )
                    []
            )


label : LabelSet a -> Method a
label labels data styled ( ( x0, y0 ), ( x1, y1 ) ) rangeFunc =
    let
        indexed =
            labels |> Array.fromList
    in
    data
        |> scale rangeFunc
        |> Array.fromList
        |> Array.toIndexedList
        |> List.concatMap
            (\( index, ( x, y ) ) ->
                case Array.get index indexed of
                    Nothing ->
                        []

                    Just ( p, attr, label_ ) ->
                        [ Svg.text_
                            ([ setAttr A.x x
                             , setAttr A.y y
                             ]
                                ++ styled
                                ++ attr
                            )
                            [ Svg.text label_ ]
                        ]
            )


collect : Point -> String -> String
collect ( x, y ) pathStr =
    let
        command =
            if pathStr == "" then
                "M"

            else
                "L"
    in
    pathStr
        ++ command
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y


path : Range -> DataSet -> String
path r data =
    data
        |> scale r
        |> List.foldr collect ""



-- FIXME BUG when domain min and max are equal


domain : List DataSet -> Domain
domain dataset =
    let
        flatData =
            dataset |> List.concatMap (\s -> s)

        seed =
            flatData
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
    flatData
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


{-| creates x and y scale functions
-}
range : Size -> Domain -> Range
range size ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        (x - x0) * (size.width / (x1 - x0)) |> noNan
    , \y ->
        (y1 - y) * (size.height / (y1 - y0)) |> noNan
    )


{-| creates x and y scale inversion functions
-}
invert : Size -> Domain -> Range
invert size ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        ((x1 - x0) / size.width) * x |> noNan
    , \y ->
        (size.height * ((y1 - y0) / y)) |> noNan
    )


{-| remove NaN's with a 0
-}
noNan : Float -> Float
noNan f =
    case isNaN f of
        True ->
            0

        False ->
            f


scale : Range -> DataSet -> DataSet
scale ( mx, my ) data =
    data
        |> List.map (\( x, y ) -> ( mx x, my y ))


setAttr : (String -> a) -> Float -> a
setAttr fun n =
    fun (String.fromFloat n)


joinAttr : (String -> a) -> List Float -> a
joinAttr fun n =
    List.map String.fromFloat n
        |> String.join " "
        |> fun



-- EVENTS


{-| TODO: document
-}
type Event a
    = Select Selection (Selection -> a)


{-| TODO: document and make this private
-}
type Selection
    = Selection Selected


type alias Selected =
    { mouseDown : Bool

    -- the starting point on the page
    , offset : Maybe Point
    , box0 : Maybe Point

    -- the bounding box in absolute size to the SVG graph
    , box1 : Maybe Point
    }


createSelection : Selection
createSelection =
    Selection (Selected False Nothing Nothing Nothing)


{-| The entry point to create a graph. See Param.
-}
subscriptions : Event msg -> Sub msg
subscriptions event =
    let
        map m =
            Json.map2 m
                (Json.field "pageX" Json.int)
                (Json.field "pageY" Json.int)

        -- DRY up
        -- FIXME remove the - 5 and get the margin from the `size`
        offsetPosition sel msg =
            case sel.offset of
                Nothing ->
                    Json.succeed (sel |> Selection |> msg)

                Just ( x0, y0 ) ->
                    map
                        (\x y ->
                            { sel | box1 = Just ( (x |> toFloat) - x0, (y |> toFloat) - y0 ) }
                                |> Selection
                                |> msg
                        )

        active sel msg =
            -- order matters LIFO seems to be the rule, the mouseup cancels the mouse move, maybe?
            [ Browser.onMouseMove (offsetPosition sel msg)
            , Browser.onMouseUp (offsetPosition { sel | mouseDown = False } msg)
            ]
    in
    Sub.batch
        (case event of
            Select (Selection sel) msg ->
                if sel.mouseDown == True then
                    active sel msg

                else
                    []
        )


{-| TODO support mousemove when outside of the region, this will probably require a subscription
-}
selection : Size -> Point -> Selection -> (Selection -> a) -> Range -> Method a
selection size ( layerOffsetX, layerOffsetY ) selected msg inverter data attr domainFunc rangeFunc =
    let
        sel =
            case selected of
                Selection s ->
                    s

        ( ( x1, y1 ), ( x2, y2 ) ) =
            domainFunc

        ( mx, my ) =
            rangeFunc

        ( ix, iy ) =
            inverter

        -- DRY up
        offsetStart m =
            Json.map4
                (\oX oY x y ->
                    let
                        _ =
                            Debug.log "mouse" [ oX, oY, x, y ]
                    in
                    { sel
                        | mouseDown = True
                        , offset =
                            Just
                                ( ((oX - x) |> toFloat) + layerOffsetX, ((oY - y) |> toFloat) + layerOffsetY )
                        , box0 =
                            Just
                                ( x |> toFloat, y |> toFloat )
                        , box1 = Nothing
                    }
                        |> Selection
                        |> msg
                )
                (Json.field "pageX" Json.int)
                (Json.field "pageY" Json.int)
                (Json.field "offsetX" Json.int)
                (Json.field "offsetY" Json.int)

        events =
            if sel.mouseDown == True then
                []

            else
                [ E.on "mousedown" (offsetStart msg)
                ]

        eventArea =
            [ rect
                ([ setAttr A.x (mx x1)
                 , setAttr A.y (my y2)
                 , setAttr width (mx x2)
                 , setAttr height (my y1)
                 , fill "rgba(0,0,0,0.0)"
                 ]
                    ++ events
                )
                []
            ]
    in
    eventArea


highlight : Selection -> Method a
highlight selected _ _ domain_ range_ =
    case selected of
        Selection sel ->
            case ( sel.box0, sel.box1 ) of
                ( Just ( ax1, ay1 ), Just ( bx1, by1 ) ) ->
                    [ rect
                        [ setAttr A.x (min ax1 bx1)
                        , setAttr A.y (min ay1 by1)
                        , setAttr width (ax1 - bx1 |> abs)
                        , setAttr height (ay1 - by1 |> abs)
                        , fill "rgba(255,0,0,0.5)"
                        ]
                        []
                    ]

                _ ->
                    []
