module Sparkline.Extras exposing
    ( Axes(..)
    , extent
    )

{-| Additional functions to help in generating the graphs.


# Definition

@docs Axes


# Data types

@docs extent

-}


y : ( Float, Float ) -> Float
y ( _, y_ ) =
    y_


x : ( Float, Float ) -> Float
x ( x_, _ ) =
    x_


{-| Descibes which axes on the graph or from the data to use.
-}
type Axes
    = X
    | Y


{-| returns the min and max Point on either the X or Y axes
-}
extent : Axes -> List ( Float, Float ) -> List ( Float, Float )
extent axes data =
    let
        getter =
            case axes of
                X ->
                    x

                Y ->
                    y
    in
    case List.head data of
        Nothing ->
            []

        Just h ->
            let
                ( minv, maxv ) =
                    List.foldr
                        (\point ( min, max ) ->
                            ( if getter point < getter min then
                                point

                              else
                                min
                            , if getter point > getter max then
                                point

                              else
                                max
                            )
                        )
                        ( h, h )
                        data
            in
            [ minv, maxv ]
