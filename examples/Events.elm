module Events exposing (main)

import Browser
import Browser.Events as E
import Html as Html
import Html.Attributes as HA
import Json.Decode as D
import Sparkline exposing (Param(..), Point, Size, sparkline)
import Sparkline.Extras exposing (Axes(..), extent)
import Svg as Svg
import Svg.Attributes as Svg


type alias Model =
    { selection : Sparkline.Selection
    , selection2 : Sparkline.Selection
    }


type Msg
    = Noop
    | GraphEvent Sparkline.Selection
    | GraphEvent2 Sparkline.Selection


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { selection =
                        Sparkline.createSelection
                  , selection2 =
                        Sparkline.createSelection
                  }
                , Cmd.none
                )
        , view = \model -> view model
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sparkline.subscriptions (Sparkline.Select model.selection GraphEvent)
        , Sparkline.subscriptions (Sparkline.Select model.selection2 GraphEvent2)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GraphEvent sel ->
            ( { model | selection = sel }
            , Cmd.none
            )

        GraphEvent2 sel ->
            ( { model | selection2 = sel }
            , Cmd.none
            )


view model =
    let
        print ( x, y ) =
            Html.text (String.fromFloat x ++ "," ++ String.fromFloat y)
    in
    Html.div []
        [ Html.div []
            [ sparkline
                (Size 600 100 5 5)
                [ Sparkline.Select model.selection GraphEvent ]
                [ Line [ ( 0, 10 ), ( 10, 10 ), ( 20, 30 ) ]
                , Line [ ( 0, 2 ), ( 10, 4 ), ( 20, 20 ), ( 30, 40 ), ( 100, 35 ) ]
                ]
            , sparkline
                (Size 600 100 5 5)
                [ Sparkline.Select model.selection2 GraphEvent2 ]
                [ Line [ ( 0, 10 ), ( 10, 10 ), ( 20, 30 ) ]
                , Line [ ( 0, 2 ), ( 10, 4 ), ( 20, 20 ), ( 30, 40 ), ( 100, 35 ) ]
                ]
            ]
        ]
