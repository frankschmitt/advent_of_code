module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Vector3D =
    { x : Float
    , y : Float
    , z : Float
    }


move : Vector3D -> Vector3D -> Vector3D
move v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y, z = v1.z + v2.z }


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
