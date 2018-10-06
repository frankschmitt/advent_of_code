module Main exposing (..)

import Html exposing (Html, button, div, h1, h2, h3, h4, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List.Extra exposing (minimumBy)
import Model exposing (..)


init : ( Model, Cmd Msg )
init =
    let
        p0_0 =
            { index = 0, position = { x = 3, y = 0, z = 0 }, velocity = { x = 2, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }

        p0_1 =
            { index = 1, position = { x = 4, y = 0, z = 0 }, velocity = { x = 1, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }
    in
    ( { system = initParticleList [ p0_0, p0_1 ] }, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( { model | system = stepSystem model.system }, Cmd.none )

        Run ->
            ( { model | system = runSystem model.system }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , button [ onClick Step ] [ text "Step" ]
        , button [ onClick Run ] [ text "Run" ]
        , viewSystemState model
        ]


vectorToString : Vector3D -> String
vectorToString v =
    "(" ++ toString v.x ++ "," ++ toString v.y ++ "," ++ toString v.z ++ ")"


particleToString : Particle -> String
particleToString p =
    "Particle with index: "
        ++ toString p.index
        ++ ", position: "
        ++ vectorToString p.position
        ++ ", velocity: "
        ++ vectorToString p.velocity
        ++ ", acceleration: "
        ++ vectorToString p.acceleration


renderParticle : Particle -> Html Msg
renderParticle p =
    text (particleToString p)


maybeParticleToString : Maybe Particle -> String
maybeParticleToString part =
    case part of
        Nothing ->
            " - Nothing - "

        Just p ->
            particleToString p


viewSystemState : Model -> Html Msg
viewSystemState model =
    let
        system =
            model.system
    in
    div []
        ([ h4 [] [ text "Particles" ]
         ]
            ++ List.map renderParticle system.particles
            ++ [ h4 [] [ text ("Iteration: " ++ toString system.iteration) ]
               , h4 [] [ text ("CTO: " ++ maybeParticleToString system.closestToOrigin) ]
               , h4 [] [ text ("Iterations since last CTO change: " ++ toString system.iterationsSinceLastCTOChange) ]
               ]
        )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
