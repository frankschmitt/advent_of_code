module Main exposing (..)

import DemoModel exposing (..)
import Html exposing (Html, button, div, h1, h2, h3, h4, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List.Extra exposing (minimumBy)
import Model exposing (..)
import SimulationModel exposing (..)


init : ( Model, Cmd Msg )
init =
    -- ( demoModel, Cmd.none )
    ( simulationModel, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( { model | system = stepSystem model.system model.removeColliders }, Cmd.none )

        Run ->
            ( { model | system = runSystem model.system model.removeColliders }, Cmd.none )

        ToggleRemoveColliders ->
            ( { model | removeColliders = not model.removeColliders }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        label =
            case model.removeColliders of
                True ->
                    "Remove colliders: ON"

                False ->
                    "Remove colliders: OFF"
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , button [ onClick Step ] [ text "Step" ]
        , button [ onClick Run ] [ text "Run" ]
        , button [ onClick ToggleRemoveColliders ] [ text label ]
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
               , h4 [] [ text ("#Particles left: " ++ toString (List.length system.particles)) ]
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
