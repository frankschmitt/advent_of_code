module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List.Extra exposing (minimumBy)


---- MODEL ----


type alias Vector3D =
    { x : Float
    , y : Float
    , z : Float
    }


type alias Particle =
    { index : Int
    , position : Vector3D
    , velocity : Vector3D
    , acceleration : Vector3D
    }


type alias ParticleSystem =
    { particles : List Particle
    , closestToOrigin : Maybe Particle
    }



-- move the given 3D vector by the given amount (using vector addition)


move : Vector3D -> Vector3D -> Vector3D
move v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y, z = v1.z + v2.z }



-- perform the next step for a given particle


step : Particle -> Particle
step { index, position, velocity, acceleration } =
    let
        v_x =
            velocity.x + acceleration.x

        v_y =
            velocity.y + acceleration.y

        v_z =
            velocity.z + acceleration.z

        x =
            position.x + v_x

        y =
            position.y + v_y

        z =
            position.z + v_z
    in
    { index = index
    , position = { x = x, y = y, z = z }
    , velocity = { x = v_x, y = v_y, z = v_z }
    , acceleration = acceleration
    }


{-| Compute the next step for a given particle list
-}
stepList : List Particle -> List Particle
stepList lst =
    List.map step lst


{-| Initialize a particle list
-}
initParticleList : List Particle -> ParticleSystem
initParticleList particles =
    let
        cto =
            minimumBy (\p -> p.x * p.x + p.y * p.y + p.z * p.z) particles
    in
    { particles = particles, closestToOrigin = cto }


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
