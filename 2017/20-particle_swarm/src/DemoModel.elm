module DemoModel exposing (..)

import Model exposing (..)


demoModel : Model
demoModel =
    let
        p0_0 =
            { index = 0, position = { x = 3, y = 0, z = 0 }, velocity = { x = 2, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }

        p0_1 =
            { index = 1, position = { x = 4, y = 0, z = 0 }, velocity = { x = 1, y = 0, z = 0 }, acceleration = { x = -1, y = 0, z = 0 } }
    in
    { system = initParticleList [ p0_0, p0_1 ]
    , removeColliders = False
    }
