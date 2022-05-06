module Item.Shot exposing
    ( Shot
    , shotInit
    , shotMove
    )

import Color
import Shared



-- ███████ ██   ██  ██████  ████████
-- ██      ██   ██ ██    ██    ██
-- ███████ ███████ ██    ██    ██
--      ██ ██   ██ ██    ██    ██
-- ███████ ██   ██  ██████     ██


type alias Shot =
    { angle : Float
    , speed : Float
    , position : Shared.Position
    , color : Color.Color
    , owner : Int
    , life : Float
    , maxLife : Float
    , radius : Float
    }


shotInit :
    { d
        | angle : Float
        , speed : Float
        , position : Shared.Position
        , color : Color.Color
        , owner : Int
        , life : Float
    }
    -> Shot
shotInit shot =
    { angle = shot.angle
    , speed = shot.speed
    , position = shot.position
    , color = shot.color
    , owner = shot.owner
    , life = shot.life
    , maxLife = Shared.conf.shotMaxLife
    , radius = Shared.conf.shotInitialRadius
    }


shotMove : Float -> Shared.Window -> Shot -> Shot
shotMove gameSpeed window shot =
    let
        newPosition { x, y } =
            Shared.Position
                (x + (gameSpeed * shot.speed * sin shot.angle))
                (y + (gameSpeed * shot.speed * cos shot.angle))

        position =
            shot.position
                |> newPosition
                |> Shared.fixBoundary window
    in
    { shot | position = position }
