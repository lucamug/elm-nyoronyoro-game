module Item.Explosion exposing
    ( Explosion
    , ItemType(..)
    , explosionInit
    )

import Collision
import Color
import Shared



-- ███████ ██   ██ ██████  ██       ██████  ███████ ██  ██████  ███    ██
-- ██       ██ ██  ██   ██ ██      ██    ██ ██      ██ ██    ██ ████   ██
-- █████     ███   ██████  ██      ██    ██ ███████ ██ ██    ██ ██ ██  ██
-- ██       ██ ██  ██      ██      ██    ██      ██ ██ ██    ██ ██  ██ ██
-- ███████ ██   ██ ██      ███████  ██████  ███████ ██  ██████  ██   ████


type ItemType
    = ItemShot
    | ItemPlayer


type alias Explosion =
    { angle : Float
    , position : Shared.Position
    , color : Color.Color
    , life : Float
    , maxLife : Float
    , type_ : ItemType
    }


explosionInit : Float -> Collision.Collision -> Maybe Explosion
explosionInit count collision =
    case collision of
        Collision.CollisionShotPlayer shot _ ->
            Just
                { angle = shot.angle
                , position = shot.position
                , color = shot.color
                , life = count
                , maxLife = 20
                , type_ = ItemShot
                }

        Collision.CollisionPlayerPlayer player1 player2 ->
            Just
                { angle = player1.angle
                , position = player1.head
                , color = player2.colorHead
                , life = count
                , maxLife = 60
                , type_ = ItemPlayer
                }

        Collision.NoCollision ->
            Nothing
