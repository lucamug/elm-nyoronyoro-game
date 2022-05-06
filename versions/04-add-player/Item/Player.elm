module Item.Player exposing
    ( Player
    , PlayerState(..)
    , player1Color
    , player1KeyMapping
    , playerInit
    , playerMove
    )

import Color
import Keyboard
import Shared



-- ██████  ██       █████  ██    ██ ███████ ██████
-- ██   ██ ██      ██   ██  ██  ██  ██      ██   ██
-- ██████  ██      ███████   ████   █████   ██████
-- ██      ██      ██   ██    ██    ██      ██   ██
-- ██      ███████ ██   ██    ██    ███████ ██   ██


type PlayerState
    = Normal
    | Hit


type alias Player =
    { head : Shared.Position
    , tail : List Shared.Position
    , angle : Float
    , speed : Float
    , rotationSpeed : Float
    , colorHead : Color.Color
    , colorTail : Color.Color
    , state : PlayerState
    , stateLife : Float
    , energy : Float
    , radius : Float
    }


type alias KeyMapping =
    { fire : Keyboard.Key
    , blast : Keyboard.Key
    , left : Keyboard.Key
    , right : Keyboard.Key
    , goFaster : Keyboard.Key
    }


player1KeyMapping : KeyMapping
player1KeyMapping =
    { left = Keyboard.ArrowLeft
    , right = Keyboard.ArrowRight
    , fire = Keyboard.Enter
    , goFaster = Keyboard.ArrowUp
    , blast = Keyboard.Shift
    }


type alias PlayerColor =
    { head : Color.Color
    , tail : Color.Color
    }


player1Color : PlayerColor
player1Color =
    { head = Color.rgb 0.5 0 0.8 -- #8000cc
    , tail = Color.rgb 0.7 0 0.8
    }


playerInit :
    { a
        | angle : Float
        , colors : PlayerColor
        , position : Shared.Position
    }
    -> Player
playerInit { position, angle, colors } =
    let
        length =
            round (toFloat Shared.conf.playerLength)
    in
    { head = position
    , tail =
        List.map
            (\_ ->
                position
            )
            (List.repeat
                (length - 1)
                ""
            )
    , angle = angle
    , speed = Shared.conf.playerNormalSpeed
    , rotationSpeed = Shared.conf.playerRotationNormalSpeed
    , colorHead = colors.head
    , colorTail = colors.tail
    , state = Normal
    , stateLife = 0
    , energy = Shared.conf.playerInitialEnergy
    , radius = Shared.conf.playerNormalRadius
    }


playerMoveHelper : Player -> Shared.Position -> Player
playerMoveHelper player { x, y } =
    { player
        | head = { x = x, y = y }
        , tail = player.head :: Shared.dropLastMemberOfAList player.tail
    }


playerMove : Shared.Window -> Player -> Player
playerMove window player =
    let
        newPosition { x, y } =
            Shared.Position
                (x + (player.speed * sin player.angle))
                (y + (player.speed * cos player.angle))

        position =
            player.head
                |> newPosition
                |> Shared.fixBoundary window
    in
    playerMoveHelper player position
