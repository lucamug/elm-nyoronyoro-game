module Item.Player exposing
    ( Player
    , PlayerState(..)
    , playerColors
    , playerGetColor
    , playerGetKeyMapping
    , playerInit
    , playerKeyMapping
    , playerMove
    , playersInit
    )

import Array
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
    , shots : Int
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


playerKeyMapping : Array.Array KeyMapping
playerKeyMapping =
    Array.fromList
        [ player1KeyMapping
        , { left = Keyboard.Character "A"
          , right = Keyboard.Character "D"
          , fire = Keyboard.Character "Q"
          , goFaster = Keyboard.Character "W"
          , blast = Keyboard.Character "Z"
          }
        , { left = Keyboard.Character "F"
          , right = Keyboard.Character "H"
          , fire = Keyboard.Character "G"
          , goFaster = Keyboard.Character "T"
          , blast = Keyboard.Character "R"
          }
        , { left = Keyboard.Character "J"
          , right = Keyboard.Character "L"
          , fire = Keyboard.Character "K"
          , goFaster = Keyboard.Character "I"
          , blast = Keyboard.Character "U"
          }
        , { left = Keyboard.Character "1"
          , right = Keyboard.Character "2"
          , fire = Keyboard.Character "3"
          , goFaster = Keyboard.Character "4"
          , blast = Keyboard.Character "5"
          }
        , { left = Keyboard.Character "6"
          , right = Keyboard.Character "7"
          , fire = Keyboard.Character "8"
          , goFaster = Keyboard.Character "9"
          , blast = Keyboard.Character "0"
          }
        , { left = Keyboard.Character "X"
          , right = Keyboard.Character "V"
          , fire = Keyboard.Character "B"
          , goFaster = Keyboard.Character "N"
          , blast = Keyboard.Character "M"
          }
        , { left = Keyboard.Character ","
          , right = Keyboard.Character "/"
          , fire = Keyboard.Character "."
          , goFaster = Keyboard.Character ";"
          , blast = Keyboard.Character "'"
          }
        ]


playerGetKeyMapping : Int -> KeyMapping
playerGetKeyMapping index =
    Maybe.withDefault player1KeyMapping (Array.get index playerKeyMapping)


type alias PlayerColor =
    { head : Color.Color
    , tail : Color.Color
    }


player1Color : PlayerColor
player1Color =
    { head = Color.rgb 0.5 0 0.8 -- #8000cc
    , tail = Color.rgb 0.7 0 0.8
    }


playerColors : Array.Array PlayerColor
playerColors =
    Array.fromList
        [ player1Color
        , { head = Color.rgb 0.9 0.3 0.5 -- #e64d80
          , tail = Color.rgb 1 0.4 0.7
          }
        , { head = Color.rgb 0.3 0.6 0.1 -- #4d991a
          , tail = Color.rgb 0.5 0.7 0.2
          }
        , { head = Color.rgb 0.9 0.5 0.3 -- #e6804d
          , tail = Color.rgb 1 0.7 0.4
          }
        , { head = Color.rgb255 77 124 230 -- #4d7ce6
          , tail = Color.rgb255 97 144 250
          }
        , { head = Color.rgb255 150 165 3 -- #96a503
          , tail = Color.rgb255 170 185 23
          }
        , { head = Color.rgb255 121 85 72 -- #795548
          , tail = Color.rgb255 141 105 92
          }
        , { head = Color.rgb255 95 125 139 -- #607d8b
          , tail = Color.rgb255 115 145 159
          }
        ]


playerGetColor : Int -> PlayerColor
playerGetColor index =
    Maybe.withDefault player1Color (Array.get index playerColors)


playerInit :
    { a
        | angle : Float
        , colors : PlayerColor
        , position : Shared.Position
        , gameSpeed : Float
    }
    -> Player
playerInit { position, angle, colors, gameSpeed } =
    let
        length =
            round (toFloat Shared.conf.playerLength / gameSpeed)
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
    , shots = Shared.conf.playerInitialShots
    , radius = Shared.conf.playerNormalRadius
    }


playersInit : Float -> Int -> { a | height : Float, width : Float } -> List Player
playersInit gameSpeed quantity { width, height } =
    List.take quantity
        [ playerInit
            { colors = playerGetColor 0
            , position = Shared.Position 50 50
            , angle = 0
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 1
            , position = Shared.Position (width - 50) (height - 50)
            , angle = pi
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 2
            , position = Shared.Position (width - 50) 50
            , angle = (pi * 3) / 2
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 3
            , position = Shared.Position 50 (height - 50)
            , angle = pi / 2
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 4
            , position = Shared.Position 100 100
            , angle = 0
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 5
            , position = Shared.Position (width - 100) (height - 100)
            , angle = pi
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 6
            , position = Shared.Position (width - 100) 100
            , angle = (pi * 3) / 2
            , gameSpeed = gameSpeed
            }
        , playerInit
            { colors = playerGetColor 7
            , position = Shared.Position 100 (height - 100)
            , angle = pi / 2
            , gameSpeed = gameSpeed
            }
        ]


playerMoveHelper : Player -> Shared.Position -> Player
playerMoveHelper player { x, y } =
    { player
        | head = { x = x, y = y }
        , tail = player.head :: Shared.dropLastMemberOfAList player.tail
    }


playerMove : Float -> Shared.Window -> Player -> Player
playerMove gameSpeed window player =
    let
        newPosition { x, y } =
            Shared.Position
                (x + (gameSpeed * player.speed * sin player.angle))
                (y + (gameSpeed * player.speed * cos player.angle))

        position =
            player.head
                |> newPosition
                |> Shared.fixBoundary window
    in
    playerMoveHelper player position
