module Shared exposing
    ( Position
    , Window
    , conf
    , dropLastMemberOfAList
    , fixBoundary
    , norm
    )

--  ██████  ██████  ███    ██ ███████ ██  ██████  ██    ██ ██████   █████  ████████ ██  ██████  ███    ██
-- ██      ██    ██ ████   ██ ██      ██ ██       ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ████   ██
-- ██      ██    ██ ██ ██  ██ █████   ██ ██   ███ ██    ██ ██████  ███████    ██    ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██  ██ ██ ██      ██ ██    ██ ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ██  ██ ██
--  ██████  ██████  ██   ████ ██      ██  ██████   ██████  ██   ██ ██   ██    ██    ██  ██████  ██   ████


conf :
    { damages :
        { extraSpeed : Float
        }
    , playerInitialEnergy : Float
    , playerLength : Int
    , playerNormalSpeed : Float
    , playerFasterSpeed : Float
    , playerRotationNormalSpeed : Float
    , playerRotationFasterSpeed : Float
    , playerNormalRadius : Float
    , playerFasterRadius : Float
    , playerInvulnerablePeriod : Float
    }
conf =
    { damages =
        { extraSpeed = 0.05
        }
    , playerInitialEnergy = 100
    , playerLength = 60
    , playerNormalSpeed = 2
    , playerFasterSpeed = 4
    , playerRotationNormalSpeed = 0.1
    , playerRotationFasterSpeed = 0.3
    , playerNormalRadius = 7
    , playerFasterRadius = 4
    , playerInvulnerablePeriod = 100
    }



-- ███████ ██   ██  █████  ██████  ███████ ██████
-- ██      ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ███████ ███████ ██████  █████   ██   ██
--      ██ ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ██   ██ ██   ██ ██   ██ ███████ ██████
--
-- ████████ ██    ██ ██████  ███████ ███████
--    ██     ██  ██  ██   ██ ██      ██
--    ██      ████   ██████  █████   ███████
--    ██       ██    ██      ██           ██
--    ██       ██    ██      ███████ ███████


type alias Window =
    { height : Float
    , width : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }



-- ███████ ██   ██  █████  ██████  ███████ ██████
-- ██      ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ███████ ███████ ██████  █████   ██   ██
--      ██ ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ██   ██ ██   ██ ██   ██ ███████ ██████
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


fixBoundary :
    { a | height : Float, width : Float }
    -> Position
    -> Position
fixBoundary { width, height } { x, y } =
    { x =
        if x < 0 then
            width

        else if x > width then
            0

        else
            x
    , y =
        if y < 0 then
            height

        else if y > height then
            0

        else
            y
    }


dropLastMemberOfAList : List a -> List a
dropLastMemberOfAList list =
    List.take (List.length list - 1) list


norm : number -> number
norm number =
    if number < 0 then
        0

    else
        number
