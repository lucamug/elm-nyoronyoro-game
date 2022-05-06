module Shared exposing
    ( Position
    , Window
    , conf
    , distance
    , dropLastMemberOfAList
    , fixBoundary
    , norm
    , transformMaybesFromListRemovingNothings
    )

--  ██████  ██████  ███    ██ ███████ ██  ██████  ██    ██ ██████   █████  ████████ ██  ██████  ███    ██
-- ██      ██    ██ ████   ██ ██      ██ ██       ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ████   ██
-- ██      ██    ██ ██ ██  ██ █████   ██ ██   ███ ██    ██ ██████  ███████    ██    ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██  ██ ██ ██      ██ ██    ██ ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ██  ██ ██
--  ██████  ██████  ██   ████ ██      ██  ██████   ██████  ██   ██ ██   ██    ██    ██  ██████  ██   ████


conf :
    { cyclesBeforeGeneratingRandomKeys : Float
    , damages :
        { collisionWithAnotherPlayer : Float
        , collisionWithShot : Float
        , extraSpeed : Float
        }
    , playerInitialShots : Int
    , playerInitialEnergy : Float
    , playerLength : Int
    , playerNormalSpeed : Float
    , playerFasterSpeed : Float
    , playerRotationNormalSpeed : Float
    , playerRotationFasterSpeed : Float
    , playerNormalRadius : Float
    , playerFasterRadius : Float
    , playerInvulnerablePeriod : Float
    , shotInitialRadius : Float
    , shotMaxLife : Float
    }
conf =
    { damages =
        { collisionWithAnotherPlayer = 10
        , collisionWithShot = 5
        , extraSpeed = 0.05
        }
    , cyclesBeforeGeneratingRandomKeys = 30
    , playerInitialEnergy = 100
    , playerInitialShots = 500
    , playerLength = 60
    , playerNormalSpeed = 2
    , playerFasterSpeed = 4
    , playerRotationNormalSpeed = 0.1
    , playerRotationFasterSpeed = 0.3
    , playerNormalRadius = 7
    , playerFasterRadius = 4
    , playerInvulnerablePeriod = 100
    , shotInitialRadius = 3
    , shotMaxLife = 60
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


transformMaybesFromListRemovingNothings : List (Maybe a) -> List a
transformMaybesFromListRemovingNothings list =
    List.foldr
        (\item list_ ->
            case item of
                Nothing ->
                    list_

                Just v ->
                    v :: list_
        )
        []
        list


distance : Position -> Position -> Float
distance position1 position2 =
    sqrt (((position1.y - position2.y) ^ 2) + ((position1.x - position2.x) ^ 2))


dropLastMemberOfAList : List a -> List a
dropLastMemberOfAList list =
    List.take (List.length list - 1) list


norm : number -> number
norm number =
    if number < 0 then
        0

    else
        number
