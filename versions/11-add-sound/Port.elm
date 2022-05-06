port module Port exposing
    ( onblur
    , pauseMusic
    , playEffect
    , startGameMusic
    , startMenuMusic
    , startMusic
    )

-- ██████   ██████  ██████  ████████
-- ██   ██ ██    ██ ██   ██    ██
-- ██████  ██    ██ ██████     ██
-- ██      ██    ██ ██   ██    ██
-- ██       ██████  ██   ██    ██


port onblur : (() -> msg) -> Sub msg


port playSound : String -> Cmd msg


port stopSound : String -> Cmd msg


port pauseSound : String -> Cmd msg



-- ██████   ██████  ██████  ████████
-- ██   ██ ██    ██ ██   ██    ██
-- ██████  ██    ██ ██████     ██
-- ██      ██    ██ ██   ██    ██
-- ██       ██████  ██   ██    ██
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


playEffect : Bool -> String -> Cmd msg
playEffect effectOn effectName =
    if effectOn then
        playSound effectName

    else
        Cmd.none


startMusic : Bool -> Cmd msg
startMusic musicOn =
    if musicOn then
        startGameMusic True

    else
        Cmd.none


startMenuMusic : Bool -> Cmd msg
startMenuMusic musicOn =
    if musicOn then
        Cmd.batch
            [ stopSound "gameMusic"
            , playSound "menuMusic"
            ]

    else
        Cmd.none


startGameMusic : Bool -> Cmd msg
startGameMusic musicOn =
    if musicOn then
        Cmd.batch
            [ stopSound "menuMusic"
            , playSound "gameMusic"
            ]

    else
        Cmd.none


pauseMusic : Cmd msg
pauseMusic =
    Cmd.batch
        [ pauseSound "gameMusic"
        , pauseSound "menuMusic"
        ]
