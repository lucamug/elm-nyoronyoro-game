port module Port exposing
    ( onblur
    , pauseMusic
    , playEffect
    , playEffectOnlyWhilePlaying
    , startGameMusic
    , startMenuMusic
    , startMusic
    )

import Shared



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


playEffectOnlyWhilePlaying : Bool -> Shared.GameState -> String -> Cmd msg
playEffectOnlyWhilePlaying effectOn gameState effectName =
    case gameState of
        Shared.StartMenu _ ->
            Cmd.none

        Shared.Playing _ ->
            playEffect effectOn effectName


startMusic : Shared.GameState -> Bool -> Cmd msg
startMusic gameState musicOn =
    if musicOn then
        if
            gameState
                == Shared.StartMenu Shared.Plain
                || gameState
                == Shared.StartMenu Shared.Credits
        then
            startMenuMusic True

        else
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
