module Update exposing (update)

import Browser.Events
import Item.Player as Player
import Keyboard
import Model
import Msg
import Shared



-- ██   ██ ███████ ██    ██ ██████   ██████   █████  ██████  ██████
-- ██  ██  ██       ██  ██  ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
-- █████   █████     ████   ██████  ██    ██ ███████ ██████  ██   ██
-- ██  ██  ██         ██    ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
-- ██   ██ ███████    ██    ██████   ██████  ██   ██ ██   ██ ██████


isKeyPressed : Keyboard.Key -> List Keyboard.Key -> Bool
isKeyPressed key pressedKeys =
    List.length (List.filter (\key_ -> key_ == key) pressedKeys) == 1


isKeyDown : Model.Model -> Keyboard.Key -> Bool
isKeyDown model key =
    not (isKeyPressed key model.pressedKeysBefore)
        && isKeyPressed key model.pressedKeys


anyKeyPressed : Model.Model -> Bool
anyKeyPressed model =
    List.length model.pressedKeysBefore == 0 && List.length model.pressedKeys > 0



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


startPause : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
startPause ( model, cmd ) =
    ( { model | isPaused = True }, cmd )


stopPause : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
stopPause ( model, cmd ) =
    ( { model | isPaused = False }, cmd )


changePlayersSpeed : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
changePlayersSpeed ( model, cmd ) =
    let
        newPlayers =
            List.indexedMap
                (\index player ->
                    if isKeyPressed (.goFaster <| Player.playerGetKeyMapping index) model.pressedKeys then
                        { player
                            | speed = Shared.conf.playerFasterSpeed
                            , rotationSpeed = Shared.conf.playerRotationFasterSpeed
                            , energy = player.energy - Shared.conf.damages.extraSpeed
                            , radius = Shared.conf.playerFasterRadius
                        }

                    else
                        { player
                            | speed = Shared.conf.playerNormalSpeed
                            , rotationSpeed = Shared.conf.playerRotationNormalSpeed
                            , radius = Shared.conf.playerNormalRadius
                        }
                )
                model.players
    in
    ( { model | players = newPlayers }, cmd )


movePlayers : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
movePlayers ( model, cmd ) =
    let
        newPlayers =
            List.indexedMap
                (\index player ->
                    let
                        newAngle =
                            let
                                key1 =
                                    .left <| Player.playerGetKeyMapping index

                                key2 =
                                    .right <| Player.playerGetKeyMapping index

                                pressedKeys =
                                    model.pressedKeys
                            in
                            if isKeyPressed key1 pressedKeys then
                                player.angle + player.rotationSpeed

                            else if isKeyPressed key2 pressedKeys then
                                player.angle - player.rotationSpeed

                            else
                                player.angle
                    in
                    Player.playerMove model.window { player | angle = newAngle }
                )
                model.players
    in
    ( { model | players = newPlayers }, cmd )


playersBackToNormalState : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
playersBackToNormalState ( model, cmd ) =
    let
        newPlayers =
            List.map
                (\player ->
                    if
                        player.state
                            == Player.Hit
                            && player.stateLife
                            + Shared.conf.playerInvulnerablePeriod
                            < model.count
                    then
                        { player | state = Player.Normal }

                    else
                        player
                )
                model.players
    in
    ( { model | players = newPlayers }, cmd )


increaseCounter : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
increaseCounter ( model, cmd ) =
    ( { model | count = model.count + 1 }, cmd )


updatePressedKeysBefore : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
updatePressedKeysBefore ( model, cmd ) =
    ( { model | pressedKeysBefore = model.pressedKeys }, cmd )


startGame :
    Int
    -> ( Model.Model, Cmd msg )
    -> ( Model.Model, Cmd msg )
startGame totPlayers ( model, cmd ) =
    ( { model
        | players = Player.playersInit totPlayers model.window
      }
    , cmd
    )


updatePressedKeys : Keyboard.Msg -> ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
updatePressedKeys keyMsg ( model, cmd ) =
    let
        pressedKeys =
            Keyboard.update keyMsg model.pressedKeys
    in
    ( { model
        | pressedKeysBefore = model.pressedKeys
        , pressedKeys = pressedKeys
      }
    , cmd
    )



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd msg )
update msg model =
    case msg of
        Msg.OnBlur _ ->
            startPause ( { model | pressedKeys = [] }, Cmd.none )

        Msg.OnVisibilityChange visibility ->
            case visibility of
                Browser.Events.Hidden ->
                    startPause ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Msg.Frame _ ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none ) |> increaseCounter
            in
            ( newModel, newCmd )
                |> playersBackToNormalState
                |> changePlayersSpeed
                |> movePlayers
                |> updatePressedKeysBefore

        Msg.KeyMsg keyMsg ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> updatePressedKeys keyMsg
            in
            if model.isPaused then
                -- Game is paused
                if anyKeyPressed newModel then
                    -- if any key is pressed we remove the pause
                    ( newModel, newCmd ) |> stopPause

                else
                    ( newModel, newCmd )

            else if isKeyDown newModel Keyboard.Escape then
                ( newModel, newCmd ) |> startPause

            else if isKeyDown newModel (Keyboard.Character "1") then
                ( newModel, newCmd ) |> startGame 4

            else if isKeyDown newModel (Keyboard.Character "2") then
                ( newModel, newCmd ) |> startGame 2

            else if isKeyDown newModel (Keyboard.Character "3") then
                ( newModel, newCmd ) |> startGame 8

            else
                ( newModel, newCmd )
