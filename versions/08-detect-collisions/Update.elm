module Update exposing (update)

import Browser.Events
import Collision
import Item.Player as Player
import Item.Shot as Shot
import Keyboard
import Model
import Msg
import Random
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
                    if isKeyPressed (.goFaster <| Player.playerGetKeyMapping index) (model.pressedKeys ++ model.pressedKeysRandom) then
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
                                    model.pressedKeys ++ model.pressedKeysRandom
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


moveShots : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
moveShots ( model, cmd ) =
    let
        newShots =
            List.map
                (\shot ->
                    Shot.shotMove model.window shot
                )
                model.shots
    in
    ( { model | shots = newShots }, cmd )


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


removeExpiredShots : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
removeExpiredShots ( model, cmd ) =
    let
        newShots =
            List.filter (\shot -> shot.life + shot.maxLife > model.count) model.shots
    in
    ( { model | shots = newShots }, cmd )


addNewShotsHelper : ( Int, Player.Player ) -> ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
addNewShotsHelper ( index, player ) ( model, cmd ) =
    let
        isFireKeyPressed =
            isKeyDown
                { model
                    | pressedKeysBefore = model.pressedKeysBefore ++ model.pressedKeysRandomBefore
                    , pressedKeys = model.pressedKeys ++ model.pressedKeysRandom
                }
                (.fire <| Player.playerGetKeyMapping index)

        isBlastKeyPressed =
            isKeyPressed
                (.blast <| Player.playerGetKeyMapping index)
                (model.pressedKeys ++ model.pressedKeysRandom)

        areAmmoLeft =
            player.shots > 0
    in
    if areAmmoLeft && (isFireKeyPressed || isBlastKeyPressed) then
        ( { model
            | shots =
                model.shots
                    ++ [ Shot.shotInit
                            { angle = player.angle
                            , speed = 5
                            , position = player.head
                            , color = player.colorHead
                            , owner = index
                            , life = model.count
                            }
                       ]
            , players =
                List.indexedMap
                    (\index_ player_ ->
                        if index_ == index then
                            { player_ | shots = player_.shots - 1 }

                        else
                            player_
                    )
                    model.players
          }
        , cmd
        )

    else
        ( model, cmd )


addNewShots : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
addNewShots ( model, cmd ) =
    List.foldl addNewShotsHelper ( model, cmd ) (List.indexedMap (\index player -> ( index, player )) model.players)


increaseCounter : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
increaseCounter ( model, cmd ) =
    ( { model | count = model.count + 1 }, cmd )


updatePressedKeysBefore : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
updatePressedKeysBefore ( model, cmd ) =
    ( { model | pressedKeysBefore = model.pressedKeys }, cmd )


updatePressedKeysRandom : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
updatePressedKeysRandom ( model, cmd ) =
    if model.countUpdatedRandomKeys + Shared.conf.cyclesBeforeGeneratingRandomKeys < model.count then
        let
            newGenerateKeystrokes =
                Random.generateKeystrokes model
        in
        ( { model
            | pressedKeysRandomBefore = model.pressedKeysRandom
            , pressedKeysRandom = newGenerateKeystrokes
            , countUpdatedRandomKeys = model.count
          }
        , cmd
        )

    else
        ( { model
            | pressedKeysRandomBefore = model.pressedKeysRandom
          }
        , cmd
        )


startGame :
    { h | botPLayers : List Int, totPlayers : Int }
    -> ( Model.Model, Cmd msg )
    -> ( Model.Model, Cmd msg )
startGame { totPlayers, botPLayers } ( model, cmd ) =
    ( { model
        | players = Player.playersInit totPlayers model.window
        , shots = []
        , possibleKeysToGenerateRandomly = Random.possibleKeys botPLayers
        , pressedKeysRandom = []
        , pressedKeysRandomBefore = []
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


detectCollisions : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
detectCollisions ( model, cmd ) =
    let
        collisions1 =
            Collision.checkPlayersCollisions model

        collisions2 =
            Collision.checkShotsCollisions model
    in
    List.foldl (Collision.collisionDetected model.count)
        ( model, cmd )
        (collisions1 ++ collisions2)



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
                |> removeExpiredShots
                |> playersBackToNormalState
                |> changePlayersSpeed
                |> movePlayers
                |> moveShots
                |> addNewShots
                |> detectCollisions
                |> updatePressedKeysRandom
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
                ( newModel, newCmd ) |> startGame { totPlayers = 4, botPLayers = [ 1, 2, 3 ] }

            else if isKeyDown newModel (Keyboard.Character "2") then
                ( newModel, newCmd ) |> startGame { totPlayers = 2, botPLayers = [] }

            else if isKeyDown newModel (Keyboard.Character "3") then
                ( newModel, newCmd ) |> startGame { totPlayers = 8, botPLayers = [ 2, 3, 4, 5, 6, 7 ] }

            else
                ( newModel, newCmd )
