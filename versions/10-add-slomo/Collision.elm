module Collision exposing
    ( Collision(..)
    , checkPlayersCollisions
    , checkShotsCollisions
    , collisionDetected
    )

import Item.Player as Player
import Item.Shot as Shot
import Shared



--  ██████  ██████  ██      ██      ██ ███████ ██  ██████  ███    ██
-- ██      ██    ██ ██      ██      ██ ██      ██ ██    ██ ████   ██
-- ██      ██    ██ ██      ██      ██ ███████ ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██      ██      ██      ██ ██ ██    ██ ██  ██ ██
--  ██████  ██████  ███████ ███████ ██ ███████ ██  ██████  ██   ████


type Collision
    = CollisionShotPlayer Shot.Shot Player.Player
    | CollisionPlayerPlayer Player.Player Player.Player
    | NoCollision


checkCollisionBetweenPlayers : Player.Player -> Player.Player -> Collision
checkCollisionBetweenPlayers player1 player2 =
    let
        head =
            player1.head

        tail =
            player2.tail

        minDistanceBeforeCollision =
            player1.radius + player2.radius

        collisions =
            List.map (\tailPart -> Shared.distance head tailPart < minDistanceBeforeCollision) tail
    in
    if List.length (List.filter (\a -> a) collisions) > 0 then
        CollisionPlayerPlayer player1 player2

    else
        NoCollision


checkCollisionBetweenShotAndPlayer : Shot.Shot -> Player.Player -> Collision
checkCollisionBetweenShotAndPlayer shot player =
    let
        head =
            shot.position

        tail =
            player.tail

        minDistanceBeforeCollision =
            shot.radius + player.radius

        collisions =
            List.map (\tailPart -> Shared.distance head tailPart < minDistanceBeforeCollision) tail
    in
    if List.length (List.filter (\a -> a) collisions) > 0 then
        CollisionShotPlayer shot player

    else
        NoCollision


checkPlayersCollisions : { a | players : List Player.Player } -> List Collision
checkPlayersCollisions model =
    List.filter (\collision -> collision /= NoCollision) <|
        List.concat <|
            List.map
                (\player1 ->
                    List.map
                        (\player2 ->
                            if player1 == player2 || player1.state == Player.Hit || player2.state == Player.Hit then
                                NoCollision

                            else
                                checkCollisionBetweenPlayers player1 player2
                        )
                        model.players
                )
                model.players


checkShotsCollisions : { a | shots : List Shot.Shot, players : List Player.Player } -> List Collision
checkShotsCollisions model =
    List.filter (\collision -> collision /= NoCollision) <|
        List.concat <|
            List.map
                (\shot ->
                    List.indexedMap
                        (\index player ->
                            if player.state == Player.Hit || shot.owner == index then
                                NoCollision

                            else
                                checkCollisionBetweenShotAndPlayer shot player
                        )
                        model.players
                )
                model.shots


collisionDetected :
    Float
    -> Collision
    -> ( { a | players : List Player.Player, shots : List Shot.Shot }, b )
    -> ( { a | players : List Player.Player, shots : List Shot.Shot }, b )
collisionDetected count collision ( model, cmd ) =
    let
        newPlayers =
            List.indexedMap
                (\index player ->
                    case collision of
                        CollisionPlayerPlayer player1 player2 ->
                            if player == player1 then
                                { player
                                    | state = Player.Hit
                                    , stateLife = count
                                    , energy = player.energy - Shared.conf.damages.collisionWithAnotherPlayer
                                }

                            else if player == player2 then
                                { player
                                    | energy = player.energy + Shared.conf.damages.collisionWithAnotherPlayer
                                }

                            else
                                player

                        CollisionShotPlayer shot player_ ->
                            if player == player_ then
                                { player
                                    | state = Player.Hit
                                    , stateLife = count
                                    , energy = player.energy - Shared.conf.damages.collisionWithShot
                                }

                            else if shot.owner == index then
                                { player
                                    | energy = player.energy + Shared.conf.damages.collisionWithShot
                                }

                            else
                                player

                        NoCollision ->
                            player
                )
                model.players

        newShots =
            -- Removing collided shots
            List.map
                (\shot ->
                    case collision of
                        CollisionShotPlayer shot_ _ ->
                            if shot == shot_ then
                                { shot | life = shot.maxLife }

                            else
                                shot

                        _ ->
                            shot
                )
                model.shots
    in
    ( { model | players = newPlayers, shots = newShots }, cmd )
