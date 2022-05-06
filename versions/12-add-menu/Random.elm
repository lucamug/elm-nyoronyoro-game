module Random exposing
    ( generateKeystrokes
    , possibleKeys
    )

import Array
import Item.Player as Player
import Keyboard
import Model
import PseudoRandom



-- ██████   █████  ███    ██ ██████   ██████  ███    ███
-- ██   ██ ██   ██ ████   ██ ██   ██ ██    ██ ████  ████
-- ██████  ███████ ██ ██  ██ ██   ██ ██    ██ ██ ████ ██
-- ██   ██ ██   ██ ██  ██ ██ ██   ██ ██    ██ ██  ██  ██
-- ██   ██ ██   ██ ██   ████ ██████   ██████  ██      ██


possibleKeys : List Int -> Array.Array ( Keyboard.Key, Float )
possibleKeys listOfPlayersIndex =
    Array.fromList <|
        List.concat <|
            List.map
                (\index ->
                    let
                        keyMapping =
                            Player.playerGetKeyMapping index
                    in
                    [ ( .left keyMapping, 0.3 )
                    , ( .right keyMapping, 0.3 )
                    , ( .fire keyMapping, 0.6 )
                    , ( .blast keyMapping, 0.005 )
                    , ( .goFaster keyMapping, 0.3 )
                    ]
                )
                listOfPlayersIndex


generateKeystrokesHelper : ( Int, Float, Model.Model ) -> List Keyboard.Key -> List Keyboard.Key
generateKeystrokesHelper ( index, random, model ) acc =
    case Array.get index model.possibleKeysToGenerateRandomly of
        Just found ->
            let
                ( key, probability ) =
                    found
            in
            if random < probability then
                key :: acc

            else
                acc

        Nothing ->
            acc


generateKeystrokes : Model.Model -> List Keyboard.Key
generateKeystrokes model =
    let
        randoms =
            PseudoRandom.floatSequence (round model.count) (Array.length model.possibleKeysToGenerateRandomly) ( 0, 1 )

        indexedRandom =
            List.indexedMap (\index random -> ( index, random, model )) randoms
    in
    List.foldl generateKeystrokesHelper [] indexedRandom
