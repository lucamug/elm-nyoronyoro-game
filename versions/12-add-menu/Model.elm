module Model exposing (Model)

import Array
import Item.Explosion as Explosion
import Item.Player as Player
import Item.Shot as Shot
import Keyboard
import Shared



-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { -- Counter
      count : Float
    , gameSpeed : Float
    , countdown : Float
    , countUpdatedRandomKeys : Float

    -- Keyboard Keys
    , pressedKeys : List Keyboard.Key
    , pressedKeysBefore : List Keyboard.Key
    , pressedKeysRandom : List Keyboard.Key
    , pressedKeysRandomBefore : List Keyboard.Key

    -- States
    , isPaused : Bool
    , gameState : Shared.GameState

    -- Window
    , window : Shared.Window

    -- Object to draw on the canvas
    , players : List Player.Player
    , shots : List Shot.Shot
    , explosions : List Explosion.Explosion

    -- Sounds On/Off
    , musicOn : Bool
    , effectsOn : Bool

    -- Others
    , possibleKeysToGenerateRandomly : Array.Array ( Keyboard.Key, Float )
    }
