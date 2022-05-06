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
    , countUpdatedRandomKeys : Float

    -- Keyboard Keys
    , pressedKeys : List Keyboard.Key
    , pressedKeysBefore : List Keyboard.Key
    , pressedKeysRandom : List Keyboard.Key
    , pressedKeysRandomBefore : List Keyboard.Key

    -- State
    , isPaused : Bool

    -- Window
    , window : Shared.Window

    -- Object to draw on the canvas
    , players : List Player.Player
    , shots : List Shot.Shot
    , explosions : List Explosion.Explosion

    -- Others
    , possibleKeysToGenerateRandomly : Array.Array ( Keyboard.Key, Float )
    }
