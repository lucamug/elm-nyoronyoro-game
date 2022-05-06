module Model exposing (Model)

import Item.Player as Player
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

    -- Keyboard Keys
    , pressedKeys : List Keyboard.Key
    , pressedKeysBefore : List Keyboard.Key

    -- State
    , isPaused : Bool

    -- Window
    , window : Shared.Window

    -- Object to draw on the canvas
    , player : Player.Player
    }
