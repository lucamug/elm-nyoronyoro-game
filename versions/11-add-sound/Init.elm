module Init exposing
    ( Flag
    , init
    )

import Array
import Item.Player as Player
import Model
import Port
import Random
import Shared



-- ███████ ██       █████   ██████
-- ██      ██      ██   ██ ██
-- █████   ██      ███████ ██   ███
-- ██      ██      ██   ██ ██    ██
-- ██      ███████ ██   ██  ██████


type alias Flag =
    { width : Float
    , height : Float
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██


initModel : Flag -> Model.Model
initModel flag =
    { -- Counter
      count = 0
    , gameSpeed = Shared.conf.gameSpeed.normal
    , countUpdatedRandomKeys = 0

    -- Keyboard Keys
    , pressedKeys = []
    , pressedKeysBefore = []
    , pressedKeysRandom = []
    , pressedKeysRandomBefore = []

    -- State
    , isPaused = False

    -- Window
    , window =
        { width = flag.width / 2
        , height = flag.height / 2
        }

    -- Object to draw on the canvas
    , players = []
    , shots = []
    , explosions = []

    -- Sounds On/Off
    , musicOn = True
    , effectsOn = True

    -- Others
    , possibleKeysToGenerateRandomly = Array.empty
    }


init : Flag -> ( Model.Model, Cmd msg )
init flag =
    ( initModel flag, Port.startMenuMusic True )
        |> initGame


initGame : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
initGame ( model, cmd ) =
    ( { model
        | pressedKeysRandom = []
        , pressedKeysRandomBefore = []
        , players = Player.playersInit model.gameSpeed 4 model.window
        , possibleKeysToGenerateRandomly = Random.possibleKeys [ 1, 2, 3 ]
        , shots = []
        , explosions = []
      }
    , cmd
    )
