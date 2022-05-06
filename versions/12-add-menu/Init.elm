module Init exposing
    ( Flag
    , init
    , initDemoGame1
    )

import Array
import Item.Player as Player
import Keyboard
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
    , gameSpeed = Shared.conf.gameSpeed.verySlow
    , countdown = 0
    , countUpdatedRandomKeys = 0

    -- Keyboard Keys
    , pressedKeys = []
    , pressedKeysBefore = []
    , pressedKeysRandom = []
    , pressedKeysRandomBefore = []

    -- States
    , isPaused = False
    , gameState = Shared.StartMenu Shared.Plain

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
        |> initDemoGame1


initDemoGame1 : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
initDemoGame1 ( model, cmd ) =
    ( { model
        | pressedKeysRandom =
            [ Keyboard.ArrowLeft
            , Keyboard.Shift
            , Keyboard.Character "A"
            , Keyboard.Alt
            , Keyboard.Character "F"
            , Keyboard.Character "R"
            , Keyboard.Character "J"
            , Keyboard.Character "U"
            ]
        , pressedKeysRandomBefore = []
        , players = Player.playersInit model.gameSpeed 4 model.window
        , possibleKeysToGenerateRandomly = Random.possibleKeys [ 0, 1, 2, 3 ]
        , shots = []
        , explosions = []
      }
    , cmd
    )
