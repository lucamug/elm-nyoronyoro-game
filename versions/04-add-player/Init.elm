module Init exposing
    ( Flag
    , init
    )

import Item.Player as Player
import Model
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

    -- Keyboard Keys
    , pressedKeys = []
    , pressedKeysBefore = []

    -- State
    , isPaused = False

    -- Window
    , window =
        { width = flag.width / 2
        , height = flag.height / 2
        }

    -- Object to draw on the canvas
    , player =
        Player.playerInit
            { angle = 0
            , colors = Player.player1Color
            , position = Shared.Position 100 100
            }
    }


init : Flag -> ( Model.Model, Cmd msg )
init flag =
    ( initModel flag, Cmd.none )
