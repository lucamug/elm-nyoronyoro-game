module Init exposing
    ( Flag
    , init
    )

import Item.Player as Player
import Model



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
    , players = []
    }


init : Flag -> ( Model.Model, Cmd msg )
init flag =
    ( initModel flag, Cmd.none )
        |> initGame


initGame : ( Model.Model, Cmd msg ) -> ( Model.Model, Cmd msg )
initGame ( model, cmd ) =
    ( { model
        | players = Player.playersInit 4 model.window
      }
    , cmd
    )
