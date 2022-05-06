module Msg exposing (Msg(..))

import Browser.Events
import Keyboard



-- ███    ███ ███████  ██████
-- ████  ████ ██      ██
-- ██ ████ ██ ███████ ██   ███
-- ██  ██  ██      ██ ██    ██
-- ██      ██ ███████  ██████


type Msg
    = Frame Float
    | KeyMsg Keyboard.Msg
    | OnBlur ()
    | OnVisibilityChange Browser.Events.Visibility
