module Subscriptions exposing (subscriptions)

import Browser.Events
import Keyboard
import Model
import Msg
import Port



-- ███████ ██    ██ ██████  ███████  ██████ ██████  ██ ██████  ████████ ██  ██████  ███    ██ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██   ██ ██ ██   ██    ██    ██ ██    ██ ████   ██ ██
-- ███████ ██    ██ ██████  ███████ ██      ██████  ██ ██████     ██    ██ ██    ██ ██ ██  ██ ███████
--      ██ ██    ██ ██   ██      ██ ██      ██   ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
-- ███████  ██████  ██████  ███████  ██████ ██   ██ ██ ██         ██    ██  ██████  ██   ████ ███████


subscriptions : Model.Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch <|
        [ Sub.map Msg.KeyMsg Keyboard.subscriptions
        , Port.onblur Msg.OnBlur
        , Browser.Events.onVisibilityChange Msg.OnVisibilityChange
        ]
            ++ (if model.isPaused then
                    []

                else
                    [ Browser.Events.onAnimationFrameDelta Msg.Frame ]
               )
