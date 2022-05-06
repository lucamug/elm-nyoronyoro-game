port module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Renderable, Setting, TextAlign(..), align, fill, rect, shapes)
import Color
import Html



-- ███    ███  ██████  ██████  ███████ ██
-- ████  ████ ██    ██ ██   ██ ██      ██
-- ██ ████ ██ ██    ██ ██   ██ █████   ██
-- ██  ██  ██ ██    ██ ██   ██ ██      ██
-- ██      ██  ██████  ██████  ███████ ███████


type alias Model =
    { -- Counter
      count : Float

    -- Window
    , window : Window
    }



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


initModel : Flag -> Model
initModel flag =
    { -- Counter
      count = 0

    -- Window
    , window =
        { width = flag.width / 2
        , height = flag.height / 2
        }
    }


init : Flag -> ( Model, Cmd msg )
init flag =
    ( initModel flag, Cmd.none )



-- ███████ ██    ██ ██████  ███████  ██████ ██████  ██ ██████  ████████ ██  ██████  ███    ██ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██   ██ ██ ██   ██    ██    ██ ██    ██ ████   ██ ██
-- ███████ ██    ██ ██████  ███████ ██      ██████  ██ ██████     ██    ██ ██    ██ ██ ██  ██ ███████
--      ██ ██    ██ ██   ██      ██ ██      ██   ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
-- ███████  ██████  ██████  ███████  ██████ ██   ██ ██ ██         ██    ██  ██████  ██   ████ ███████


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [ Browser.Events.onAnimationFrameDelta Frame
        ]



-- ███    ███ ███████  ██████
-- ████  ████ ██      ██
-- ██ ████ ██ ███████ ██   ███
-- ██  ██  ██      ██ ██    ██
-- ██      ██ ███████  ██████


type Msg
    = Frame Float



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )



-- ███████ ██   ██  █████  ██████  ███████ ██████
-- ██      ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ███████ ███████ ██████  █████   ██   ██
--      ██ ██   ██ ██   ██ ██   ██ ██      ██   ██
-- ███████ ██   ██ ██   ██ ██   ██ ███████ ██████
--
-- ████████ ██    ██ ██████  ███████ ███████
--    ██     ██  ██  ██   ██ ██      ██
--    ██      ████   ██████  █████   ███████
--    ██       ██    ██      ██           ██
--    ██       ██    ██      ███████ ███████


type alias Window =
    { height : Float
    , width : Float
    }



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


colorWhilePlaying : Color.Color
colorWhilePlaying =
    Color.rgb255 139 109 154


margin : Float
margin =
    10


text : List Setting -> Canvas.Point -> String -> Renderable
text settings point string =
    Canvas.text settings
        point
        (string
            |> String.replace "a" "A"
        )


textComposable : List Setting -> Canvas.Point -> String -> List Renderable -> List Renderable
textComposable settings point string renderables =
    renderables ++ [ text settings point string ]


font : Int -> Setting
font size =
    Canvas.font { size = size, family = "Major Mono Display, monospace" }



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
-- ██████  ███████ ███    ██ ██████  ███████ ██████   █████  ██████  ██      ███████ ███████
-- ██   ██ ██      ████   ██ ██   ██ ██      ██   ██ ██   ██ ██   ██ ██      ██      ██
-- ██████  █████   ██ ██  ██ ██   ██ █████   ██████  ███████ ██████  ██      █████   ███████
-- ██   ██ ██      ██  ██ ██ ██   ██ ██      ██   ██ ██   ██ ██   ██ ██      ██           ██
-- ██   ██ ███████ ██   ████ ██████  ███████ ██   ██ ██   ██ ██████  ███████ ███████ ███████


viewFullscreenRect : Window -> Color.Color -> List Renderable -> List Renderable
viewFullscreenRect window color renderable =
    renderable
        ++ [ shapes [ fill color ]
                [ rect ( 0, 0 )
                    window.width
                    window.height
                ]
           ]


viewLogo : Color.Color -> List Renderable -> List Renderable
viewLogo color renderable =
    renderable
        ++ [ text
                [ font 29
                , fill color
                , align Left
                ]
                ( margin, margin + 16 )
                "ニョロニョロ"
           , text
                [ font 8
                , fill color
                , align Left
                ]
                ( margin + 4, margin + 28 )
                "n  y  o  r  o  n  y  o  r  o"
           ]


viewTexts : Model -> List Renderable -> List Renderable
viewTexts model renderables =
    renderables
        |> textComposable [ font 200, fill colorWhilePlaying, align Left ]
            ( model.window.width / 2 - 220, (model.window.height / 2) + 60 )
            (String.padLeft 3 '0' <| String.fromInt <| modBy 1000 (round model.count))



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███
--
--  ██████  █████  ███    ██ ██    ██  █████  ███████
-- ██      ██   ██ ████   ██ ██    ██ ██   ██ ██
-- ██      ███████ ██ ██  ██ ██    ██ ███████ ███████
-- ██      ██   ██ ██  ██ ██  ██  ██  ██   ██      ██
--  ██████ ██   ██ ██   ████   ████   ██   ██ ███████


viewCanvas : Model -> List Renderable
viewCanvas model =
    []
        |> viewFullscreenRect model.window (Color.rgb 0.9 0.7 1)
        |> viewLogo colorWhilePlaying
        |> viewTexts model



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███


view : Model -> Html.Html Msg
view model =
    Canvas.toHtml
        ( floor model.window.width, floor model.window.height )
        []
        (viewCanvas model)



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
