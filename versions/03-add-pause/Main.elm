port module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Renderable, Setting, TextAlign(..), align, fill, rect, shapes)
import Color
import Html
import Keyboard



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

    -- States
    , isPaused : Bool

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

    -- Keyboard Keys
    , pressedKeys = []
    , pressedKeysBefore = []

    -- States
    , isPaused = False

    -- Window
    , window =
        { width = flag.width / 2
        , height = flag.height / 2
        }
    }


init : Flag -> ( Model, Cmd msg )
init flag =
    ( initModel flag, Cmd.none )



-- ██████   ██████  ██████  ████████
-- ██   ██ ██    ██ ██   ██    ██
-- ██████  ██    ██ ██████     ██
-- ██      ██    ██ ██   ██    ██
-- ██       ██████  ██   ██    ██


port onblur : (() -> msg) -> Sub msg



-- ███████ ██    ██ ██████  ███████  ██████ ██████  ██ ██████  ████████ ██  ██████  ███    ██ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██   ██ ██ ██   ██    ██    ██ ██    ██ ████   ██ ██
-- ███████ ██    ██ ██████  ███████ ██      ██████  ██ ██████     ██    ██ ██    ██ ██ ██  ██ ███████
--      ██ ██    ██ ██   ██      ██ ██      ██   ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
-- ███████  ██████  ██████  ███████  ██████ ██   ██ ██ ██         ██    ██  ██████  ██   ████ ███████


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Sub.map KeyMsg Keyboard.subscriptions
        , onblur OnBlur
        , Browser.Events.onVisibilityChange OnVisibilityChange
        ]
            ++ (if model.isPaused then
                    []

                else
                    [ Browser.Events.onAnimationFrameDelta Frame ]
               )



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



-- ██   ██ ███████ ██    ██ ██████   ██████   █████  ██████  ██████
-- ██  ██  ██       ██  ██  ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
-- █████   █████     ████   ██████  ██    ██ ███████ ██████  ██   ██
-- ██  ██  ██         ██    ██   ██ ██    ██ ██   ██ ██   ██ ██   ██
-- ██   ██ ███████    ██    ██████   ██████  ██   ██ ██   ██ ██████


isKeyPressed : Keyboard.Key -> List Keyboard.Key -> Bool
isKeyPressed key pressedKeys =
    List.length (List.filter (\key_ -> key_ == key) pressedKeys) == 1


isKeyDown : Model -> Keyboard.Key -> Bool
isKeyDown model key =
    not (isKeyPressed key model.pressedKeysBefore)
        && isKeyPressed key model.pressedKeys


anyKeyPressed : Model -> Bool
anyKeyPressed model =
    List.length model.pressedKeysBefore == 0 && List.length model.pressedKeys > 0



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
--
-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████


startPause : ( Model, Cmd msg ) -> ( Model, Cmd msg )
startPause ( model, cmd ) =
    ( { model | isPaused = True }, cmd )


stopPause : ( Model, Cmd msg ) -> ( Model, Cmd msg )
stopPause ( model, cmd ) =
    ( { model | isPaused = False }, cmd )


increaseCounter : ( Model, Cmd msg ) -> ( Model, Cmd msg )
increaseCounter ( model, cmd ) =
    ( { model | count = model.count + 1 }, cmd )


updatePressedKeysBefore : ( Model, Cmd msg ) -> ( Model, Cmd msg )
updatePressedKeysBefore ( model, cmd ) =
    ( { model | pressedKeysBefore = model.pressedKeys }, cmd )


updatePressedKeys : Keyboard.Msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
updatePressedKeys keyMsg ( model, cmd ) =
    let
        pressedKeys =
            Keyboard.update keyMsg model.pressedKeys
    in
    ( { model
        | pressedKeysBefore = model.pressedKeys
        , pressedKeys = pressedKeys
      }
    , cmd
    )



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OnBlur _ ->
            startPause ( { model | pressedKeys = [] }, Cmd.none )

        OnVisibilityChange visibility ->
            case visibility of
                Browser.Events.Hidden ->
                    startPause ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Frame _ ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none ) |> increaseCounter
            in
            ( newModel, newCmd )
                |> updatePressedKeysBefore

        KeyMsg keyMsg ->
            let
                ( newModel, newCmd ) =
                    ( model, Cmd.none )
                        |> updatePressedKeys keyMsg
            in
            if model.isPaused then
                -- Game is paused
                if anyKeyPressed newModel then
                    -- if any key is pressed we remove the pause
                    ( newModel, newCmd ) |> stopPause

                else
                    ( newModel, newCmd )

            else if isKeyDown newModel Keyboard.Escape then
                ( newModel, newCmd ) |> startPause

            else
                ( newModel, newCmd )



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


cX : Window -> Float
cX window =
    window.width / 2


cY : Window -> Float
cY window =
    window.height / 2


colorWhilePlaying : Color.Color
colorWhilePlaying =
    Color.rgb255 139 109 154


colorStartMenu : Color.Color
colorStartMenu =
    Color.rgb255 0 0 0


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


viewPauseBackground : Window -> List Renderable -> List Renderable
viewPauseBackground window renderables =
    let
        cx_ =
            cX window

        cy_ =
            cY window
    in
    renderables
        ++ [ shapes
                [ fill <| Color.rgb 1 0.7 0.4 ]
                [ rect ( cx_ - 130, cy_ - 80 ) 260 160 ]
           ]


keyboardKeyToString : List Keyboard.Key -> String
keyboardKeyToString keys =
    String.join ", " <|
        List.map
            (\key ->
                case key of
                    Keyboard.Character k ->
                        k

                    _ ->
                        "key"
            )
            keys


viewTexts : Model -> List Renderable -> List Renderable
viewTexts model renderables =
    renderables
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin )
            "[esc] pause"
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 10 )
            ("keyboard: " ++ (String.toLower <| keyboardKeyToString model.pressedKeys))
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 20 )
            ("count: " ++ (String.padLeft 5 ' ' <| String.fromFloat model.count))



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
    if model.isPaused then
        []
            |> viewPauseBackground model.window
            |> viewLogo colorStartMenu
            |> textComposable [ font 48, fill Color.black, align Center ] ( cX model.window, cY model.window - 10 ) "pause"
            |> textComposable [ font 10, fill Color.black, align Center ] ( cX model.window, cY model.window + 30 ) "press any key to resume"

    else
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
