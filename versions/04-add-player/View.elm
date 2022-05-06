module View exposing (view)

import Canvas exposing (Renderable, Setting, TextAlign(..), align, alpha, circle, fill, rect, shapes)
import Color
import Html
import Item.Player as Player
import Model
import Msg
import Shared



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


cX : Shared.Window -> Float
cX window =
    window.width / 2


cY : Shared.Window -> Float
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


positionToCanvasPoint : Shared.Position -> Canvas.Point
positionToCanvasPoint { x, y } =
    ( x, y )



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


viewFullscreenRect : Shared.Window -> Color.Color -> List Renderable -> List Renderable
viewFullscreenRect window color renderable =
    renderable
        ++ [ shapes [ fill color ]
                [ rect ( 0, 0 )
                    window.width
                    window.height
                ]
           ]


viewPlayer : Player.Player -> List Renderable
viewPlayer player =
    if player.state == Player.Hit then
        [ shapes
            [ fill <| player.colorTail
            , alpha 0.5
            ]
          <|
            List.map
                (\position ->
                    circle (positionToCanvasPoint position) player.radius
                )
                player.tail
        ]

    else
        [ shapes
            [ fill <| player.colorTail
            ]
          <|
            List.map
                (\position ->
                    circle (positionToCanvasPoint position) player.radius
                )
                player.tail
        , shapes
            [ fill <| player.colorHead
            ]
            [ circle (positionToCanvasPoint player.head) player.radius ]
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


viewPauseBackground : Shared.Window -> List Renderable -> List Renderable
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


viewTexts : Model.Model -> List Renderable -> List Renderable
viewTexts model renderables =
    renderables
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin )
            "[esc] pause"
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 10 )
            "player 1:  [⇦] [⇨] [⇧]"



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


viewCanvas : Model.Model -> List Renderable
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
            |> (\renderables -> renderables ++ viewPlayer model.player)



-- ██    ██ ██ ███████ ██     ██
-- ██    ██ ██ ██      ██     ██
-- ██    ██ ██ █████   ██  █  ██
--  ██  ██  ██ ██      ██ ███ ██
--   ████   ██ ███████  ███ ███


view : Model.Model -> Html.Html Msg.Msg
view model =
    Canvas.toHtml
        ( floor model.window.width, floor model.window.height )
        []
        (viewCanvas model)
