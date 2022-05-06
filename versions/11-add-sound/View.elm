module View exposing (view)

import Canvas exposing (Renderable, Setting, TextAlign(..), align, alpha, circle, fill, rect, shapes)
import Color
import Html
import Item.Explosion as Explosion
import Item.Player as Player
import Item.Shot as Shot
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


totalPoints :
    List Player.Player
    ->
        List
            { index : Int
            , player : Player.Player
            , points : Float
            }
totalPoints players =
    List.indexedMap
        (\index player ->
            { index = index
            , points = (player.energy * 15) + toFloat player.shots
            , player = player
            }
        )
        players


viewPlayerScore :
    Shared.Window
    -> Int
    ->
        { c
            | index : Int
            , player : Player.Player
            , points : Float
        }
    -> List Renderable
viewPlayerScore window indexVerticalPosition totalPoint =
    let
        player =
            totalPoint.player

        textAttrs fontSize color =
            [ font fontSize
            , fill <| color
            , align Right
            ]

        width =
            70

        height =
            2

        ( x, y ) =
            ( window.width - margin - width, toFloat indexVerticalPosition * 44 )
    in
    [ text (textAttrs 12 (.head <| Player.playerGetColor totalPoint.index))
        ( x + width, margin + y + 12 )
        ("player " ++ (String.fromInt <| totalPoint.index + 1))

    -- Shots
    , shapes [ fill <| Color.rgb 0.7 0.5 0.8 ]
        [ rect ( x, y + 26 ) width height ]
    , shapes [ fill <| player.colorHead ]
        [ rect ( x, y + 26 )
            (width * (toFloat (Shared.norm player.shots) / toFloat Shared.conf.playerInitialShots))
            height
        ]

    -- Energy
    , shapes [ fill <| Color.rgb 0.7 0.5 0.8 ]
        [ rect ( x, y + 30 ) width height ]
    , shapes [ fill <| player.colorHead ]
        [ rect ( x, y + 30 )
            (width * (Shared.norm player.energy / Shared.conf.playerInitialEnergy))
            height
        ]

    -- Points
    , text (textAttrs 10 (.head <| Player.playerGetColor totalPoint.index))
        ( x + width, margin + y + 34 )
        (String.fromInt <| round totalPoint.points)
    ]



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


viewShot : Shot.Shot -> List Renderable
viewShot shot =
    [ shapes
        [ fill shot.color
        ]
        [ circle (positionToCanvasPoint shot.position) 3 ]
    ]


viewExplosionHelper :
    { d
        | gameSpeed : Float
        , deltaLife : Float
        , explosion : Explosion.Explosion
        , explosionSize : Float
        , explosionSpeed : Float
        , sizeMoltiplicator : Float
        , window : Shared.Window
    }
    -> Float
    -> Canvas.Shape
viewExplosionHelper data angle =
    let
        newPosition =
            Shared.fixBoundary data.window <|
                Shared.Position
                    (data.explosion.position.x + (data.gameSpeed * data.deltaLife * data.explosionSpeed * sin angle))
                    (data.explosion.position.y + (data.gameSpeed * data.deltaLife * data.explosionSpeed * cos angle))
    in
    circle (positionToCanvasPoint newPosition) (data.sizeMoltiplicator * data.deltaLife * data.explosionSize)


viewExplosion : Float -> Float -> Shared.Window -> Explosion.Explosion -> List Renderable
viewExplosion gameSpeed count window explosion =
    let
        data =
            { gameSpeed = gameSpeed
            , deltaLife = deltaLife
            , explosion = explosion
            , explosionSize = explosionSize
            , explosionSpeed = explosionSpeed
            , sizeMoltiplicator = sizeMoltiplicator
            , window = window
            }

        deltaLife =
            count - explosion.life

        ( explosionSize, explosionSpeed ) =
            case explosion.type_ of
                Explosion.ItemShot ->
                    ( 3, 3 )

                Explosion.ItemPlayer ->
                    ( 5, 1 )

        sizeMoltiplicator =
            4 / explosion.maxLife

        alphaMoltiplicator =
            1 / explosion.maxLife
    in
    [ shapes
        [ alpha (1 - (alphaMoltiplicator * deltaLife))
        , fill explosion.color
        ]
        [ viewExplosionHelper data <| explosion.angle
        , viewExplosionHelper data <| explosion.angle + 0.5
        , viewExplosionHelper data <| explosion.angle - 0.5
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


viewScore : Model.Model -> List Renderable -> List Renderable
viewScore model renderables =
    renderables
        ++ (List.concat <|
                List.indexedMap
                    (\index totalPoint ->
                        viewPlayerScore model.window index totalPoint
                    )
                    (List.reverse (List.sortBy .points (totalPoints model.players)))
           )


viewTexts : Model.Model -> List Renderable -> List Renderable
viewTexts model renderables =
    renderables
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin )
            (explanationText model)
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 10 )
            (speedText model)
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 20 )
            "player 2:  [a] [d]   [q]     [z]     [w]"
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 30 )
            "player 1:  [⇦] [⇨] [enter] [shift]  [⇧]"
        |> textComposable [ font 8, fill colorWhilePlaying, align Left ]
            ( margin, model.window.height - margin - 40 )
            "[1] [2] [3] start game"


viewPlayers : List Player.Player -> List Renderable -> List Renderable
viewPlayers players renderables =
    renderables ++ List.concat (List.map (\player -> viewPlayer player) players)


viewShots : List Shot.Shot -> List Renderable -> List Renderable
viewShots shots renderables =
    renderables ++ List.concat (List.map (\shot -> viewShot shot) shots)


viewExplosions : Float -> Float -> Shared.Window -> List Explosion.Explosion -> List Renderable -> List Renderable
viewExplosions gameSpeed count window explosions renderables =
    renderables ++ List.concat (List.map (\explosion -> viewExplosion gameSpeed count window explosion) explosions)



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
            |> viewScore model
            |> viewTexts model
            |> viewPlayers model.players
            |> viewShots model.shots
            |> viewExplosions model.gameSpeed model.count model.window model.explosions



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



-- ████████ ███████ ██   ██ ████████
--    ██    ██       ██ ██     ██
--    ██    █████     ███      ██
--    ██    ██       ██ ██     ██
--    ██    ███████ ██   ██    ██


explanationText : { a | effectsOn : Bool, musicOn : Bool } -> String
explanationText model =
    "[esc] pause  [m] music "
        ++ (if model.musicOn then
                "off"

            else
                "on"
           )
        ++ "  [e] effects "
        ++ (if model.effectsOn then
                "off"

            else
                "on"
           )


speedText : Model.Model -> String
speedText model =
    "[5] "
        ++ (if model.gameSpeed == Shared.conf.gameSpeed.slow then
                ">slow<"

            else
                " slow "
           )
        ++ "  [6] "
        ++ (if model.gameSpeed == Shared.conf.gameSpeed.normal then
                ">normal<"

            else
                " normal "
           )
        ++ "  [7] "
        ++ (if model.gameSpeed == Shared.conf.gameSpeed.fast then
                ">fast<"

            else
                " fast "
           )
