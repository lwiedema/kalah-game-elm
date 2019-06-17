module KalahaMain exposing (main)

import ArrayHelper
import Browser
import Color
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (attribute, disabled, start, style)
import Html.Events exposing (onClick)
import Material.Icons.Action
import Material.Icons.Navigation
import Platform.Sub
import Player exposing (Player(..), Winner(..))
import Settings exposing (SowingSpeed(..))
import String exposing (fromInt)
import Svg
import Svg.Attributes
import Time



-- BEGIN datatypes


type alias Model =
    Game


type Msg
    = Click Player Int
    | NextSowingStep
    | Restart
    | OpenSettings
    | SowingSpeedChanged SowingSpeed
    | SeedNumberChanged Int
    | LastSeedsBehaviourChanged
    | UpsideDownChanged
    | SowOpponentsStoreChanged


initalModel : Model
initalModel =
    { state = Turn One
    , board = GameBoard.initalBoard
    , settings = Settings.defaultSettings
    }



-- END datatypes
-- BEGIN web-app


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initalModel, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.board.sowingState of
        NotSowing ->
            Sub.none

        _ ->
            Time.every (Settings.speedInMilliseconds model.settings.sowingSpeed) (\_ -> NextSowingStep)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click player pos ->
            case model.state of
                Turn onTurn ->
                    -- check if click on house was legal
                    if onTurn == player && not (GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer model.board player) pos == 0) then
                        Game.startSowingSeeds model player pos

                    else
                        model

                _ ->
                    model

        NextSowingStep ->
            Game.nextSowingStep model

        Restart ->
            { initalModel | settings = model.settings, board = GameBoard.buildBoard model.settings }

        OpenSettings ->
            let
                oldSettings =
                    model.settings
            in
            { model
                | settings =
                    { oldSettings
                        | settingsOpen = not model.settings.settingsOpen
                    }
            }

        SowingSpeedChanged speed ->
            let
                oldSettings =
                    model.settings
            in
            { model | settings = { oldSettings | sowingSpeed = speed } }

        SeedNumberChanged n ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | numberOfSeeds = n }
            in
            { initalModel | settings = newSettings, board = GameBoard.buildBoard newSettings }

        LastSeedsBehaviourChanged ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | lastSeedsForFinishingPlayer = not oldSettings.lastSeedsForFinishingPlayer }
            in
            { initalModel | settings = newSettings, board = GameBoard.buildBoard newSettings }

        UpsideDownChanged ->
            let
                oldSettings =
                    model.settings
            in
            { model
                | settings =
                    { oldSettings
                        | upsideDownEnabled = not model.settings.upsideDownEnabled
                    }
            }

        SowOpponentsStoreChanged ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | sowInOpponentsStore = not oldSettings.sowInOpponentsStore }
            in
            { initalModel | settings = newSettings, board = GameBoard.buildBoard newSettings }


view : Model -> Html Msg
view model =
    div []
        [ div [ style "display" "inline-block" ] [ iconButton "Einstellungen" Material.Icons.Action.settings OpenSettings ]
        , div [ style "height" "100%", style "width" "100%" ]
            ([ div
                [ style "width" "948px"
                , style "margin" "0 auto"
                ]
                [ infoView model Two
                , div
                    boardStyle
                    [ div
                        (upsideDown model Two :: storeStyle)
                        [ storeView model Two ]
                    , div
                        [ fillParentHeight
                        , centerText
                        , orderSiblingsHorizontally
                        , style "width" "580px"
                        , style "position" "relative"
                        ]
                        [ div
                            [ fillParentHeight ]
                            [ div
                                rowStyle
                                (rowView model Two)
                            , div
                                [ style "height" "60px"
                                , style "border" "10px solid #dcdcdc"
                                , style "border-radius" "10px"
                                , style "background-color" "#dcdcdc"
                                ]
                                [ case model.state of
                                    Turn _ ->
                                        sowingView model

                                    End _ ->
                                        restartButton
                                ]
                            , div
                                rowStyle
                                (rowView model One)
                            ]
                        ]
                    , div
                        storeStyle
                        [ storeView model One ]
                    ]
                , infoView model One
                ]
             ]
                ++ settingsView model
            )
        ]



-- END web-app
-- BEGIN views


settingsView : Model -> List (Html Msg)
settingsView model =
    case model.settings.settingsOpen of
        True ->
            [ div
                ([ style "background-color" "white"
                 , style "width" "500px"
                 , style "height" "500px"
                 , style "position" "absolute"
                 , style "z-index" "10"
                 , style "top" "60px"
                 , style "border" ("5px solid " ++ sowedSeedColor)
                 , style "border-radius" "10px"
                 , style "opacity" "0.95"
                 , centerText
                 ]
                    ++ defaultTextFont
                )
                [ Html.br [] []
                , Html.text "Einstellungen zur Darstellung"
                , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                    [ div
                        []
                        [ Html.label [] [ Html.text "Tablet-Modus" ]
                        , div (onClick UpsideDownChanged :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.settings.upsideDownEnabled
                                ]
                                []
                            , Html.text "Die Spielelemente für Spieler 2 werden kopfüber dargestellt."
                            ]
                        ]
                    , Html.br [] []
                    , Html.label [] [ Html.text "Geschwindigkeit der Animation" ]
                    , Html.br [] []
                    , div
                        spaceChildrenEvenly
                        [ div (onClick (SowingSpeedChanged Slow) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "sowingSpeed"
                                , Html.Attributes.value "slow"
                                , Html.Attributes.checked (model.settings.sowingSpeed == Slow)
                                ]
                                []
                            , Html.text "Langsam"
                            ]
                        , div (onClick (SowingSpeedChanged Normal) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "sowingSpeed"
                                , Html.Attributes.value "normal"
                                , Html.Attributes.checked (model.settings.sowingSpeed == Normal)
                                ]
                                []
                            , Html.text "Normal"
                            ]
                        , div (onClick (SowingSpeedChanged Fast) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "sowingSpeed"
                                , Html.Attributes.value "fast"
                                , Html.Attributes.checked (model.settings.sowingSpeed == Fast)
                                ]
                                []
                            , Html.text "Schnell"
                            ]
                        ]
                    ]
                , div [ style "height" "2px", style "width" "90%", style "margin" "0 auto", style "background-color" sowingSeedsColor ] []
                , Html.br [] []
                , Html.text "Einstellungen der Spielregeln"
                , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                    [ div [ style "font-size" "14px" ] [ Html.text "Hinweis: Das Spiel wird bei Änderung neu gestartet." ]
                    , Html.br [] []
                    , Html.label [] [ Html.text "Anzahl der Steine pro Mulde" ]
                    , div
                        spaceChildrenEvenly
                        [ div (onClick (SeedNumberChanged 3) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "seedNumber"
                                , Html.Attributes.value "3"
                                , Html.Attributes.checked (model.settings.numberOfSeeds == 3)
                                ]
                                []
                            , Html.text "3"
                            ]
                        , div (onClick (SeedNumberChanged 4) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "seedNumber"
                                , Html.Attributes.value "4"
                                , Html.Attributes.checked (model.settings.numberOfSeeds == 4)
                                ]
                                []
                            , Html.text "4"
                            ]
                        , div (onClick (SeedNumberChanged 6) :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "radio"
                                , Html.Attributes.name "seedNumber"
                                , Html.Attributes.value "6"
                                , Html.Attributes.checked (model.settings.numberOfSeeds == 6)
                                ]
                                []
                            , Html.text "6"
                            ]
                        ]
                    , Html.br [] []
                    , div
                        []
                        [ Html.label [] [ Html.text "Verteilung übriger Steine" ]
                        , div (onClick LastSeedsBehaviourChanged :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.settings.lastSeedsForFinishingPlayer
                                ]
                                []
                            , Html.text "Am Ende des Spieles erhält der Spieler, der keine Steine mehr in seiner Reihe hat, die übrig gebliebenen Steine."
                            ]
                        ]
                    , Html.br [] []
                    , div
                        []
                        [ Html.label [] [ Html.text "Gegnerisches Kalaha" ]
                        , div (onClick SowOpponentsStoreChanged :: settingsChoiceStyle)
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.settings.sowInOpponentsStore
                                ]
                                []
                            , Html.text "Steine werden auch in das gegnerische Kalaha verteilt."
                            ]
                        ]
                    ]
                ]
            ]

        False ->
            []


rowView : Model -> Player -> List (Html Msg)
rowView model player =
    (if player == Two then
        -- mirror list of houses for player Two
        List.foldl (::) []

     else
        identity
    )
        (rowViewHelper model player model.settings.numberOfHouses)


rowViewHelper : Model -> Player -> Int -> List (Html Msg)
rowViewHelper model player housesToCreate =
    -- create houses recursively
    case housesToCreate of
        0 ->
            []

        _ ->
            rowHouseView model player (model.settings.numberOfHouses - housesToCreate)
                :: rowViewHelper model player (housesToCreate - 1)


rowHouseView : Model -> Player -> Int -> Html Msg
rowHouseView model player pos =
    -- create view for one house
    let
        -- get information on house from board
        house =
            ArrayHelper.getWithDefault
                pos
                (GameBoard.getRowForPlayer model.board player)
                { justSownTo = False, seeds = 0 }
    in
    -- show seeds as number and circles
    div
        ([ upsideDown model player
         , cursorStyle model player
         , onClick (Click player pos)
         , fillParentHeight
         , orderSiblingsHorizontally
         , style "width" "72px"
         , style "padding" "0 5px"
         ]
            ++ defaultTextFont
            ++ houseStyle
        )
        [ Html.text (fromInt house.seeds)
        , div
            [ fillParentWidth ]
            (seedsInHouseView house.seeds house.justSownTo)
        ]


seedsInHouseView : Int -> Bool -> List (Html Msg)
seedsInHouseView numOfSeeds justSownTo =
    case numOfSeeds of
        0 ->
            []

        _ ->
            case justSownTo of
                -- newly added seed displayed in other color then normal
                True ->
                    List.repeat (numOfSeeds - 1) (seedView normalSeedColor) ++ [ seedView sowedSeedColor ]

                False ->
                    List.repeat numOfSeeds (seedView normalSeedColor)


storeView : Model -> Player -> Html Msg
storeView model player =
    let
        store =
            GameBoard.getStoreForPlayer model.board player
    in
    div
        ([ centerText
         , style "margin" "10px"
         , style "width" "140px"
         , style "height" "380px"
         ]
            ++ houseStyle
            ++ defaultTextFont
        )
        [ div
            [ fillParentWidth
            , style "padding" "10px 0"
            ]
            [ Html.text (fromInt store.seeds) ]
        , div
            [ style "padding" "10px"
            , fillParentWidth
            ]
            (seedsInHouseView store.seeds store.justSownTo)
        ]


infoView : Model -> Player -> Html Msg
infoView model player =
    div
        ([ upsideDown model player
         , fillParentWidth
         , centerText
         , style "padding" "5px"
         ]
            ++ defaultTextFont
        )
        [ Html.text
            ("Spieler " ++ Player.toString player ++ ": ")
        , Html.br [] []
        , Html.text
            (case model.state of
                Turn p ->
                    if p == player then
                        "Du bist am Zug."

                    else
                        "Spieler " ++ Player.toString p ++ " ist am Zug."

                End winner ->
                    "Spiel beendet. "
                        ++ (case winner of
                                Drawn ->
                                    "Es ist unentschieden."

                                Winner w finalScore ->
                                    (if w == player then
                                        "Du hast gewonnen. "

                                     else
                                        "Leider verloren. "
                                    )
                                        ++ "Endstand: "
                                        ++ String.fromInt (Tuple.first finalScore)
                                        ++ ":"
                                        ++ String.fromInt (Tuple.second finalScore)
                           )
            )
        ]


restartButton : Html Msg
restartButton =
    iconButton "Neues Spiel" Material.Icons.Navigation.refresh Restart


iconButton : String -> (Color.Color -> Int -> Svg.Svg Msg) -> Msg -> Html Msg
iconButton text icon onClickMsg =
    Html.div
        [ onClick onClickMsg
        , style "background-color" sowedSeedColor
        , style "width" "160px"
        , style "height" "30px"
        , style "margin" "10px auto"
        , style "border" ("5px solid " ++ sowedSeedColor)
        , style "border-radius" "10px"
        , pointerCursor
        ]
        [ div [ style "float" "left" ]
            [ Svg.svg
                [ Svg.Attributes.viewBox "0 0 30 30"
                , Svg.Attributes.width "30"
                , Svg.Attributes.height "30"
                ]
                [ icon (Color.rgb255 60 60 60) 30 ]
            ]
        , div
            ([ style "float" "right"
             , style "color" normalSeedColor
             , style "padding" "4px 0"
             ]
                ++ defaultTextFont
            )
            [ Html.text text ]
        ]


sowingView : Model -> Html Msg
sowingView model =
    -- create view showing seeds to be sown in middle of board
    div
        [ style "padding" "17px 0 15px 0"
        ]
        [ div
            (fillParentWidth :: spaceChildrenEvenly)
            [ div []
                (case model.board.sowingState of
                    Sowing info ->
                        seedsToSowView info.seedsToSow

                    SowingFinished _ _ ->
                        seedsToSowView 0

                    HandleLastSeedInEmptyHouse _ _ ->
                        seedsToSowView 0

                    NotSowing ->
                        []
                )
            ]
        ]


seedsToSowView : Int -> List (Html Msg)
seedsToSowView numOfSeeds =
    List.repeat numOfSeeds (seedView sowingSeedsColor)


seedView : String -> Html Msg
seedView seedColor =
    -- creating view for one single seed as svg
    div
        [ orderSiblingsHorizontally
        , style "padding" "2px"
        ]
        [ Svg.svg
            [ Svg.Attributes.viewBox ("0 0 " ++ seedSizeString ++ " " ++ seedSizeString)
            , Svg.Attributes.width seedSizeString
            , Svg.Attributes.height seedSizeString
            ]
            [ Svg.circle
                [ Svg.Attributes.cx seedRadiusString
                , Svg.Attributes.cy seedRadiusString
                , Svg.Attributes.r seedRadiusString
                , Svg.Attributes.fill seedColor
                ]
                []
            ]
        ]



-- END views
-- BEGIN styles & attributes


storeStyle : List (Attribute Msg)
storeStyle =
    [ fillParentHeight
    , orderSiblingsHorizontally
    ]


rowStyle : List (Attribute Msg)
rowStyle =
    spaceChildrenEvenly
        ++ [ style "width" "580px"
           , style "padding" "10px 0"
           , style "height" "140px"
           ]


houseStyle : List (Attribute Msg)
houseStyle =
    [ style "border" "2px solid"
    , style "border-radius" "20px"
    , style "background-color" "#979797"
    , style "overflow" "hidden"
    ]


boardStyle : List (Attribute Msg)
boardStyle =
    [ style "border" ("20px solid " ++ boardBackgroundColor)
    , style "background-color" boardBackgroundColor
    , style "border-radius" "40px"
    , style "height" "408px"
    ]


cursorStyle : Model -> Player -> Attribute Msg
cursorStyle model player =
    case model.state of
        Turn p ->
            case p == player of
                True ->
                    pointerCursor

                False ->
                    style "cursor" "default"

        End _ ->
            style "cursor" "default"


pointerCursor : Attribute Msg
pointerCursor =
    style "cursor" "pointer"


upsideDown : Model -> Player -> Attribute Msg
upsideDown model player =
    if model.settings.upsideDownEnabled && player == Two then
        style "transform" "rotate(180deg)"

    else
        style "" ""


fillParentWidth : Attribute Msg
fillParentWidth =
    style "width" "100%"


fillParentHeight : Attribute Msg
fillParentHeight =
    style "height" "100%"


centerText : Attribute Msg
centerText =
    style "text-align" "center"


spaceChildrenEvenly : List (Attribute Msg)
spaceChildrenEvenly =
    [ style "display" "flex"
    , style "justify-content" "space-evenly"
    ]


orderSiblingsHorizontally : Attribute Msg
orderSiblingsHorizontally =
    style "float" "left"


defaultTextFont : List (Attribute Msg)
defaultTextFont =
    [ style "font-family" "Arial"
    , style "font-size" "20px"
    ]


settingsChoiceStyle : List (Attribute Msg)
settingsChoiceStyle =
    [ pointerCursor, style "font-size" "16px", style "margin-top" "5px" ]



-- END Styles & Attributes
-- BEGIN constants


sowingSeedsColor : String
sowingSeedsColor =
    "#db004e"


sowedSeedColor : String
sowedSeedColor =
    "#00db8d"


normalSeedColor : String
normalSeedColor =
    "#3c3c3c"


boardBackgroundColor : String
boardBackgroundColor =
    "#4b4b4b"


seedSize : Int
seedSize =
    20


seedRadiusString : String
seedRadiusString =
    String.fromInt (seedSize // 2)


seedSizeString : String
seedSizeString =
    String.fromInt seedSize



-- END constants
