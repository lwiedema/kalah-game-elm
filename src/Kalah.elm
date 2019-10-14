module Kalah exposing (main)

import Array
import Browser
import Color
import Datatypes exposing (ErrorType(..), Model(..), Msg(..), SettingOption(..))
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes exposing (href, style, target)
import Html.Events exposing (onClick)
import KalahaAI
import Localization exposing (Language(..))
import Material.Icons.Action
import Material.Icons.Navigation
import Material.Icons.Toggle
import Player exposing (Player(..), Winner(..))
import Random
import Settings exposing (Intelligence(..), Opponent(..), Settings, SowingSpeed(..))
import String
import Svg
import Svg.Attributes
import Time



-- BEGIN web-app


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initalModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameOk game ->
            case game.board.sowingState of
                NotSowing ->
                    case game.state of
                        Turn player ->
                            case game.settings.opponent of
                                Real ->
                                    Sub.none

                                Computer _ ->
                                    if player == Two then
                                        Time.every
                                            (Settings.speedInMilliseconds game.settings.sowingSpeed)
                                            (\_ -> ComputerHasTurn)

                                    else
                                        Sub.none

                        End _ ->
                            Sub.none

                _ ->
                    Time.every
                        (Settings.speedInMilliseconds game.settings.sowingSpeed)
                        (\_ -> NextSowingStep)

        GameError _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOk game ->
            case msg of
                Click player pos ->
                    case game.state of
                        Turn onTurn ->
                            -- check if click on house was legal
                            if
                                (onTurn
                                    == player
                                )
                                    && not (GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer game.board player) pos == 0)
                            then
                                ( case Game.startSowingSeeds game player pos of
                                    Just g ->
                                        GameOk g

                                    Nothing ->
                                        GameError StartSowingIllegal
                                , Cmd.none
                                )

                            else
                                ( GameOk game, Cmd.none )

                        _ ->
                            ( GameOk game, Cmd.none )

                NextSowingStep ->
                    ( case Game.nextSowingStep game of
                        Just g ->
                            GameOk g

                        Nothing ->
                            GameError NextSowingStepIllegal
                    , Cmd.none
                    )

                Restart ->
                    ( GameOk (restartGame game.settings), Cmd.none )

                OpenSettings ->
                    let
                        oldSettings =
                            game.settings
                    in
                    ( GameOk
                        { game
                            | settings =
                                { oldSettings
                                    | settingsOpen = not game.settings.settingsOpen
                                }
                        }
                    , Cmd.none
                    )

                SettingChanged option ->
                    let
                        oldSettings =
                            game.settings
                    in
                    ( GameOk
                        (case option of
                            Speed speed ->
                                { game | settings = { oldSettings | sowingSpeed = speed } }

                            LanguageSetting language ->
                                { game | settings = { oldSettings | language = language } }

                            SeedNumber n ->
                                restartGame { oldSettings | numberOfSeeds = n }

                            LastSeedsBehaviour ->
                                restartGame { oldSettings | lastSeedsForFinishingPlayer = not oldSettings.lastSeedsForFinishingPlayer }

                            UpsideDown ->
                                { game
                                    | settings =
                                        { oldSettings
                                            | upsideDownEnabled = not game.settings.upsideDownEnabled
                                        }
                                }

                            SowOpponentsStore ->
                                restartGame { oldSettings | sowInOpponentsStore = not oldSettings.sowInOpponentsStore }

                            OpponentOption ->
                                restartGame (Settings.toggleOpponentOption oldSettings)

                            IntelligenceOption i ->
                                restartGame { oldSettings | opponent = Computer i }

                            StartingPlayer ->
                                restartGame { oldSettings | playerTwoStarting = not oldSettings.playerTwoStarting }
                        )
                    , Cmd.none
                    )

                ComputerHasTurn ->
                    ( GameOk game, Random.generate RandomMoveWeights (KalahaAI.weightMoves game.settings) )

                RandomMoveWeights weights ->
                    ( case KalahaAI.nextMove game weights of
                        Just idx ->
                            case Game.startSowingSeeds game Two idx of
                                Just g ->
                                    GameOk g

                                Nothing ->
                                    GameError StartSowingIllegal

                        Nothing ->
                            GameError BestMoveZero
                    , Cmd.none
                    )

                DoNothing ->
                    ( model, Cmd.none )

        GameError _ ->
            ( case msg of
                Restart ->
                    GameOk (restartGame Settings.defaultSettings)

                _ ->
                    model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model of
        GameOk game ->
            div []
                [ div
                    [ style "display" "inline-block" ]
                    [ div []
                        [ div [ style "float" "left" ]
                            [ iconButton
                                (Localization.settings game.settings.language)
                                Material.Icons.Action.settings
                                OpenSettings
                            ]
                        , a
                            [ href "https://github.com/lwiedema/kalah-game-elm"
                            , target "_blank"
                            , style "float" "right"
                            , style "margin-left" "10px"
                            ]
                            [ iconButton
                                (Localization.moreInfo game.settings.language)
                                Material.Icons.Action.info
                                DoNothing
                            ]
                        ]
                    ]
                , div [ style "height" "100%", style "width" "100%" ]
                    (div
                        [ style "width" "948px"
                        , style "margin" "0 auto"
                        ]
                        [ infoView game Two
                        , div
                            boardStyle
                            [ div
                                (upsideDown game Two :: storeStyle)
                                [ storeView game Two ]
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
                                        (rowView game Two)
                                    , div
                                        [ style "height" "60px"
                                        , style "border" "10px solid #dcdcdc"
                                        , style "border-radius" "10px"
                                        , style "background-color" "#dcdcdc"
                                        ]
                                        [ case game.state of
                                            Turn _ ->
                                                sowingView game

                                            End _ ->
                                                iconButton
                                                    (Localization.restart game.settings.language)
                                                    Material.Icons.Navigation.refresh
                                                    Restart
                                        ]
                                    , div
                                        rowStyle
                                        (rowView game One)
                                    ]
                                ]
                            , div
                                storeStyle
                                [ storeView game One ]
                            ]
                        , infoView game One
                        ]
                        :: settingsView game
                    )
                ]

        GameError errorType ->
            div []
                [ Html.text "Sorry! An error has occurred."
                , Html.br [] []
                , Html.text (Datatypes.errorCode errorType ++ ": " ++ Datatypes.errorToString errorType)
                , Html.br [] []
                , Html.text "Please reload game."
                , iconButton (Localization.restart English) Material.Icons.Navigation.refresh Restart
                ]


initalModel : Model
initalModel =
    -- for seeing error page use this initalModel:
    -- GameError Unexpected
    GameOk (restartGame Settings.defaultSettings)


restartGame : Settings -> Game
restartGame settings =
    { settings = settings
    , board = GameBoard.buildBoard settings
    , state =
        Turn
            (if settings.playerTwoStarting then
                Two

             else
                One
            )
    }



-- END web-app
-- BEGIN views


rowView : Game -> Player -> List (Html Msg)
rowView game player =
    (if player == Two then
        -- mirror list of houses for player Two
        List.foldl (::) []

     else
        identity
    )
        (rowViewHelper game player game.settings.numberOfHouses)


rowViewHelper : Game -> Player -> Int -> List (Html Msg)
rowViewHelper game player housesToCreate =
    -- create houses recursively
    case housesToCreate of
        0 ->
            []

        _ ->
            rowHouseView game player (game.settings.numberOfHouses - housesToCreate)
                :: rowViewHelper game player (housesToCreate - 1)


rowHouseView : Game -> Player -> Int -> Html Msg
rowHouseView game player pos =
    -- create view for one house
    let
        -- get information on house from board
        h =
            Array.get pos (GameBoard.getRowForPlayer game.board player)
    in
    case h of
        Just house ->
            -- show seeds as number and circles
            div
                ([ upsideDown game player
                 , cursorStyle game player
                 , fillParentHeight
                 , orderSiblingsHorizontally
                 , style "width" "72px"
                 , style "padding" "0 5px"
                 ]
                    ++ defaultTextFont
                    ++ houseStyle
                    ++ (case game.board.sowingState of
                            NotSowing ->
                                [ onClick (Click player pos) ]

                            _ ->
                                []
                       )
                )
                [ Html.text (String.fromInt house.seeds)
                , div
                    [ fillParentWidth ]
                    (seedsInHouseView house.seeds house.justSown)
                ]

        Nothing ->
            div [] []


seedsInHouseView : Int -> Int -> List (Html Msg)
seedsInHouseView numOfSeeds numOfNewlySownSeeds =
    case numOfSeeds of
        0 ->
            []

        _ ->
            -- newly added seed displayed in other color then normal
            List.repeat (numOfSeeds - numOfNewlySownSeeds) (seedView normalSeedColor)
                ++ List.repeat numOfNewlySownSeeds (seedView sowedSeedColor)


storeView : Game -> Player -> Html Msg
storeView game player =
    let
        store =
            GameBoard.getStoreForPlayer game.board player
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
            [ Html.text (String.fromInt store.seeds) ]
        , div
            [ style "padding" "10px"
            , fillParentWidth
            ]
            (seedsInHouseView store.seeds store.justSown)
        ]


infoView : Game -> Player -> Html Msg
infoView game player =
    div
        ([ upsideDown game player
         , fillParentWidth
         , centerText
         , style "padding" "5px"
         ]
            ++ defaultTextFont
        )
        [ Html.text
            (Localization.player game.settings.language
                ++ " "
                ++ Player.toString player
                ++ (case game.settings.opponent of
                        Real ->
                            ""

                        Computer _ ->
                            if player == Two then
                                " (Computer)"

                            else
                                ""
                   )
                ++ ": "
            )
        , Html.br [] []
        , Html.text
            (case game.state of
                Turn p ->
                    if p == player then
                        Localization.yourTurn game.settings.language

                    else
                        Localization.opponentsTurn game.settings.language (Localization.player game.settings.language ++ " " ++ Player.toString p)

                End winner ->
                    Localization.gameOver game.settings.language
                        ++ " "
                        ++ (case winner of
                                Drawn ->
                                    Localization.drawnGame game.settings.language

                                Winner w finalScore ->
                                    (if w == player then
                                        Localization.youWin game.settings.language

                                     else
                                        Localization.youLoose game.settings.language
                                    )
                                        ++ " "
                                        ++ Localization.finalScore game.settings.language
                                        ++ ": "
                                        ++ String.fromInt (Tuple.first finalScore)
                                        ++ ":"
                                        ++ String.fromInt (Tuple.second finalScore)
                           )
            )
        ]


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


sowingView : Game -> Html Msg
sowingView game =
    -- create view showing seeds to be sown in middle of board
    div
        [ style "padding" "17px 0 15px 0"
        ]
        [ div
            (fillParentWidth :: spaceChildrenEvenly)
            [ div []
                (case game.board.sowingState of
                    Sowing info ->
                        List.repeat info.seedsToSow (seedView sowingSeedsColorString)

                    _ ->
                        []
                )
            ]
        ]


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


settingsView : Game -> List (Html Msg)
settingsView game =
    if game.settings.settingsOpen then
        [ div
            ([ style "background-color" "white"
             , style "width" "700px"
             , style "height" "600px"
             , style "position" "absolute"
             , style "z-index" "10"
             , style "overflow-y" "scroll"
             , style "overflow-x" "auto"
             , style "top" "60px"
             , style "border" ("5px solid " ++ sowedSeedColor)
             , style "border-radius" "10px"
             , style "opacity" "0.95"
             , centerText
             ]
                ++ defaultTextFont
            )
            [ Html.br [] []
            , Html.text (Localization.presentationSettings game.settings.language)
            , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                [ div
                    []
                    (checkBox
                        (SettingChanged UpsideDown)
                        (Localization.tabletModeTitle game.settings.language)
                        (Localization.tabletModeDescription game.settings.language)
                        game.settings.upsideDownEnabled
                    )
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.animationSpeedTitle game.settings.language) ]
                , Html.br [] []
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (Speed Slow))
                        (Localization.slowSpeed game.settings.language)
                        (game.settings.sowingSpeed == Slow)
                    , radioButton
                        (SettingChanged (Speed Normal))
                        (Localization.normalSpeed game.settings.language)
                        (game.settings.sowingSpeed == Normal)
                    , radioButton
                        (SettingChanged (Speed Fast))
                        (Localization.fastSpeed game.settings.language)
                        (game.settings.sowingSpeed == Fast)
                    ]
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.languageSetting game.settings.language) ]
                , Html.br [] []
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (LanguageSetting German))
                        (Localization.germanLanguage game.settings.language)
                        (game.settings.language == German)
                    , radioButton
                        (SettingChanged (LanguageSetting English))
                        (Localization.englishLanguage game.settings.language)
                        (game.settings.language == English)
                    ]
                ]
            , div [ style "height" "2px", style "width" "90%", style "margin" "0 auto", style "background-color" sowingSeedsColorString ] []
            , Html.br [] []
            , Html.text (Localization.gameModeSettings game.settings.language)
            , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                [ div [ style "font-size" "14px" ] [ Html.text (Localization.gameModeHint game.settings.language) ]
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.numberOfSeeds game.settings.language) ]
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (SeedNumber 3))
                        "3"
                        (game.settings.numberOfSeeds == 3)
                    , radioButton
                        (SettingChanged (SeedNumber 4))
                        "4"
                        (game.settings.numberOfSeeds == 4)
                    , radioButton
                        (SettingChanged (SeedNumber 5))
                        "5"
                        (game.settings.numberOfSeeds == 5)
                    , radioButton
                        (SettingChanged (SeedNumber 6))
                        "6"
                        (game.settings.numberOfSeeds == 6)
                    ]
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged LastSeedsBehaviour)
                        (Localization.lastSeedsTitle game.settings.language)
                        (Localization.lastSeedsDescription game.settings.language)
                        game.settings.lastSeedsForFinishingPlayer
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged SowOpponentsStore)
                        (Localization.opponentsStoreTitle game.settings.language)
                        (Localization.opponentsStoreDescription game.settings.language)
                        game.settings.sowInOpponentsStore
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged StartingPlayer)
                        (Localization.firstTurnTitle game.settings.language)
                        (Localization.firstTurnDescription game.settings.language)
                        game.settings.playerTwoStarting
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged OpponentOption)
                        (Localization.opponentLevelTitle game.settings.language)
                        (Localization.opponentDescription game.settings.language)
                        (not (game.settings.opponent == Real))
                    )
                , case game.settings.opponent of
                    Real ->
                        div [] []

                    Computer intelligence ->
                        div
                            spaceChildrenEvenly
                            [ radioButton
                                (SettingChanged (IntelligenceOption Low))
                                (Localization.easyDifficulty game.settings.language)
                                (intelligence == Low)
                            , radioButton
                                (SettingChanged (IntelligenceOption Medium))
                                (Localization.mediumDifficulty game.settings.language)
                                (intelligence == Medium)
                            , radioButton
                                (SettingChanged (IntelligenceOption High))
                                (Localization.highDifficulty game.settings.language)
                                (intelligence == High)
                            ]
                ]
            ]
        ]

    else
        []


radioButton : Msg -> String -> Bool -> Html Msg
radioButton onClickMsg label isChecked =
    div (onClick onClickMsg :: settingsChoiceStyle)
        [ Html.div
            [ style "display" "inline-block" ]
            [ (if isChecked then
                Material.Icons.Toggle.radio_button_checked

               else
                Material.Icons.Toggle.radio_button_unchecked
              )
                sowingSeedsColor
                20
            ]
        , div [ style "margin-left" "3px" ] [ Html.text label ]
        ]


checkBox : Msg -> String -> String -> Bool -> List (Html Msg)
checkBox onClickMsg title description isChecked =
    [ Html.div [] [ Html.text title ]
    , div
        (onClick onClickMsg :: settingsChoiceStyle)
        [ Html.div
            [ style "display" "inline"
            ]
            [ (if isChecked then
                Material.Icons.Toggle.check_box

               else
                Material.Icons.Toggle.check_box_outline_blank
              )
                sowingSeedsColor
                25
            ]
        , div [ style "margin-left" "3px" ] [ Html.text description ]
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


cursorStyle : Game -> Player -> Attribute Msg
cursorStyle game player =
    case game.state of
        Turn p ->
            if p == player then
                pointerCursor

            else
                style "cursor" "default"

        End _ ->
            style "cursor" "default"


pointerCursor : Attribute Msg
pointerCursor =
    style "cursor" "pointer"


upsideDown : Game -> Player -> Attribute Msg
upsideDown game player =
    if game.settings.upsideDownEnabled && player == Two then
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
    [ pointerCursor
    , style "font-size" "16px"
    , style "margin-top" "5px"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]



-- END Styles & Attributes
-- BEGIN constants


sowingSeedsColorString : String
sowingSeedsColorString =
    "#db004e"


sowingSeedsColor : Color.Color
sowingSeedsColor =
    Color.rgb255 219 0 78


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
