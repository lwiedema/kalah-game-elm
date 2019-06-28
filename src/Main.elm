module Main exposing (main)

import Array
import Browser
import Color
import Datatypes exposing (Model, Msg(..), SettingOption(..))
import Game exposing (State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
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
    case model.board.sowingState of
        NotSowing ->
            case model.state of
                Turn player ->
                    case model.settings.opponent of
                        Real ->
                            Sub.none

                        Computer _ ->
                            if player == Two then
                                Time.every
                                    (Settings.speedInMilliseconds model.settings.sowingSpeed)
                                    (\_ -> ComputerHasTurn)

                            else
                                Sub.none

                End _ ->
                    Sub.none

        _ ->
            Time.every
                (Settings.speedInMilliseconds model.settings.sowingSpeed)
                (\_ -> NextSowingStep)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click player pos ->
            case model.state of
                Turn onTurn ->
                    -- check if click on house was legal
                    if onTurn == player && not (GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer model.board player) pos == 0) then
                        ( Game.startSowingSeeds model player pos, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextSowingStep ->
            ( Game.nextSowingStep model, Cmd.none )

        Restart ->
            ( restartGame model.settings, Cmd.none )

        OpenSettings ->
            let
                oldSettings =
                    model.settings
            in
            ( { model
                | settings =
                    { oldSettings
                        | settingsOpen = not model.settings.settingsOpen
                    }
              }
            , Cmd.none
            )

        SettingChanged option ->
            let
                oldSettings =
                    model.settings
            in
            case option of
                Speed speed ->
                    ( { model | settings = { oldSettings | sowingSpeed = speed } }, Cmd.none )

                LanguageSetting language ->
                    ( { model | settings = { oldSettings | language = language } }, Cmd.none )

                SeedNumber n ->
                    ( restartGame { oldSettings | numberOfSeeds = n }, Cmd.none )

                LastSeedsBehaviour ->
                    ( restartGame { oldSettings | lastSeedsForFinishingPlayer = not oldSettings.lastSeedsForFinishingPlayer }, Cmd.none )

                UpsideDown ->
                    ( { model
                        | settings =
                            { oldSettings
                                | upsideDownEnabled = not model.settings.upsideDownEnabled
                            }
                      }
                    , Cmd.none
                    )

                SowOpponentsStore ->
                    ( restartGame { oldSettings | sowInOpponentsStore = not oldSettings.sowInOpponentsStore }, Cmd.none )

                OpponentOption ->
                    ( restartGame (Settings.toggleOpponentOption oldSettings), Cmd.none )

                IntelligenceOption i ->
                    ( restartGame { oldSettings | opponent = Computer i }, Cmd.none )

                StartingPlayer ->
                    ( restartGame { oldSettings | playerTwoStarting = not oldSettings.playerTwoStarting }, Cmd.none )

        ComputerHasTurn ->
            ( model, Random.generate RandomMoveWeights (KalahaAI.weightMoves model.settings) )

        RandomMoveWeights weights ->
            ( Game.startSowingSeeds model Two (KalahaAI.nextMove model weights), Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "display" "inline-block" ]
            [ iconButton
                (Localization.settings model.settings.language)
                Material.Icons.Action.settings
                OpenSettings
            ]
        , div [ style "height" "100%", style "width" "100%" ]
            (div
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
                                        restartButton model
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
                :: settingsView model
            )
        ]


initalModel : Model
initalModel =
    restartGame Settings.defaultSettings


restartGame : Settings -> Model
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
        h =
            Array.get pos (GameBoard.getRowForPlayer model.board player)
    in
    case h of
        Just house ->
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
            [ Html.text (String.fromInt store.seeds) ]
        , div
            [ style "padding" "10px"
            , fillParentWidth
            ]
            (seedsInHouseView store.seeds store.justSown)
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
            (Localization.player model.settings.language
                ++ " "
                ++ Player.toString player
                ++ (case model.settings.opponent of
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
            (case model.state of
                Turn p ->
                    if p == player then
                        Localization.yourTurn model.settings.language

                    else
                        Localization.opponentsTurn model.settings.language (Localization.player model.settings.language ++ " " ++ Player.toString p)

                End winner ->
                    Localization.gameOver model.settings.language
                        ++ " "
                        ++ (case winner of
                                Drawn ->
                                    Localization.drawnGame model.settings.language

                                Winner w finalScore ->
                                    (if w == player then
                                        Localization.youWin model.settings.language

                                     else
                                        Localization.youLoose model.settings.language
                                    )
                                        ++ " "
                                        ++ Localization.finalScore model.settings.language
                                        ++ ": "
                                        ++ String.fromInt (Tuple.first finalScore)
                                        ++ ":"
                                        ++ String.fromInt (Tuple.second finalScore)
                           )
            )
        ]


restartButton : Model -> Html Msg
restartButton model =
    iconButton (Localization.restart model.settings.language) Material.Icons.Navigation.refresh Restart


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
                        List.repeat info.seedsToSow (seedView sowingSeedsColor)

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


settingsView : Model -> List (Html Msg)
settingsView model =
    if model.settings.settingsOpen then
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
            , Html.text (Localization.presentationSettings model.settings.language)
            , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                [ div
                    []
                    (checkBox
                        (SettingChanged UpsideDown)
                        (Localization.tabletModeTitle model.settings.language)
                        (Localization.tabletModeDescription model.settings.language)
                        model.settings.upsideDownEnabled
                    )
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.animationSpeedTitle model.settings.language) ]
                , Html.br [] []
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (Speed Slow))
                        (Localization.slowSpeed model.settings.language)
                        (model.settings.sowingSpeed == Slow)
                    , radioButton
                        (SettingChanged (Speed Normal))
                        (Localization.normalSpeed model.settings.language)
                        (model.settings.sowingSpeed == Normal)
                    , radioButton
                        (SettingChanged (Speed Fast))
                        (Localization.fastSpeed model.settings.language)
                        (model.settings.sowingSpeed == Fast)
                    ]
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.languageSetting model.settings.language) ]
                , Html.br [] []
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (LanguageSetting German))
                        (Localization.germanLanguage model.settings.language)
                        (model.settings.language == German)
                    , radioButton
                        (SettingChanged (LanguageSetting English))
                        (Localization.englishLanguage model.settings.language)
                        (model.settings.language == English)
                    ]
                ]
            , div [ style "height" "2px", style "width" "90%", style "margin" "0 auto", style "background-color" sowingSeedsColor ] []
            , Html.br [] []
            , Html.text (Localization.gameModeSettings model.settings.language)
            , Html.form [ style "margin" "10px", style "font-size" "18px" ]
                [ div [ style "font-size" "14px" ] [ Html.text (Localization.gameModeHint model.settings.language) ]
                , Html.br [] []
                , Html.label [] [ Html.text (Localization.numberOfSeeds model.settings.language) ]
                , div
                    spaceChildrenEvenly
                    [ radioButton
                        (SettingChanged (SeedNumber 3))
                        "3"
                        (model.settings.numberOfSeeds == 3)
                    , radioButton
                        (SettingChanged (SeedNumber 4))
                        "4"
                        (model.settings.numberOfSeeds == 4)
                    , radioButton
                        (SettingChanged (SeedNumber 5))
                        "5"
                        (model.settings.numberOfSeeds == 5)
                    , radioButton
                        (SettingChanged (SeedNumber 6))
                        "6"
                        (model.settings.numberOfSeeds == 6)
                    ]
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged LastSeedsBehaviour)
                        (Localization.lastSeedsTitle model.settings.language)
                        (Localization.lastSeedsDescription model.settings.language)
                        model.settings.lastSeedsForFinishingPlayer
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged SowOpponentsStore)
                        (Localization.opponentsStoreTitle model.settings.language)
                        (Localization.opponentsStoreDescription model.settings.language)
                        model.settings.sowInOpponentsStore
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged StartingPlayer)
                        (Localization.firstTurnTitle model.settings.language)
                        (Localization.firstTurnDescription model.settings.language)
                        model.settings.playerTwoStarting
                    )
                , Html.br [] []
                , div
                    []
                    (checkBox
                        (SettingChanged OpponentOption)
                        (Localization.opponentLevelTitle model.settings.language)
                        (Localization.opponentDescription model.settings.language)
                        (not (model.settings.opponent == Real))
                    )
                , case model.settings.opponent of
                    Real ->
                        div [] []

                    Computer intelligence ->
                        div
                            spaceChildrenEvenly
                            [ radioButton
                                (SettingChanged (IntelligenceOption Low))
                                (Localization.easyDifficulty model.settings.language)
                                (intelligence == Low)
                            , radioButton
                                (SettingChanged (IntelligenceOption Medium))
                                (Localization.mediumDifficulty model.settings.language)
                                (intelligence == Medium)
                            , radioButton
                                (SettingChanged (IntelligenceOption High))
                                (Localization.highDifficulty model.settings.language)
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
                sowingSeedsColorColor
                20
            ]
        , Html.text label
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
                sowingSeedsColorColor
                25
            ]
        , div [ style "display" "inline" ] [ Html.text description ]
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
            if p == player then
                pointerCursor

            else
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


sowingSeedsColorColor : Color.Color
sowingSeedsColorColor =
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
