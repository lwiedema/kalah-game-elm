module KalahaMain exposing (main)

import Browser
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (attribute, disabled, start, style)
import Html.Events exposing (onClick)
import Lists
import Platform.Sub
import Player exposing (Player(..), Winner(..))
import Settings
import String exposing (fromInt)
import Svg exposing (symbol)
import Svg.Attributes
import Time



-- BEGIN datatypes


type alias Model =
    Game


type Msg
    = Click Player Int
    | NextSowingStep
    | Other


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

        Other ->
            model


view : Model -> Html Msg
view model =
    div
        [ style "width" "940px"
        ]
        [ infoView model Two
        , div
            boardStyle
            [ div
                (upsideDown model Two :: storeStyle)
                [ storeView model Two ]
            , div
                [ style "width" "580px"
                , style "height" "100%"
                , style "text-align" "center"
                , style "position" "relative"
                , style "float" "left"
                ]
                [ div
                    [ style "height" "100%" ]
                    [ div
                        rowStyle
                        (rowView model Two)
                    , div
                        [ style "height" "60px"
                        , style "border" "10px solid white"
                        , style "border-radius" "10px"
                        , style "background-color" "white"
                        ]
                        [ sowingView model
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



-- END web-app
-- BEGIN views


rowView : Model -> Player -> List (Html Msg)
rowView model player =
    (if player == Two then
        List.foldl (::) []

     else
        identity
    )
        (rowViewHelper
            model
            player
            model.settings.numberOfHouses
        )


rowViewHelper : Model -> Player -> Int -> List (Html Msg)
rowViewHelper model player housesToCreate =
    case housesToCreate of
        1 ->
            houseView model player (model.settings.numberOfHouses - housesToCreate) :: []

        _ ->
            houseView model player (model.settings.numberOfHouses - housesToCreate)
                :: rowViewHelper model player (housesToCreate - 1)


houseView : Model -> Player -> Int -> Html Msg
houseView model player pos =
    let
        house =
            Lists.elementAtWithDefault
                (GameBoard.getRowForPlayer model.board player)
                pos
                { justSownTo = False, seeds = 0 }
    in
    div
        (upsideDown model player
            :: (case model.state of
                    Turn p ->
                        [ cursorStyle (p == player) ]

                    End _ ->
                        []
               )
            ++ [ onClick (Click player pos)
               , style "width" "72px"
               , style "height" "100%"
               , style "float" "left"
               ]
        )
        [ Html.text (fromInt house.seeds)
        , div
            [ style "width" "100%"
            ]
            (seedsInHouseView house.seeds house.justSownTo)
        ]


seedsInHouseView : Int -> Bool -> List (Html Msg)
seedsInHouseView numOfSeeds justSownTo =
    case numOfSeeds of
        0 ->
            []

        _ ->
            case justSownTo of
                True ->
                    List.repeat (numOfSeeds - 1) (seedView "black") ++ [ seedView "red" ]

                False ->
                    List.repeat numOfSeeds (seedView "black")


storeView : Model -> Player -> Html Msg
storeView model player =
    let
        store =
            GameBoard.getStoreForPlayer model.board player
    in
    div
        [ style "text-align" "center"
        , style "margin" "10px"
        , style "width" "140px"
        ]
        [ div [ style "width" "100%" ] [ Html.text (fromInt store.seeds) ]
        , div
            [ style "padding" "10px"
            , style "width" "100%"
            ]
            (seedsInHouseView store.seeds store.justSownTo)
        ]


infoView : Model -> Player -> Html Msg
infoView model player =
    div
        (upsideDown model player
            :: [ style "width" "100%"
               , style "text-align" "center"
               ]
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


sowingView : Model -> Html Msg
sowingView model =
    div
        [ style "padding" "17px 0 15px 0"
        ]
        [ div
            [ style "width" "100%"
            , style "display" "flex"
            , style "justify-content" "space-evenly"
            ]
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
    List.repeat numOfSeeds (seedView "red")


seedView : String -> Html Msg
seedView color =
    div [ style "padding" "2px", style "float" "left" ]
        [ Svg.svg [ Svg.Attributes.viewBox ("0 0 " ++ seedSizeString ++ " " ++ seedSizeString), Svg.Attributes.width seedSizeString, Svg.Attributes.height seedSizeString, Svg.Attributes.fill color ]
            [ Svg.circle [ Svg.Attributes.cx seedRadiusString, Svg.Attributes.cy seedRadiusString, Svg.Attributes.r seedRadiusString ] []
            ]
        ]



-- END views
-- BEGIN styles & attributes


storeStyle : List (Attribute Msg)
storeStyle =
    [ style "height" "100%"
    , style "float" "left"
    ]


rowStyle : List (Attribute Msg)
rowStyle =
    [ style "width" "580px"
    , style "padding" "10px 0"
    , style "height" "140px"
    , style "display" "flex"
    , style "justify-content" "space-evenly"
    ]


upsideDown : Model -> Player -> Attribute Msg
upsideDown model player =
    if model.settings.upsideDownEnabled && player == Two then
        style "transform" "rotate(180deg)"

    else
        style "" ""


boardStyle : List (Attribute Msg)
boardStyle =
    let
        boardColor =
            "#939393"

        --"#4a4a4a"
    in
    [ style "border" ("20px solid " ++ boardColor)
    , style "background-color" boardColor
    , style "border-radius" "40px"
    , style "height" "400px"
    ]


cursorStyle : Bool -> Attribute Msg
cursorStyle possible =
    case possible of
        True ->
            style "cursor" "pointer"

        False ->
            style "cursor" "default"



-- END Styles & Attributes


seedSize : Int
seedSize =
    20


seedRadiusString : String
seedRadiusString =
    String.fromInt (seedSize // 2)


seedSizeString : String
seedSizeString =
    String.fromInt seedSize
