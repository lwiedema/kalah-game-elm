module KalahaMain exposing (Model, Msg(..), initalModel, main, subscriptions, update, view)

import Browser
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (attribute, disabled, height, start, style, width)
import Html.Events exposing (onClick)
import Lists
import Platform.Sub
import Player exposing (Player(..), Winner(..))
import Settings
import String exposing (fromInt)
import Time


type alias Model =
    Game


initalModel : Model
initalModel =
    { state = Turn One
    , board = GameBoard.initalBoard
    , settings = Settings.defaultSettings
    }


type Msg
    = Click Player Int
    | NextSowingStep
    | Other


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
            Time.every 500 (\_ -> NextSowingStep)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click player int ->
            case model.state of
                Turn onTurn ->
                    if onTurn == player then
                        Game.startSowingSeeds model player int

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
    div []
        [ div
            [ style "width" "900px"
            , style "height" "400px"
            , grayBackground
            ]
            [ div
                (upsideDown :: storeStyle)
                [ storeView model Two ]
            , div
                [ style "width" "60%"
                , style "height" "100%"
                , style "text-align" "center"
                , style "position" "relative"
                , style "float" "left"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "top" "5px"
                    , style "right" "5px"
                    , style "bottom" "5px"
                    , style "left" "5px"
                    ]
                    [ div
                        (upsideDown :: rowStyle)
                        (rowView model Two)
                    , div [ style "height" "20%" ]
                        [ infoView model ]
                    , div
                        rowStyle
                        (rowView model One)
                    ]
                ]
            , div
                storeStyle
                [ storeView model One ]
            ]
        ]


rowView : Model -> Player -> List (Html Msg)
rowView model player =
    List.foldl (::) [] (rowViewHelper model player model.settings.numberOfHouses)


rowViewHelper : Model -> Player -> Int -> List (Html Msg)
rowViewHelper model player rowsToCreate =
    case rowsToCreate of
        1 ->
            houseView model player (model.settings.numberOfHouses - rowsToCreate) :: []

        _ ->
            rowViewHelper model player (rowsToCreate - 1) ++ [ houseView model player (model.settings.numberOfHouses - rowsToCreate) ]


houseView : Model -> Player -> Int -> Html Msg
houseView model player pos =
    let
        house =
            Lists.elementAtWithDefault (GameBoard.getRowForPlayer model.board player) pos { justSownTo = False, seeds = 0 }
    in
    button
        ([ onClick (Click player pos)
         , disabled (not (model.state == Turn player))
         ]
            ++ sownStyle house.justSownTo
        )
        [ Html.text (fromInt house.seeds) ]


sownStyle : Bool -> List (Attribute Msg)
sownStyle justSown =
    if justSown then
        [ style "background-color" "red" ]

    else
        []


storeView : Model -> Player -> Html Msg
storeView model player =
    let
        store =
            GameBoard.getStoreForPlayer model.board player
    in
    div (sownStyle store.justSownTo)
        [ Html.text (fromInt store.seeds)
        ]


infoView : Model -> Html Msg
infoView model =
    Html.text
        (case model.state of
            Turn p ->
                "Spieler " ++ Player.toString p ++ " ist am Zug."

            End winner ->
                "Spiel beendet. "
                    ++ (case winner of
                            Drawn ->
                                "Es ist unentschieden."

                            Winner w ->
                                "Es gewinnt Spieler " ++ Player.toString w ++ "."
                       )
        )



-- Attributes


storeStyle : List (Attribute Msg)
storeStyle =
    [ style "width" "20%"
    , style "height" "100%"
    , style "float" "left"
    ]


rowStyle : List (Attribute Msg)
rowStyle =
    [ style "background-color" "yellow"
    , style "width" "100%"
    , style "height" "40%"
    , style "display" "inline-block"
    ]


upsideDown : Attribute Msg
upsideDown =
    style "transform" "rotate(180deg)"


grayBackground : Attribute Msg
grayBackground =
    style "background-color" "#4a4a4a"
