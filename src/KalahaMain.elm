module KalahaMain exposing (Model, Msg(..), initalModel, main, subscriptions, update, view)

import Browser
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, height, start, style, width)
import Html.Events exposing (onClick)
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
        [ Html.text
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
        , div
            [ style "display" "flex"
            ]
            [ div
                [ style "width" "200px"
                , style "height" "400px"
                , style "background-color" "red"
                ]
                [ Html.text (fromInt (Tuple.second model.board.stores)) ]
            , div
                [ style "width" "800px"
                , style "height" "400px"
                , style "background-color" "blue"
                , style "text-align" "center"
                , style "position" "relative"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "top" "5px"
                    , style "right" "5px"
                    , style "bottom" "5px"
                    , style "left" "5px"
                    ]
                    [ div
                        [ style "background-color" "yellow"
                        , style "width" "100%"
                        , style "height" "45%"
                        , style "display" "inline-block"
                        ]
                        (rowView model Two)
                    , div
                        [ style "background-color" "yellow"
                        , style "width" "100%"
                        , style "height" "45%"
                        , style "display" "inline-block"
                        ]
                        (rowView model One)
                    ]
                ]
            , div
                [ style "width" "200px"
                , style "height" "400px"
                , style "background-color" "red"
                ]
                [ Html.text (fromInt (Tuple.first model.board.stores)) ]
            ]
        ]


rowView : Model -> Player -> List (Html Msg)
rowView model player =
    case player of
        One ->
            List.foldl (::) [] (rowViewHelper model player model.settings.numberOfHouses)

        Two ->
            rowViewHelper model player model.settings.numberOfHouses


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
        numberOfSeeds =
            GameBoard.numberOfSeedsInHouse (GameBoard.getRowForPlayer model.board player) pos
    in
    button [ onClick (Click player pos), disabled (not (model.state == Turn player)) ] [ Html.text (fromInt numberOfSeeds) ]
