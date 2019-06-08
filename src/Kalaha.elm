module Kalaha exposing (Model, Msg(..), initalModel, main, update, view)

import Browser
import GameBoard exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, height, start, style, width)
import Html.Events exposing (onClick)
import Platform.Sub
import String exposing (fromInt)
import Time


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
        Sowing _ ->
            Time.every 1000 (\_ -> SowNext)

        SowingFinished _ ->
            Time.every 1000 (\_ -> AfterSow)

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click player int ->
            ( startSowingSeeds model player int
            , Cmd.none
            )

        SowNext ->
            ( sowNextSeed model, Cmd.none )

        AfterSow ->
            ( sowNextSeed model, Cmd.none )

        Other ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ Html.text
            (case model.state of
                Turn p ->
                    case p of
                        One ->
                            "Spieler 1 ist am Zug"

                        Two ->
                            "Spieler 2 ist am Zug"

                End winner ->
                    "Spiel beendet. "
                        ++ (case winner of
                                Drawn ->
                                    "Es ist unentschieden."

                                Winner One ->
                                    "Es gewinnt Spieler 1."

                                Winner Two ->
                                    "Es gewinnt Spieler 2."
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
                        [ button [ onClick (Click Two 5), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 5)) ]
                        , button [ onClick (Click Two 4), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 4)) ]
                        , button [ onClick (Click Two 3), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 3)) ]
                        , button [ onClick (Click Two 2), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 2)) ]
                        , button [ onClick (Click Two 1), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 1)) ]
                        , button [ onClick (Click Two 0), disabled (not (model.state == Turn Two)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 0)) ]
                        ]
                    , div
                        [ style "background-color" "yellow"
                        , style "width" "100%"
                        , style "height" "45%"
                        , style "display" "inline-block"
                        ]
                        [ button [ onClick (Click One 0), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 0)) ]
                        , button [ onClick (Click One 1), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 1)) ]
                        , button [ onClick (Click One 2), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 2)) ]
                        , button [ onClick (Click One 3), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 3)) ]
                        , button [ onClick (Click One 4), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 4)) ]
                        , button [ onClick (Click One 5), disabled (not (model.state == Turn One)) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 5)) ]
                        ]
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


initalModel : Model
initalModel =
    { state = Turn One, board = GameBoard.initalBoard }


type alias Model =
    Game


type Msg
    = Click Player Int
    | SowNext
    | AfterSow
    | Other
