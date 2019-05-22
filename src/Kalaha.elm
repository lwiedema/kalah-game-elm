module Kalaha exposing (Model, Msg(..), State(..), initalModel, main, update, view)

import Browser
import GameBoard exposing (GameBoard, Player(..), getRowForPlayer, numberOfSeedsInHouse, startSowingSeeds)
import Html exposing (Html, button, div)
import Html.Attributes exposing (height, start, style, width)
import Html.Events exposing (onClick)
import Platform.Sub
import String exposing (fromInt)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initalModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click player int ->
            ( { model
                | board = startSowingSeeds model.board player int
              }
            , Cmd.none
            )

        Other ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
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
                    [ button [ onClick (Click Two 5) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 5)) ]
                    , button [ onClick (Click Two 4) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 4)) ]
                    , button [ onClick (Click Two 3) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 3)) ]
                    , button [ onClick (Click Two 2) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 2)) ]
                    , button [ onClick (Click Two 1) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 1)) ]
                    , button [ onClick (Click Two 0) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.second model.board.rows) 0)) ]
                    ]
                , div
                    [ style "background-color" "yellow"
                    , style "width" "100%"
                    , style "height" "45%"
                    , style "display" "inline-block"
                    ]
                    [ button [ onClick (Click One 0) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 0)) ]
                    , button [ onClick (Click One 1) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 1)) ]
                    , button [ onClick (Click One 2) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 2)) ]
                    , button [ onClick (Click One 3) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 3)) ]
                    , button [ onClick (Click One 4) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 4)) ]
                    , button [ onClick (Click One 5) ] [ Html.text (fromInt (numberOfSeedsInHouse (Tuple.first model.board.rows) 5)) ]
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


initalModel : Model
initalModel =
    { state = Turn One, board = GameBoard.initalBoard }


type State
    = Turn Player
    | End


type alias Model =
    { state : State
    , board : GameBoard
    }


type Msg
    = Click Player Int
    | Other
