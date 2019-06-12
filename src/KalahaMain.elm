module KalahaMain exposing (main)

import Browser
import Game exposing (Game, State(..))
import GameBoard exposing (SowingState(..))
import Html exposing (Attribute, Html, button, div, q, text)
import Html.Attributes exposing (attribute, disabled, start, style)
import Html.Events exposing (onClick)
import Lists
import Platform.Sub
import Player exposing (Player(..), Winner(..))
import Settings
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
                    [ style "height" "100%" ]
                    [ div
                        (upsideDown :: rowStyle)
                        (rowView model Two)
                    , div
                        [ style "height" "20%"
                        , style "margin-top" "-4px"
                        ]
                        [ infoView model
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
        ]



-- END web-app
-- BEGIN views


rowView : Model -> Player -> List (Html Msg)
rowView model player =
    rowViewHelper model player model.settings.numberOfHouses


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
        ((case model.state of
            Turn p ->
                [ clickPossible (p == player) ]

            End _ ->
                []
         )
            ++ [ onClick (Click player pos)
               , style "width" (String.fromFloat (100 / toFloat model.settings.numberOfHouses) ++ "%")
               , style "height" "100%"
               , style "float" "left"
               ]
        )
        [ Html.text (fromInt house.seeds)
        , div [] (seedsInHouseView house.seeds house.justSownTo)
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
    div []
        [ Html.text (fromInt store.seeds)
        , div [] (seedsInHouseView store.seeds store.justSownTo)
        ]


infoView : Model -> Html Msg
infoView model =
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

                                Winner w finalScore ->
                                    "Es gewinnt Spieler "
                                        ++ Player.toString w
                                        ++ ". Endstand: "
                                        ++ String.fromInt (Tuple.first finalScore)
                                        ++ ":"
                                        ++ String.fromInt (Tuple.second finalScore)
                           )
            )
        , div
            [ style "width" "100%"
            , style "display" "flex"
            , style "justify-content" "space-evenly"
            ]
            [ sowingView model ]
        ]


sowingView : Model -> Html Msg
sowingView model =
    div []
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


clickPossible : Bool -> Attribute Msg
clickPossible possible =
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
