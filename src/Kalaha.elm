module Kalaha exposing (Model, Msg(..), Player(..), State(..), initalModel, main, togglePlayer, update, view)

import Browser
import Html exposing (Html)
import Platform.Sub


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
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        []


initalModel : Model
initalModel =
    { state = Turn One }


type State
    = Turn Player
    | End


type Player
    = One
    | Two


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        One ->
            Two

        Two ->
            One


type alias Model =
    { state : State
    }


type Msg
    = Click
    | Other
