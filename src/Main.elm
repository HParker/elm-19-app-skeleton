module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Url exposing (Url)


type alias Model =
    { text : String
    }


type alias Flags =
    {}


type Msg
    = None
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model "hi there!", Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Example"
    , body = [ text model.text ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
