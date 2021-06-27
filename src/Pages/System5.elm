module Pages.System5 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System5 exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page
import UI
import Html

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view model =
    { title = "System5"
    , body = UI.layout 5 [ Html.text "Hello, world! Eswar" ]
    }