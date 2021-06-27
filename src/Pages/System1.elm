module Pages.System1 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System1 exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page
import UI
import Html
import Array
import Array exposing (fromList)
import Util exposing (get_array1)
import Util exposing (dot_product)
import Util exposing (single_multiply)
import Html exposing (div)
import Html.Attributes exposing (class)
import Html exposing (h2)
import Html exposing (text)
import Html exposing (br)
import Html exposing (p)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT
a1: Array.Array Int
a1 = get_array1
a2: Array.Array Int
a2 = get_array1
ans : Int
ans = dot_product a1 a2

type alias Model =
    { out : Int
    , prod : Int
    , s1 : Int
    , s2 : Int
    }


init : ( Model, Effect Msg )
init =
    ( { out = 0, prod = 0, s1 = -1, s2 = -1}, Effect.none )



-- UPDATE


type Msg
    = ChangeSelected Int Int | Multiply | Add


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeSelected an i ->
            if an == 1 then 
                ( {model | s1 = i}, Effect.none )
            else
                ({model | s2 = i}, Effect.none)
        Multiply ->
            if model.s1 /= -1 then
                if model.s2 /= -1 then
                    let
                        temp = single_multiply a1 model.s1 a2 model.s2
                    in
                    ({model | prod = temp, s1 = -1, s2 = -1}, Effect.none)
                else
                    (model, Effect.none)
            else
                (model, Effect.none)
        Add ->
            let
                temp = model.out + model.prod
            in
            ({model | out = temp, prod = 0 }, Effect.none)
            

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "System1"
    , body = UI.layout 1 
    [
        div [class "system"]
        [
            div [class "info"]
            [
                h2 [] [text "Objective"],
                p [] [text "Finding the Dot Product of two arrays by multiplying two elements from each and adding them to output"],
                h2 [] [text "Experiment Setup"],
                p [] [text "It consists two list of numbers. You can select two numbers one from each array. On selecting the colors of those numbers will be changed."],
                br [] [],
                p [] [text "You have multiply button. On clicking it, the selected elements are multiplied and shown in the product box. You can click add to add that product to output."],
                h2 [] [text "Procedure"],
                p [] [text "Steps to be followed:"],
                p [] [text "Step 1: Select two items randomly one from each array randomly."],
                p [] [text "Step 2: Click on multiply"],
                p [] [text "Step 3: Decide whether you like to add that product to output. If you like to, then click add. Check you get the right answer or not. And repeat the procedure from step 1."]
            ],
            div [class "exp"]
            [
                p [] [
                    text "ans = ",
                    text (String.fromInt ans)
                ]
            ]
        ]
    ]
    }
