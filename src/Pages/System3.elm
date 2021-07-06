module Pages.System3 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System3 exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page
import UI
import Html
import Array
import Util
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)

instructions : Array.Array String
instructions = Array.fromList ["Click next", "You have found the required dot product"]

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
a1 = Util.get_array1
a2: Array.Array Int
a2 = Util.get_array2
prods : Array.Array Int
prods = Util.get_products a1 a2

ans : Int
ans = Util.array_sum prods

type alias Model =
    { out : Int
    , s : Int
    , products : Array.Array Int
    , step : Int
    }


init : ( Model, Effect Msg )
init =
    ( { out = 0, s = 0, products = Array.fromList [0, 0, 0, 0], step = 0}, Effect.none )



-- UPDATE


type Msg
    = Next


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Next ->
             if model.s < 4 then
                let
                    help = Util.single_multiply a1 model.s a2 model.s
                    step_tmp = if help + model.out == ans then 1 else 0
                in

                ({model | out = model.out + help, s = model.s + 1, products = Array.set model.s help model.products, step = step_tmp}, Effect.none)
             else
                (model , Effect.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view model =
    let
        make_bubble : Int -> Int -> Int -> Html.Html Msg
        make_bubble selected num ind = 
            if selected == ind then
                button [ class "bubble" ,id "selected-bubble"]
                    [text (String.fromInt num) ]
            else
                button [ class "bubble"]
                    [text (String.fromInt num) ]
        
        l1 = Array.indexedMap (\i x -> make_bubble model.s x i) a1
        l2 = Array.indexedMap (\i x -> make_bubble model.s x i) a2
    in 
    
    { title = "System3"
    , body = UI.layout 3 
    [
        div [class "system"]
        [
            div [class "info"]
            [
                div [class "info-inner"]
                [
                    div [class "head-p"]
                    [
                        h2 [] [text "Objective"],
                        p [] [text "Finding the Dot Product of two arrays by multiplying two elements having same indices sequentially from each array and adding them to output"]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Experiment Setup"],
                        p [] [text "It consists two list of numbers. Intially 0 index is selected. On clicking next, the elements having selected index multiplied and added to output. Then the selected index is incremented."]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Procedure"],
                        p [] [text "Steps to be followed:"],
                        p [] [text "Step 1: Click the next button"],
                        p [] [text "Step 2: Check you get the right answer or not. And repeat the procedure from step 1."]
                    ]
                ]
            ],
            div [class "exp"]
            [
                div [class "expInner"] 
                [   
                    p [] [
                        text (Maybe.withDefault "" (Array.get model.step instructions))
                    ],
                    p [] [
                        text "ans = ",
                        text (String.fromInt ans)
                    ],
                    div []
                        (text "Array 1" :: Array.toList l1),
                    UI.mul_row "x",
                    div []
                        (text "Array 2" :: Array.toList l2),
                    UI.mul_row "=",
                    UI.sum_bub "Products" model.products,
                    p [] [
                        text " output = ",
                        text (String.fromInt model.out)
                    ],
                    div [class "actions-space"]
                    [
                        button [class "primary-button bottom-button",
                        onClick Next]
                        [text "Next"]
                    ]
                ]
            ]
        ]
    ]
    }
