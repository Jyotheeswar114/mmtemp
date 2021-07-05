module Pages.System2 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System2 exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page
import UI
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Array
import Util
-- import Array exposing (Array)

instructions : Array.Array String
instructions = Array.fromList ["Select a number from any array", "Click multiply to multiply selected numbers", "Click add to add product to output"]

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
    , prod : Int
    , s : Int
    , products : Array.Array Int
    , step : Int
    }


init : ( Model, Effect Msg )
init =
    ( { out = 0, prod = 0, s = -1, products = Array.fromList [0, 0, 0, 0], step = 0}, Effect.none )



-- UPDATE

type Msg
    = ChangeSelected Int | Multiply | Add


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeSelected i ->
            ( { model | s = i, step = 1}, Effect.none )
        Multiply ->
            if model.step == 1 then
                let
                    temp = Util.single_multiply a1 model.s a2 model.s
                in
                ({model | s = -1, products = (Array.set model.s temp model.products), prod = temp, step = 2}, Effect.none)
                
            else
                (model, Effect.none)
        Add -> 
            if model.step == 2 then
                let
                    temp = model.out + model.prod
                in
                ({model | out = temp, prod = 0, step = 0 }, Effect.none)
            else
                (model, Effect.none)





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        make_bubble : Int -> Int -> Int -> Html.Html Msg
        make_bubble selected num ind = 
            if selected == ind then
                button [ class "bubble" ,id "selected-bubble", onClick (ChangeSelected ind )]
                    [text (String.fromInt num) ]
            else
                button [ class "bubble", onClick (ChangeSelected ind ) ]
                    [text (String.fromInt num) ]
        
        l1 = Array.indexedMap (\i x -> make_bubble model.s x i) a1
        l2 = Array.indexedMap (\i x -> make_bubble model.s x i) a2

    in
    { title = "System2"
    , body = UI.layout 2 
    [
        div [class "system"]
        [
            div [class "info"]
            [
                div [class "head-p"]
                [
                    h2 [] [text "Objective"],
                    p [] [text "Finding the Dot Product of two arrays by multiplying two elements having same indices from each array and adding them to output"]
                ],
                div [class "head-p"]
                [
                    h2 [] [text "Experiment Setup"],
                    p [] [text "It consists two list of numbers. You can select a number having index i from a array. Then the number having same index i in another array is also selected automatically. On selecting the colors of those numbers will be changed. "],
                    p [] [text "You have multiply button. On clicking it, the selected elements are multiplied and shown in the product box. You can click add to add that product to output."]
                ],
                div [class "head-p"]
                [
                    h2 [] [text "Procedure"],
                    p [] [text "Steps to be followed:"],
                    p [] [text "Step 1: Select a number from any array."],
                    p [] [text "Step 2: Click on multiply"],
                    p [] [text "Step 3: Decide whether you like to add that product to output. If you like to, then click add. Check you get the right answer or not. And repeat the procedure from step 1."]
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
                        text "product = ",
                        text (String.fromInt model.prod), 
                        text " output = ",
                        text (String.fromInt model.out)
                    ],
                    div [class "actions-space"]
                    [
                        button [class "secondary-button bottom-button",
                        onClick Multiply]
                        [text "Multiply"],
                        button [class "primary-button bottom-button",
                        onClick Add]
                        [text "Add"]
                    ]
                ]
            ]
        ]
    ]
    }
