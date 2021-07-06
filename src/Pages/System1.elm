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
import Html.Attributes exposing (class, id)
import Html exposing (h2)
import Html exposing (text)
import Html exposing (br)
import Html exposing (p, button)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Util exposing (get_array2)
import Html.Attributes exposing (step)

instructions : Array String
instructions = Array.fromList ["Select a number from each array", "Click multiply to multiply selected numbers", "Click add to add product to output"]

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
a2 = get_array2
ans : Int
ans = dot_product a1 a2

type alias Model =
    { out : Int
    , prod : Int
    , s1 : Int
    , s2 : Int
    , step : Int
    }


init : ( Model, Effect Msg )
init =
    ( { out = 0, prod = 0, s1 = -1, s2 = -1, step = 0}, Effect.none )



-- UPDATE


type Msg
    = ChangeSelected Int Int | Multiply | Add


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeSelected an i ->
            if an == 1 then
                if model.s2 /= -1 then
                    ({model | s1 = i, step = 1}, Effect.none)
                else
                    ( {model | s1 = i, step = 0}, Effect.none )
            else
                if model.s1 /= -1 then
                    ({model | s2 = i, step = 1}, Effect.none)
                else
                    ({model | s2 = i, step = 0}, Effect.none)
        Multiply ->
            if model.step == 1 then
                let
                    temp = single_multiply a1 model.s1 a2 model.s2
                in
                ({model | prod = temp, s1 = -1, s2 = -1, step = 2}, Effect.none)
            else
                (model, Effect.none)
        Add ->
            if model.step == 2 then
                let
                    temp = model.out + model.prod
                in
                ({model | out = temp, prod = 0, s1 = -1 , s2 = -1, step = 0 }, Effect.none)
            else
                (model , Effect.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        make_bubble : Int -> Int -> Int -> Int -> Html.Html Msg
        make_bubble selected num a_n ind = 
            if selected == ind then
                button [ class "bubble" ,id "selected-bubble", onClick (ChangeSelected a_n ind )]
                    [text (String.fromInt num) ]
            else
                button [ class "bubble", onClick (ChangeSelected a_n ind ) ]
                    [text (String.fromInt num) ]
        
        l1 = Array.indexedMap (\i x -> make_bubble model.s1 x 1 i) a1
        l2 = Array.indexedMap (\i x -> make_bubble model.s2 x 2 i) a2

    in
    
    { title = "System1"
    , body = UI.layout 1 
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
                        p [] [text "Finding the Dot Product of two arrays by multiplying two elements from each and adding them to output"]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Experiment Setup"],
                        p [] [text "It consists two list of numbers. You can select two numbers one from each array. On selecting the colors of those numbers will be changed."],
                        p [] [text "You have multiply button. On clicking it, the selected elements are multiplied and shown in the product box. You can click add to add that product to output."]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Procedure"],
                        p [] [text "Steps to be followed:"],
                        p [] [text "Step 1: Select two items randomly one from each array randomly."],
                        p [] [text "Step 2: Click on multiply"],
                        p [] [text "Step 3: Decide whether you like to add that product to output. If you like to, then click add. Check you get the right answer or not. And repeat the procedure from step 1."]
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
                    div []
                        (text "Array 2" :: Array.toList l2),
                    p [] [
                        text "Product = ",
                        text (String.fromInt model.prod), 
                        text " Output = ",
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
