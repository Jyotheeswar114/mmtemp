module Pages.System6 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System6 exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page
import UI
import Html
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Util
import Array

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT

a1 : Array.Array (Array.Array Int)
a1 = Util.get_m1
a2 : Array.Array (Array.Array Int)
a2 = Util.get_m2
res : Array.Array (Array.Array Int)
res = Util.multiply_matrices a1 a2

type alias Model =
    {
        s1 : Int,
        s2 : Int,
        output : Array.Array (Array.Array Int),
        reviel_answer : Bool,
        product : Int,
        prods : Array.Array Int
    }


init : ( Model, Effect Msg )
init =
    let
        temp = Util.row_column_prods a1 0 a2 0
    in
    
    ( {
        s1 = 0,
        s2 = 0,
        output = Util.get_2d_zeroes 4 4,
        reviel_answer = False,
        product = Util.array_sum temp,
        prods = temp
    }, Effect.none )



-- UPDATE


type Msg
    = Next
    | RevielChange


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Next ->
            let
                s1_ = modBy 4 (model.s1 + (model.s2 + 1)//4)
                s2_ = modBy 4 (model.s2 + 1)
                temp_prods = Util.row_column_prods a1 s1_ a2 s2_ 
            in
            
            ( {model | output = (Util.matrix_value_set model.output (Util.array_sum model.prods )) model.s1 model.s2, prods = temp_prods,s1 = s1_, s2 = s2_, product = Util.array_sum temp_prods}, Effect.none )
        RevielChange ->
            ({model | reviel_answer = not model.reviel_answer}, Effect.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

type MatrixType
    = AnsMatrix
    | OutMatrix Bool
    | Normal Bool

view : Model -> View Msg
view model =
    let
        make_bubble : Int -> Html.Html Msg
        make_bubble num = 
            if num == -1 then 
                button [ class "bubble" ]
                    []
            else
                button [ class "bubble" ]
                    [text (String.fromInt num) ]
        make_square : MatrixType -> Int -> Html Msg
        make_square m_type num =
            case m_type of
                AnsMatrix ->
                    button [class "square matrix_square"]
                    [ text (String.fromInt num)]
                OutMatrix selected ->
                    if selected == True then
                        button [class "square matrix_square m_selected"]
                        [ text (String.fromInt num)]
                    else
                        button [class "square matrix_square"]
                        [ text (String.fromInt num)]
                Normal selected ->
                    if selected == True then
                        button [class "square selected-square"]
                        [ text (String.fromInt num)]
                    else
                        button [class "square"]
                        [ text (String.fromInt num)]
        l1 = List.map (\x -> div [class "matrix_row"] (List.map (\y -> make_square (Normal (x == model.s1)) (Util.get_matrix_element x y a1)) [0, 1, 2, 3])) [0,1,2,3]
        l2 = List.map (\x -> div [class "matrix_col"] (List.map (\y -> make_square (Normal (x == model.s2)) (Util.get_matrix_element x y a2)) [0, 1, 2, 3])) [0,1,2,3]
        l_out = List.map (\x -> div [] (List.map (\y -> make_square (OutMatrix (x == model.s1 && y ==  model.s2 ) ) (Util.get_matrix_element x y model.output)) [0, 1, 2, 3])) [0,1,2,3]
        l_res = List.map (\x -> div [] (List.map (\y -> make_square AnsMatrix (Util.get_matrix_element x y res)) [0, 1, 2, 3])) [0,1,2,3]
        v1 = Array.map (\x -> make_bubble x) (Maybe.withDefault (Array.repeat 4 -1) (Array.get model.s1 a1))
        v2 = Array.map (\x -> make_bubble x) (Maybe.withDefault (Array.repeat 4 -1) (Array.get model.s2 a2))
        v_sum = Array.map (\x -> make_bubble x) model.prods
        reviel =
            if model.reviel_answer == True then
                button [class "howle-button bottom-button", onClick RevielChange]
                [ text "Hide answer"]
            else
                button [class "howle-button bottom-button", onClick RevielChange]
                [ text "Reviel answer"]

    in
    
    { title = "System6"
    , body = UI.layout 6 
    [
        div [ class "system"]
        [
            div [ class "info"]
            [
                div [class "head-p"]
                [
                    h2 [] [text "Objective"],
                    p [] [text "Finding the matrix product of two matrices by finding dot product of rows and colums of matrices sequentially."]
                ],
                div [class "head-p"]
                [
                    h2 [] [text "Experiment Setup"],
                    p [] [text "It consists of two matrices of integers. First element of output matrix is selected initially. On clicking the element of output matrix is calculated. THen the selection passes to next element."]
                ],
                div [class "head-p"]
                [
                    h2 [] [text "Procedure"],
                    p [] [text "Steps to be followed:"],
                    p [] [text "Step 1: Click next"],
                    p [] [text "Step 2: Check you get the right answer or not. And repeat the procedure from step 1."]
                ]
            ],
            div [class "exp"]
            [
                div [class "expInner"] 
                [
                    div [class "matrices"]
                    [
                        div [class "matrix"]
                            ((p [] [text "Matrix A"]) :: l1),
                        div [class "matrix"]
                        [
                            p [] [text "Matrix B"],
                            div[class "col-matrix"] l2
                        ],
                        div [class "matrix"]
                            ((p [] [text "Output Matrix"]) :: l_out),
                        if model.reviel_answer == True then
                            div [class "matrix"]
                            ((p [] [text "Answer Matrix"]) :: l_res)
                        else
                            div [] []
                        
                    ],
                    div [] 
                        (text "Array 1" :: Array.toList v1),
                    UI.mul_row "x",
                    div [] 
                        (text "Array 2" :: Array.toList v2),
                    UI.mul_row "=",
                    div [] 
                        (text "Products" :: Array.toList v_sum),
                    p [] [text ("product = " ++ (String.fromInt model.product))],
                    button [class "primary-button bottom-button", onClick Next] [text "Next"],
                    reviel
                ]
            ]
        ]
    ]
    }
