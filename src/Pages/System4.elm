module Pages.System4 exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.System4 exposing (Params)
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

instructions : Array.Array String
instructions = Array.fromList ["Select a row from each Matrix 1 and column from Matrix 2", "Click dot to find dot product of selected row and column", "Select a cell in output matrix", "Click place to place the dot product at selected cell in output matrix"]

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
        p1 : Int,
        p2 : Int,
        prods : Array.Array Int,
        step : Int
    }


init : ( Model, Effect Msg )
init =
    ( {
        s1 = -1,
        s2 = -1,
        output = Util.get_2d_zeroes 4 4,
        reviel_answer = False,
        product = 0,
        p1 = -1,
        p2 = -1,
        prods = Array.fromList [-1, -1, -1, -1],
        step = 0
    }, Effect.none )



-- UPDATE


type Msg
    = ChangeSelection Int Int
    | ChangeMatrixSelection Int Int
    | Dot
    | Place
    | RevielChange


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangeSelection a i ->
            if a == 1 then
                if model.s2 /= -1 then
                    ( { model | s1 = i, prods = Array.repeat 4 0, step = 1}, Effect.none )
                else
                    ( { model | s1 = i, prods = Array.repeat 4 0, step = 0}, Effect.none )
            else
                if model.s1 /= -1 then
                    ( { model | s2 = i, prods = Array.repeat 4 0, step = 1}, Effect.none )
                else
                    ( { model | s2 = i, prods = Array.repeat 4 0, step = 0}, Effect.none )
        ChangeMatrixSelection i j ->
            if model.step == 2 then 
                ( { model | p1 = i, p2 = j, step = 3}, Effect.none )
            else
                (model, Effect.none)
        Dot ->
            if model.step == 1 then
                let
                    temp_prods = Util.row_column_prods a1 model.s1 a2 model.s2
                in
                ( { model | product = Util.array_sum temp_prods, prods = temp_prods,  p1 = -1, p2 = -1, step = 2}, Effect.none )
            else
                (model, Effect.none)
        Place ->
            if model.step == 3 then
                ( { model | output =  Util.matrix_value_set model.output model.product model.p1 model.p2, p1 = -1, p2 = -1, product = 0, step = 0}, Effect.none)
            else
                ( model, Effect.none )
        RevielChange ->
            ({model | reviel_answer = not model.reviel_answer}, Effect.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
type MatrixType
    = AnsMatrix
    | OutMatrix Bool Int Int
    | Normal Bool Int Int

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
                OutMatrix selected p1 p2 ->
                    if selected == True then
                        button [class "square matrix_square m_selected"]
                        [ text (String.fromInt num)]
                    else
                        button [class "square matrix_square", onClick (ChangeMatrixSelection p1 p2)]
                        [ text (String.fromInt num)]
                Normal selected a i->
                    if selected == True then
                        button [class "square selected-square", onClick (ChangeSelection a i)]
                        [ text (String.fromInt num)]
                    else
                        button [class "square", onClick (ChangeSelection a i)]
                        [ text (String.fromInt num)]
        l1 = List.map (\x -> div [class "matrix_row"] (List.map (\y -> make_square (Normal (x == model.s1) 1 x) (Util.get_matrix_element x y a1)) [0, 1, 2, 3])) [0,1,2,3]
        l2 = List.map (\x -> div [class "matrix_col"] (List.map (\y -> make_square (Normal (x == model.s2) 2 x) (Util.get_matrix_element x y a2)) [0, 1, 2, 3])) [0,1,2,3]
        l_out = List.map (\x -> div [] (List.map (\y -> make_square (OutMatrix (x == model.p1 && y ==  model.p2 ) x y) (Util.get_matrix_element x y model.output)) [0, 1, 2, 3])) [0,1,2,3]
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
    
    { title = "System4"
    , body = UI.layout 4 
    [
        div [ class "system"]
        [
            div [class "info"]
            [
                div [ class "info-inner"]
                [
                    div [class "head-p"]
                    [
                        h2 [] [text "Objective"],
                        p [] [text "Finding the matrix product of two matrices by multiplying row and columns of them"]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Experiment Setup"],
                        p [] [text "It consists of two matrices of integers. You can select a row from A and column from B and find dot product by clicking dot button. You can place that dot product in a cell of output by selecting a cell and clicking place."]
                    ],
                    div [class "head-p"]
                    [
                        h2 [] [text "Procedure"],
                        p [] [text "Steps to be followed:"],
                        p [] [text "Step 1: Select a row in matrix A and column in matrix B."],
                        p [] [text "Step 2: Find dot product by clicking dot."],
                        p [] [text "Step 3: Select a cell in the output matrix."],
                        p [] [text "Step 4: Click button place to assign the dot product to the selected cell in output matrix."],
                        p [] [text "Step 5: Check you get the right answer or not. And repeat the procedure from step 1."]
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
                    button [class "secondary-button bottom-button", onClick Dot] [text "Dot"],
                    button [class "primary-button bottom-button", onClick Place]
                    [text "Place"],
                    reviel
                ]
            ]
        ]
    ]
    }
