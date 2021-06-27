module UI exposing (layout)

import Html exposing (Html)
import Html.Attributes as Attr
import Gen.Route as Route exposing (Route)
import Html.Attributes exposing (..)
import Html exposing (..)
import Array exposing (Array)
-- nav_buttons : List String
-- nav_buttons = [ "Home", "Multiply and Add randomly", "Multiply and Add Inorder", "Dot Product sequentially", "Dot products between a row and column of two matrices", "Dot products in order", "Matrix multiplication"]
-- routes : Array Route
-- routes = Array.fromList [Route.Home_, Route.System1, Route.System2, Route.System3, Route.System4, Route.System5, Route.System6]




layout : Int ->List (Html msg) -> List(Html msg)
layout selected children =
    let
        -- viewLink : String -> Route -> Html msg
        -- viewLink label url =
        --     Html.a [ Attr.href url ] [ Html.text label ]
        
        make_nav_button : Int -> String -> Route -> Html msg
        make_nav_button ind name route = 
            if selected == ind then
                a [  href (Route.toHref (route)) ]
                    [  button [ id "highlighted-nav" ] [text name] ]
            else
                a [ href (Route.toHref (route)) ]
                    [  button [ href (Route.toHref (route)) ] [text name] ]

    in
    [
    div [ class "App" ]
        [
            header [class "heading"]
            [
                text "Matrix multiplication"
            ],
            div [class "navbar"]
                [ make_nav_button 0 "Home" Route.Home_
                , make_nav_button 1 "Multiply and Add randomly" Route.System1
                , make_nav_button 2 "Multiply and Add Inorder" Route.System2
                , make_nav_button 3 "Dot Product sequentially" Route.System3
                , make_nav_button 4 "Dot products between a row and column of two matrices" Route.System4
                , make_nav_button 5 "Dot products in order" Route.System5
                , make_nav_button 6 "Matrix multiplication" Route.System6
                ]
                

            , Html.main_ [] children
        ]
    ]

    -- [ Html.div [ Attr.class "container" ]
    --     [ Html.header [ Attr.class "navbar" ]
    --         [ viewLink "Home" Route.Home_
    --         , viewLink "Static" Route.Home_
    --         ]
    --     , Html.main_ [] children
    --     ]
    -- ]

