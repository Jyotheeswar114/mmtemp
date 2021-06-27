module Pages.Home_ exposing (view)


import Html
import View exposing (View)
import UI
import Html exposing (div)
import Html.Attributes exposing (class)
import Html exposing (p)
import Html exposing (text)

view : View msg
view =
    { title = "Homepage"
    , body = UI.layout 0 
        [ 
            div [class "home-div"]
            [
                p [class "home-note"]
                [ text "Learning algorithms by pseudo code is difficult. Although animation is better than pseudo but it's not the best. So we are introducing transition systems which are interative by nature. Through these systems learning will be simple." ]
            ]    
        ]
    }
