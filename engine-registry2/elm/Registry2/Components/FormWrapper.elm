module Registry2.Components.FormWrapper exposing
    ( ViewProps
    , view
    )

import Html exposing (Html, div, form, h5, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)


type alias ViewProps msg =
    { title : String
    , submitMsg : msg
    , content : List (Html msg)
    }


view : ViewProps msg -> Html msg
view props =
    div [ class "d-flex justify-content-center align-items-center my-5" ]
        [ form
            [ class "bg-white rounded shadow-sm p-4 w-100 box"
            , onSubmit props.submitMsg
            ]
            (h5 [] [ text props.title ] :: props.content)
        ]
