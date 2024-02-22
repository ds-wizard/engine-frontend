module Registry2.Components.ActionButton exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, button, i, text)
import Html.Attributes exposing (class, classList)
import Registry2.Components.FontAwesome exposing (fas)


type alias ButtonConfig a =
    { label : String
    , actionResult : ActionResult a
    }


view : ButtonConfig a -> Html msg
view cfg =
    let
        isLoading =
            ActionResult.isLoading cfg.actionResult

        content =
            if isLoading then
                [ fas "fa-spinner fa-spin" ]

            else
                [ text cfg.label ]
    in
    button
        [ class "btn btn-primary w-100"
        , classList [ ( "disabled", isLoading ) ]
        ]
        content
