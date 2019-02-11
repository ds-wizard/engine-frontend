module Public.Common.View exposing (FormConfig, publicForm)

import ActionResult exposing (ActionResult)
import Common.Html exposing (linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Msgs
import Routing exposing (Route)


type alias FormConfig =
    { title : String
    , submitMsg : Msgs.Msg
    , actionResult : ActionResult String
    , submitLabel : String
    , formContent : Html Msgs.Msg
    , link : Maybe ( Route, String )
    }


publicForm : FormConfig -> Html Msgs.Msg
publicForm formConfig =
    let
        link =
            case formConfig.link of
                Just ( route, linkText ) ->
                    linkTo route [] [ text linkText ]

                _ ->
                    span [] []
    in
    div [ class "align-self-center col-xs-10 col-sm-8 col-md-6 col-lg-4" ]
        [ form [ onSubmit formConfig.submitMsg, class "card bg-light" ]
            [ div [ class "card-header" ] [ text formConfig.title ]
            , div [ class "card-body" ]
                [ FormResult.view formConfig.actionResult
                , formConfig.formContent
                , div [ class "form-group d-flex align-items-baseline justify-content-between" ]
                    [ link
                    , ActionButton.submit ( formConfig.submitLabel, formConfig.actionResult )
                    ]
                ]
            ]
        ]
