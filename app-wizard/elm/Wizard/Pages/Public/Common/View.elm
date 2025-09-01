module Wizard.Pages.Public.Common.View exposing
    ( FormConfig
    , publicForm
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Components.ActionButton as ActionButton
import Shared.Components.FormResult as FormResult
import Wizard.Components.Html exposing (linkTo)
import Wizard.Routes as Routes


type alias FormConfig msg =
    { title : String
    , submitMsg : msg
    , actionResult : ActionResult String
    , submitLabel : String
    , formContent : Html msg
    , link : Maybe ( Routes.Route, String )
    }


publicForm : FormConfig msg -> Html msg
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
                    , ActionButton.submit <| ActionButton.SubmitConfig formConfig.submitLabel formConfig.actionResult
                    ]
                ]
            ]
        ]
