module Public.Common.View exposing (FormConfig, publicForm)

import ActionResult exposing (ActionResult)
import Common.AppState exposing (AppState)
import Common.Html exposing (linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Routes


type alias FormConfig msg =
    { title : String
    , submitMsg : msg
    , actionResult : ActionResult String
    , submitLabel : String
    , formContent : Html msg
    , link : Maybe ( Routes.Route, String )
    }


publicForm : AppState -> FormConfig msg -> Html msg
publicForm appState formConfig =
    let
        link =
            case formConfig.link of
                Just ( route, linkText ) ->
                    linkTo appState route [] [ text linkText ]

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
