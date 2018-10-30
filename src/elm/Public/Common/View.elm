module Public.Common.View exposing (..)

import ActionResult exposing (ActionResult)
import Common.Html exposing (emptyNode, linkTo)
import Common.View.Forms exposing (formResultView, submitButton)
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
                [ formResultView formConfig.actionResult
                , formConfig.formContent
                , div [ class "form-group d-flex align-items-baseline justify-content-between" ]
                    [ link
                    , submitButton ( formConfig.submitLabel, formConfig.actionResult )
                    ]
                ]
            ]
        ]
