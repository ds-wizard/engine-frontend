module Wizard.Projects.Detail.Components.Shared.TemplateNotSet exposing (view)

import Html exposing (Html, p)
import Html.Attributes exposing (class)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes as PlansRoutes
import Wizard.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Shared.TemplateNotSet"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.Shared.TemplateNotSet"


view : AppState -> QuestionnaireDetail -> Html msg
view appState questionnaire =
    let
        content =
            if QuestionnaireDetail.isOwner appState questionnaire then
                [ p [] [ lx_ "textOwner" appState ]
                , p []
                    [ linkTo appState
                        (Wizard.Routes.PlansRoute (PlansRoutes.DetailRoute questionnaire.uuid PlanDetailRoute.Settings))
                        [ class "btn btn-primary btn-lg link-with-icon-after" ]
                        [ lx_ "link" appState
                        , faSet "_global.arrowRight" appState
                        ]
                    ]
                ]

            else
                [ p [] [ lx_ "textNotOwner" appState ]
                ]
    in
    Page.illustratedMessageHtml
        { image = "website_builder"
        , heading = l_ "heading" appState
        , content = content
        }
