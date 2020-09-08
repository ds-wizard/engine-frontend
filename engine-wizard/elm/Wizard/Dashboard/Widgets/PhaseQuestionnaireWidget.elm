module Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, code, div, h3, span, strong, text)
import Html.Attributes exposing (class, title)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Time
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing exposing (ListingConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Dashboard.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


view : AppState -> ActionResult (List Level) -> ActionResult (List Questionnaire) -> Html Msg
view appState levels questionnaires =
    Page.actionResultView appState (viewQuestionnaires appState) (ActionResult.combine questionnaires levels)


viewQuestionnaires : AppState -> ( List Questionnaire, List Level ) -> Html Msg
viewQuestionnaires appState ( questionnaires, levels ) =
    if List.length questionnaires > 0 then
        div [ class "PhaseQuestionnaireWidget" ]
            [ h3 [] [ lx_ "recentProjects" appState ]
            , div [] (List.map (viewLevelGroup appState questionnaires) levels)
            ]

    else
        emptyNode


viewLevelGroup : AppState -> List Questionnaire -> Level -> Html Msg
viewLevelGroup appState questionnaires level =
    let
        levelQuestionnaires =
            questionnaires
                |> List.filter (.level >> (==) level.level)
                |> List.sortBy (.updatedAt >> Time.posixToMillis >> negate)
                |> List.take 7
    in
    viewLevelGroupWithData appState levelQuestionnaires level


viewLevelGroupWithData : AppState -> List Questionnaire -> Level -> Html Msg
viewLevelGroupWithData appState questionnaires level =
    let
        content =
            if List.length questionnaires > 0 then
                Listing.view appState (listingConfig appState) <| Listing.modelFromList questionnaires

            else
                div [ class "empty" ]
                    [ lx_ "noProjectsInPhase" appState ]
    in
    div []
        [ strong [] [ text level.title ]
        , content
        ]


listingConfig : AppState -> ListingConfig Questionnaire Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = always []
    , textTitle = .name
    , emptyText = l_ "clickCreate" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    }


listingTitle : AppState -> Questionnaire -> Html msg
listingTitle appState questionnaire =
    span []
        (linkTo appState (Routes.PlansRoute <| DetailRoute questionnaire.uuid PlanDetailRoute.Questionnaire) [] [ text questionnaire.name ]
            :: visibilityIcons appState questionnaire
        )


listingDescription : AppState -> Questionnaire -> Html msg
listingDescription appState questionnaire =
    let
        kmRoute =
            Routes.KnowledgeModelsRoute <|
                Wizard.KnowledgeModels.Routes.DetailRoute questionnaire.package.id
    in
    linkTo appState
        kmRoute
        [ title <| l_ "knowledgeModel" appState ]
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]
