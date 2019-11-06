module Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, code, div, h3, span, strong, text)
import Html.Attributes exposing (class, title)
import Time
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode, linkTo)
import Wizard.Common.Locale exposing (l, lx)
import Wizard.Common.View.Listing as Listing exposing (ListingConfig)
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KnowledgeModels.Routes
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.View exposing (accessibilityBadge)
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


view : AppState -> ActionResult (List Level) -> ActionResult (List Questionnaire) -> Html msg
view appState levels questionnaires =
    Page.actionResultView appState (viewQuestionnaires appState) (ActionResult.combine questionnaires levels)


viewQuestionnaires : AppState -> ( List Questionnaire, List Level ) -> Html msg
viewQuestionnaires appState ( questionnaires, levels ) =
    if List.length questionnaires > 0 then
        div [ class "PhaseQuestionnaireWidget" ]
            [ h3 [] [ lx_ "recentQuestionnaires" appState ]
            , div [] (List.map (viewLevelGroup appState questionnaires) levels)
            ]

    else
        emptyNode


viewLevelGroup : AppState -> List Questionnaire -> Level -> Html msg
viewLevelGroup appState questionnaires level =
    let
        levelQuestionnaires =
            questionnaires
                |> List.filter (.level >> (==) level.level)
                |> List.sortBy (.updatedAt >> Time.posixToMillis >> negate)
                |> List.take 7
    in
    viewLevelGroupWithData appState levelQuestionnaires level


viewLevelGroupWithData : AppState -> List Questionnaire -> Level -> Html msg
viewLevelGroupWithData appState questionnaires level =
    let
        content =
            if List.length questionnaires > 0 then
                Listing.view appState (listingConfig appState) questionnaires

            else
                div [ class "empty" ]
                    [ lx_ "noQuestionnairesInPhase" appState ]
    in
    div []
        [ strong [] [ text level.title ]
        , content
        ]


listingConfig : AppState -> ListingConfig Questionnaire msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , actions = always []
    , textTitle = .name
    , emptyText = l_ "clickCreate" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : AppState -> Questionnaire -> Html msg
listingTitle appState questionnaire =
    span []
        [ linkTo appState (Routes.QuestionnairesRoute <| DetailRoute <| questionnaire.uuid) [] [ text questionnaire.name ]
        , accessibilityBadge appState questionnaire.accessibility
        ]


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
