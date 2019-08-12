module Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, linkTo)
import Common.View.Listing as Listing exposing (ListingConfig)
import Common.View.Page as Page
import Html exposing (Html, code, div, h3, span, strong, text)
import Html.Attributes exposing (class, title)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KnowledgeModels.Common.Version as Version
import KnowledgeModels.Routing
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (Route(..))
import Time


view : AppState -> ActionResult (List Level) -> ActionResult (List Questionnaire) -> Html msg
view appState levels questionnaires =
    Page.actionResultView (viewQuestionnaires appState) (ActionResult.combine questionnaires levels)


viewQuestionnaires : AppState -> ( List Questionnaire, List Level ) -> Html msg
viewQuestionnaires appState ( questionnaires, levels ) =
    if List.length questionnaires > 0 then
        div [ class "PhaseQuestionnaireWidget" ]
            [ h3 [] [ text "Recent questionnaires" ]
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
                Listing.view (listingConfig appState) questionnaires

            else
                div [ class "empty" ]
                    [ text "No questionnaires in this phase" ]
    in
    div []
        [ strong [] [ text level.title ]
        , content
        ]


listingConfig : AppState -> ListingConfig Questionnaire msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription
    , actions = always []
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Questionnaire."
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : AppState -> Questionnaire -> Html msg
listingTitle appState questionnaire =
    span []
        [ linkTo (Routing.Questionnaires <| Detail <| questionnaire.uuid) [] [ text questionnaire.name ]
        , accessibilityBadge appState questionnaire.accessibility
        ]


listingDescription : Questionnaire -> Html msg
listingDescription questionnaire =
    let
        kmRoute =
            Routing.KnowledgeModels <|
                KnowledgeModels.Routing.Detail questionnaire.package.id
    in
    linkTo kmRoute
        [ title "Knowledge Model" ]
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]
