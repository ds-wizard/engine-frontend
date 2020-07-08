module Wizard.Questionnaires.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireState exposing (QuestionnaireState(..))
import Shared.Data.SummaryReport exposing (IndicationReport(..), compareIndicationReport, unwrapIndicationReport)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Utils exposing (listInsertIf)
import Uuid
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionConfig, ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModalMsg
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.View exposing (visibilityBadge)
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Questionnaires.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Questionnaires.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.successOnlyView appState model.deleteModalModel.deletingQuestionnaire
        , FormResult.view appState model.deletingMigration
        , FormResult.view appState model.cloningQuestionnaire
        , Listing.view appState (listingConfig appState) model.questionnaires
        , Html.map DeleteQuestionnaireModalMsg <| DeleteQuestionnaireModal.view appState model.deleteModalModel
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    [ linkTo appState
        (Routes.QuestionnairesRoute <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]
    ]


listingConfig : AppState -> ViewConfig Questionnaire Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , sortOptions =
        [ ( "name", lg "questionnaire.name" appState )
        , ( "createdAt", lg "questionnaire.createdAt" appState )
        , ( "updatedAt", lg "questionnaire.updatedAt" appState )
        ]
    , toRoute = Routes.QuestionnairesRoute << IndexRoute
    , toolbarExtra = Nothing
    }


listingTitle : AppState -> Questionnaire -> Html Msg
listingTitle appState questionnaire =
    let
        linkRoute =
            if questionnaire.state == Migrating then
                migrationRoute

            else
                detailRoute
    in
    span []
        [ linkTo appState (linkRoute questionnaire) [] [ text questionnaire.name ]
        , visibilityBadge appState questionnaire.visibility
        , migrationBadge appState questionnaire.state
        ]


listingDescription : AppState -> Questionnaire -> Html Msg
listingDescription appState questionnaire =
    let
        ownerName =
            case questionnaire.owner of
                Just owner ->
                    span [ class "fragment fragment-icon-light" ]
                        [ img [ src (User.imageUrl owner), class "user-icon user-icon-small" ] []
                        , text <| User.fullName owner
                        ]

                Nothing ->
                    emptyNode

        kmRoute =
            Routes.KnowledgeModelsRoute <|
                Wizard.KnowledgeModels.Routes.DetailRoute questionnaire.package.id

        kmLink =
            linkTo appState
                kmRoute
                [ title <| lg "knowledgeModel" appState, class "fragment" ]
                [ text questionnaire.package.name
                , text ", "
                , text <| Version.toString questionnaire.package.version
                , text " ("
                , code [] [ text questionnaire.package.id ]
                , text ")"
                ]

        toAnsweredInidcation answeredInidciation =
            let
                { answeredQuestions, unansweredQuestions } =
                    unwrapIndicationReport answeredInidciation
            in
            span [ class "fragment", classList [ ( "text-success", unansweredQuestions == 0 ) ] ]
                [ text ("Answered " ++ String.fromInt answeredQuestions ++ "/" ++ String.fromInt (answeredQuestions + unansweredQuestions)) ]

        answered =
            questionnaire.report.indications
                |> List.sortWith compareIndicationReport
                |> List.take 1
                |> List.map toAnsweredInidcation
    in
    span []
        (ownerName :: kmLink :: answered)


listingActions : AppState -> Questionnaire -> List (ListingDropdownItem Msg)
listingActions appState questionnaire =
    let
        fillQuestionnaire =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaire.fill" appState
                , label = l_ "action.fillQuestionnaire" appState
                , msg = ListingActionLink (detailRoute questionnaire)
                }

        viewQuestionnaire =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.view" appState
                , label = l_ "action.viewQuestionnaire" appState
                , msg = ListingActionLink (detailRoute questionnaire)
                }

        edit =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.edit" appState
                , label = l_ "action.edit" appState
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| EditRoute questionnaire.uuid)
                }

        createDocument =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createDocument" appState
                , label = l_ "action.createDocument" appState
                , msg = ListingActionLink (Routes.DocumentsRoute <| Wizard.Documents.Routes.CreateRoute questionnaire.uuid)
                }

        viewDocuments =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.viewDocuments" appState
                , label = l_ "action.viewDocuments" appState
                , msg = ListingActionLink (Routes.DocumentsRoute <| Wizard.Documents.Routes.IndexRoute (Just questionnaire.uuid) PaginationQueryString.empty)
                }

        clone =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.clone" appState
                , label = l_ "action.clone" appState
                , msg = ListingActionMsg (CloneQuestionnaire questionnaire)
                }

        createMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.createMigration" appState
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| CreateMigrationRoute questionnaire.uuid)
                }

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| MigrationRoute questionnaire.uuid)
                }

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.cancel" appState
                , label = l_ "action.cancelMigration" appState
                , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg =
                    QuestionnaireDescriptor.fromQuestionnaire questionnaire
                        |> Just
                        |> DeleteQuestionnaireModalMsg.ShowHideDeleteQuestionnaire
                        |> DeleteQuestionnaireModalMsg
                        |> ListingActionMsg
                }

        editable =
            Questionnaire.isEditable appState questionnaire

        migrating =
            questionnaire.state == Migrating
    in
    []
        |> listInsertIf fillQuestionnaire (editable && not migrating)
        |> listInsertIf viewQuestionnaire (not editable && not migrating)
        |> listInsertIf edit (editable && not migrating)
        |> listInsertIf Listing.dropdownSeparator (not migrating)
        |> listInsertIf createDocument (not migrating)
        |> listInsertIf viewDocuments (not migrating)
        |> listInsertIf Listing.dropdownSeparator (not migrating)
        |> listInsertIf clone (not migrating)
        |> listInsertIf continueMigration migrating
        |> listInsertIf cancelMigration migrating
        |> listInsertIf createMigration (not migrating)
        |> listInsertIf Listing.dropdownSeparator (editable && not migrating)
        |> listInsertIf delete (editable && not migrating)


detailRoute : Questionnaire -> Routes.Route
detailRoute =
    Routes.QuestionnairesRoute << DetailRoute << .uuid


migrationRoute : Questionnaire -> Routes.Route
migrationRoute =
    Routes.QuestionnairesRoute << MigrationRoute << .uuid


migrationBadge : AppState -> QuestionnaireState -> Html msg
migrationBadge appState state =
    case state of
        Migrating ->
            span [ class "badge badge-info" ]
                [ lx_ "badge.migrating" appState ]

        Outdated ->
            span [ class "badge badge-warning" ]
                [ lx_ "badge.outdated" appState ]

        Default ->
            emptyNode
