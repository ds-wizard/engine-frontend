module Wizard.Questionnaires.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (l, lg, lh, lx)
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig, ListingDropdownItem)
import Wizard.Common.Html exposing (emptyNode, faKeyClass, faSet, linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Documents.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireState exposing (QuestionnaireState(..))
import Wizard.Questionnaires.Common.View exposing (accessibilityBadge)
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils exposing (listInsertIf)


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
    Page.actionResultView appState (viewQuestionnaires appState model) model.questionnaires


viewQuestionnaires : AppState -> Model -> Listing.Model Questionnaire -> Html Msg
viewQuestionnaires appState model questionnaires =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.successOnlyView appState model.deletingQuestionnaire
        , FormResult.view appState model.deletingMigration
        , FormResult.view appState model.cloningQuestionnaire
        , Listing.view appState (listingConfig appState) questionnaires
        , deleteModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    [ linkTo appState
        (Routes.QuestionnairesRoute <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]
    ]


listingConfig : AppState -> ListingConfig Questionnaire Msg
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
        , ownerIcon appState questionnaire
        , accessibilityBadge appState questionnaire.accessibility
        , migrationBadge appState questionnaire.state
        ]


ownerIcon : AppState -> Questionnaire -> Html Msg
ownerIcon appState questionnaire =
    if appState.config.questionnaireAccessibilityEnabled && questionnaire.ownerUuid == Maybe.map .uuid appState.session.user then
        i
            [ class <| faKeyClass "questionnaireList.owner" appState
            , title <| l_ "badge.owner" appState
            ]
            []

    else
        emptyNode


listingDescription : AppState -> Questionnaire -> Html Msg
listingDescription appState questionnaire =
    let
        kmRoute =
            Routes.KnowledgeModelsRoute <|
                Wizard.KnowledgeModels.Routes.DetailRoute questionnaire.package.id
    in
    linkTo appState
        kmRoute
        [ title <| lg "knowledgeModel" appState ]
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]


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
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| EditRoute <| questionnaire.uuid)
                }

        createDocument =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createDocument" appState
                , label = l_ "action.createDocument" appState
                , msg = ListingActionLink (Routes.DocumentsRoute <| Wizard.Documents.Routes.CreateRoute <| Just questionnaire.uuid)
                }

        viewDocuments =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.viewDocuments" appState
                , label = l_ "action.viewDocuments" appState
                , msg = ListingActionLink (Routes.DocumentsRoute <| Wizard.Documents.Routes.IndexRoute <| Just questionnaire.uuid)
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
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| CreateMigrationRoute <| questionnaire.uuid)
                }

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "questionnaireList.createMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink (Routes.QuestionnairesRoute <| MigrationRoute <| questionnaire.uuid)
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
                , msg = ListingActionMsg (ShowHideDeleteQuestionnaire <| Just questionnaire)
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


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text name ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteQuestionnaire
            , cancelMsg = Just <| ShowHideDeleteQuestionnaire Nothing
            , dangerous = True
            }
    in
    Modal.confirm appState modalConfig


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
