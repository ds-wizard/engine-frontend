module Questionnaires.Index.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, linkTo)
import Common.Html.Attribute exposing (listClass)
import Common.Locale exposing (l, lg, lh, lx)
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routes
import Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Questionnaires.Common.QuestionnaireState exposing (QuestionnaireState(..))
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Index.ExportModal.View as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routes exposing (Route(..))
import Routes
import Utils exposing (listInsertIf)


l_ : String -> AppState -> String
l_ =
    l "Questionnaires.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Questionnaires.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Questionnaires.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewQuestionnaires appState model) model.questionnaires


viewQuestionnaires : AppState -> Model -> List Questionnaire -> Html Msg
viewQuestionnaires appState model questionnaires =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.successOnlyView model.deletingQuestionnaire
        , FormResult.view model.deletingMigration
        , Listing.view appState (listingConfig appState) <| List.sortBy (String.toLower << .name) questionnaires
        , ExportModal.view appState model.exportModalModel |> Html.map ExportModalMsg
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
    , actions = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
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
        i [ class "fa fa-user", title <| l_ "badge.owner" appState ] []

    else
        emptyNode


listingDescription : AppState -> Questionnaire -> Html Msg
listingDescription appState questionnaire =
    let
        kmRoute =
            Routes.KnowledgeModelsRoute <|
                KnowledgeModels.Routes.DetailRoute questionnaire.package.id
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


listingActions : AppState -> Questionnaire -> List (ListingActionConfig Msg)
listingActions appState questionnaire =
    let
        fillQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = l_ "action.fillQuestionnaire" appState
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        viewQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = l_ "action.viewQuestionnaire" appState
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        export_ =
            { extraClass = Nothing
            , icon = Just "download"
            , label = l_ "action.export" appState
            , msg = ListingActionMsg (ShowExportQuestionnaire questionnaire)
            }

        createMigration =
            { extraClass = Nothing
            , icon = Just "random"
            , label = l_ "action.createMigration" appState
            , msg = ListingActionLink (Routes.QuestionnairesRoute <| CreateMigrationRoute <| questionnaire.uuid)
            }

        continueMigration =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = l_ "action.continueMigration" appState
            , msg = ListingActionLink (Routes.QuestionnairesRoute <| MigrationRoute <| questionnaire.uuid)
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just "ban"
            , label = l_ "action.cancelMigration" appState
            , msg = ListingActionMsg (DeleteQuestionnaireMigration questionnaire.uuid)
            }

        edit =
            { extraClass = Nothing
            , icon = Just "edit"
            , label = l_ "action.edit" appState
            , msg = ListingActionLink (Routes.QuestionnairesRoute <| EditRoute <| questionnaire.uuid)
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
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
        |> listInsertIf continueMigration migrating
        |> listInsertIf cancelMigration migrating
        |> listInsertIf export_ (not migrating)
        |> listInsertIf createMigration (not migrating)
        |> listInsertIf edit (editable && not migrating)
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
    Modal.confirm modalConfig


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
