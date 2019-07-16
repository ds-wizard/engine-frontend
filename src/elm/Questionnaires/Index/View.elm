module Questionnaires.Index.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, linkTo)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routing
import Msgs
import Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)
import Questionnaires.Common.QuestionnaireState exposing (QuestionnaireState(..))
import Questionnaires.Common.View exposing (accessibilityBadge)
import Questionnaires.Index.ExportModal.View as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing
import Utils exposing (listInsertIf)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewQuestionnaires wrapMsg appState model) model.questionnaires


viewQuestionnaires : (Msg -> Msgs.Msg) -> AppState -> Model -> List Questionnaire -> Html Msgs.Msg
viewQuestionnaires wrapMsg appState model questionnaires =
    div [ listClass "Questionnaires__Index" ]
        [ Page.header "Questionnaires" indexActions
        , FormResult.successOnlyView model.deletingQuestionnaire
        , FormResult.view model.deletingMigration
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy (String.toLower << .name) questionnaires
        , ExportModal.view (wrapMsg << ExportModalMsg) appState model.exportModalModel
        , deleteModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.Questionnaires <| Create Nothing) [ class "btn btn-primary" ] [ text "Create" ] ]


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig Questionnaire Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle appState
    , description = listingDescription
    , actions = listingActions wrapMsg appState
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Questionnaire."
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : AppState -> Questionnaire -> Html Msgs.Msg
listingTitle appState questionnaire =
    let
        linkRoute =
            if questionnaire.state == Migrating then
                migrationRoute

            else
                detailRoute
    in
    span []
        [ linkTo (linkRoute questionnaire) [] [ text questionnaire.name ]
        , ownerIcon appState questionnaire
        , accessibilityBadge appState questionnaire.accessibility
        , migrationBadge questionnaire.state
        ]


ownerIcon : AppState -> Questionnaire -> Html Msgs.Msg
ownerIcon appState questionnaire =
    if appState.config.questionnaireAccessibilityEnabled && questionnaire.ownerUuid == Maybe.map .uuid appState.session.user then
        i [ class "fa fa-user", title "Questionnaire created by you" ] []

    else
        emptyNode


listingDescription : Questionnaire -> Html Msgs.Msg
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


listingActions : (Msg -> Msgs.Msg) -> AppState -> Questionnaire -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg appState questionnaire =
    let
        fillQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Fill questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        viewQuestionnaire =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "View questionnaire"
            , msg = ListingActionLink (detailRoute questionnaire)
            }

        export_ =
            { extraClass = Nothing
            , icon = Just "download"
            , label = "Export"
            , msg = ListingActionMsg (wrapMsg <| ShowExportQuestionnaire questionnaire)
            }

        createMigration =
            { extraClass = Nothing
            , icon = Just "random"
            , label = "Create Migration"
            , msg = ListingActionLink (Routing.Questionnaires <| CreateMigration <| questionnaire.uuid)
            }

        continueMigration =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Continue Migration"
            , msg = ListingActionLink (Routing.Questionnaires <| Migration <| questionnaire.uuid)
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just "ban"
            , label = "Cancel Migration"
            , msg = ListingActionMsg (wrapMsg <| DeleteQuestionnaireMigration questionnaire.uuid)
            }

        edit =
            { extraClass = Nothing
            , icon = Just "edit"
            , label = "Edit"
            , msg = ListingActionLink (Routing.Questionnaires <| Edit <| questionnaire.uuid)
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
            , label = "Delete"
            , msg = ListingActionMsg (wrapMsg <| ShowHideDeleteQuestionnaire <| Just questionnaire)
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


detailRoute : Questionnaire -> Routing.Route
detailRoute =
    Routing.Questionnaires << Detail << .uuid


migrationRoute : Questionnaire -> Routing.Route
migrationRoute =
    Routing.Questionnaires << Migration << .uuid


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete questionnaire"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteQuestionnaire
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteQuestionnaire Nothing
            , dangerous = True
            }
    in
    Modal.confirm modalConfig


migrationBadge : QuestionnaireState -> Html msg
migrationBadge state =
    case state of
        Migrating ->
            span [ class "badge badge-info" ]
                [ text "migrating" ]

        Outdated ->
            span [ class "badge badge-warning" ]
                [ text "outdated" ]

        Default ->
            emptyNode
