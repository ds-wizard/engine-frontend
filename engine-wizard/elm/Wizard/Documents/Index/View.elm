module Wizard.Documents.Index.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, title, type_)
import Html.Events exposing (onCheck, onClick)
import Markdown
import Maybe.Extra as Maybe
import Shared.Api.Documents as DocumentsApi
import Shared.Auth.Permission as Perm
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lf, lg, lh, lx)
import Shared.Utils exposing (listInsertIf)
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Documents.Index.Models exposing (Model)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes
import Wizard.Routes as Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Documents.Index.View"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Documents.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Documents.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Documents.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        questionnaireActionResult =
            case model.questionnaire of
                Just questionnaire ->
                    ActionResult.map Just questionnaire

                Nothing ->
                    Success Nothing
    in
    Page.actionResultView appState (viewDocuments appState model) questionnaireActionResult


viewDocuments : AppState -> Model -> Maybe QuestionnaireDetail -> Html Msg
viewDocuments appState model mbQuestionnaire =
    let
        mbQuestionnaireFilterView =
            case mbQuestionnaire of
                Just questionnaire ->
                    Just <|
                        div [ class "listing-toolbar-extra questionnaire-filter" ]
                            [ linkTo appState
                                (Routes.ProjectsRoute (Wizard.Projects.Routes.DetailRoute questionnaire.uuid PlanDetailRoute.Questionnaire))
                                [ class "questionnaire-name" ]
                                [ text questionnaire.name ]
                            , linkTo appState
                                (Routes.DocumentsRoute (IndexRoute Nothing PaginationQueryString.empty))
                                [ class "text-danger" ]
                                [ faSet "_global.remove" appState ]
                            ]

                Nothing ->
                    Nothing
    in
    div [ listClass "Documents__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.successOnlyView appState model.deletingDocument
        , Listing.view appState (listingConfig appState model mbQuestionnaireFilterView) model.documents
        , deleteModal appState model
        , submitModal appState model
        ]


listingConfig : AppState -> Model -> Maybe (Html Msg) -> Listing.ViewConfig Document Msg
listingConfig appState model mbQuestionnaireFilterView =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "document.name" appState )
        , ( "createdAt", lg "document.createdAt" appState )
        ]
    , filters = []
    , toRoute = \_ -> Routes.DocumentsRoute << IndexRoute model.questionnaireUuid
    , toolbarExtra = mbQuestionnaireFilterView
    }


listingTitle : AppState -> Document -> Html Msg
listingTitle appState document =
    let
        name =
            if document.state == DoneDocumentState then
                a
                    [ href <| DocumentsApi.downloadDocumentUrl (Uuid.toString document.uuid) appState
                    , target "_blank"
                    , title <| l_ "listing.name.title" appState
                    ]
                    [ text document.name ]

            else
                span [] [ text document.name ]
    in
    span []
        [ name
        , stateBadge appState document.state
        ]


listingDescription : AppState -> Document -> Html Msg
listingDescription appState document =
    let
        questionnaireLink =
            case document.questionnaire of
                Just questionnaire ->
                    let
                        questionnaireRoute =
                            Routes.ProjectsRoute <|
                                Wizard.Projects.Routes.DetailRoute questionnaire.uuid PlanDetailRoute.Questionnaire
                    in
                    linkTo appState
                        questionnaireRoute
                        [ class "fragment" ]
                        [ text questionnaire.name ]

                Nothing ->
                    emptyNode

        ( icon, formatName ) =
            case Document.getFormat document of
                Just format ->
                    ( fa format.icon, format.name )

                Nothing ->
                    ( emptyNode, "" )
    in
    span []
        [ span [ class "fragment" ] [ icon, text formatName ]
        , questionnaireLink
        ]


listingActions : AppState -> Document -> List (ListingDropdownItem Msg)
listingActions appState document =
    let
        download =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.download" appState
                , label = l_ "action.download" appState
                , msg = ListingActionExternalLink (DocumentsApi.downloadDocumentUrl (Uuid.toString document.uuid) appState)
                , dataCy = "download"
                }

        submit =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.submit" appState
                , label = l_ "action.submit" appState
                , msg = ListingActionMsg (ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg = ListingActionMsg (ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }
    in
    []
        |> listInsertIf download (Feature.documentDownload appState document)
        |> listInsertIf submit (Feature.documentSubmit appState document)
        |> listInsertIf Listing.dropdownSeparator (Feature.documentDelete appState document)
        |> listInsertIf delete (Feature.documentDelete appState document)


stateBadge : AppState -> DocumentState -> Html msg
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            span [ class "badge badge-info", dataCy "documents_state-badge" ]
                [ faSet "_global.spinner" appState
                , lx_ "badge.queued" appState
                ]

        InProgressDocumentState ->
            span [ class "badge badge-info", dataCy "documents_state-badge" ]
                [ faSet "_global.spinner" appState
                , lx_ "badge.inProgress" appState
                ]

        DoneDocumentState ->
            emptyNode

        ErrorDocumentState ->
            span [ class "badge badge-danger", dataCy "documents_state-badge" ]
                [ lx_ "badge.error" appState ]


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, name ) =
            case model.documentToBeDeleted of
                Just document ->
                    ( True, document.name )

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
            , actionResult = model.deletingDocument
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteDocument
            , cancelMsg = Just <| ShowHideDeleteDocument Nothing
            , dangerous = True
            , dataCy = "document-delete"
            }
    in
    Modal.confirm appState modalConfig


submitModal : AppState -> Model -> Html Msg
submitModal appState model =
    let
        ( visible, name ) =
            case model.documentToBeSubmitted of
                Just document ->
                    ( True, document.name )

                Nothing ->
                    ( False, "" )

        submitButton =
            if ActionResult.isSuccess model.submittingDocument then
                button [ class "btn btn-primary", onClick <| ShowHideSubmitDocument Nothing ]
                    [ lx_ "submitModal.button.done" appState ]

            else if ActionResult.isSuccess model.submissionServices && Maybe.isJust model.selectedSubmissionServiceId then
                ActionButton.button appState
                    { label = l_ "submitModal.button.submit" appState
                    , result = model.submittingDocument
                    , msg = SubmitDocument
                    , dangerous = False
                    }

            else
                button [ class "btn btn-primary", disabled True ]
                    [ lx_ "submitModal.button.submit" appState ]

        cancelButton =
            button [ onClick <| ShowHideSubmitDocument Nothing, class "btn btn-secondary", disabled <| ActionResult.isLoading model.submittingDocument ]
                [ lx_ "submitModal.button.cancel" appState ]

        viewOption submissionService =
            div [ class "form-check", classList [ ( "form-check-selected", model.selectedSubmissionServiceId == Just submissionService.id ) ] ]
                [ input
                    [ type_ "radio"
                    , class "form-check-input"
                    , id submissionService.id
                    , checked (model.selectedSubmissionServiceId == Just submissionService.id)
                    , onCheck (\_ -> SelectSubmissionService submissionService.id)
                    , disabled <| ActionResult.isLoading model.submittingDocument
                    ]
                    []
                , label [ class "form-check-label", for submissionService.id ]
                    [ text submissionService.name
                    , Markdown.toHtml [ class "form-text text-muted" ] submissionService.description
                    ]
                ]

        options submissionServices =
            if List.length submissionServices > 0 then
                div [ class "form-radio-group" ]
                    (List.map viewOption submissionServices)

            else
                Flash.info appState <| l_ "submitModal.noSubmission" appState

        submissionBody submissionServices =
            div []
                [ FormResult.errorOnlyView appState model.submittingDocument
                , options submissionServices
                ]

        resultBody submission =
            let
                link =
                    case submission.location of
                        Just location ->
                            div [ class "mt-2" ]
                                [ lx_ "submitModal.success.link" appState
                                , a [ href location, target "_blank" ]
                                    [ text location ]
                                ]

                        Nothing ->
                            emptyNode
            in
            div [ class "alert alert-success" ]
                [ faSet "_global.success" appState
                , lx_ "submitModal.success.message" appState
                , link
                ]

        body =
            if ActionResult.isSuccess model.submittingDocument then
                ActionResultBlock.view appState resultBody model.submittingDocument

            else
                ActionResultBlock.view appState submissionBody model.submissionServices

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text <| lf_ "submitModal.title" [ name ] appState ]
                ]
            , div [ class "modal-body" ]
                [ body
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        modalConfig =
            { modalContent = content
            , visible = visible
            , dataCy = "document-submit"
            }
    in
    Modal.simple modalConfig
