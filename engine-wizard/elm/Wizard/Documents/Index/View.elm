module Wizard.Documents.Index.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, title, type_)
import Html.Events exposing (onCheck, onClick)
import Markdown
import Maybe.Extra as Maybe
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lf, lh, lx)
import Wizard.Auth.Permission as Permission
import Wizard.Common.Api.Documents as DocumentsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing exposing (ListingActionType(..), ListingConfig, ListingDropdownItem)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.Document as Document exposing (Document)
import Wizard.Documents.Common.DocumentState exposing (DocumentState(..))
import Wizard.Documents.Index.Models exposing (Model)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Routes
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Utils exposing (listInsertIf)


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

        actionResult =
            ActionResult.combine model.documents questionnaireActionResult
    in
    Page.actionResultView appState (viewDocuments appState model) actionResult


viewDocuments : AppState -> Model -> ( Listing.Model Document, Maybe QuestionnaireDetail ) -> Html Msg
viewDocuments appState model ( documents, mbQuestionnaire ) =
    let
        questionnaireView =
            case mbQuestionnaire of
                Just questionnaire ->
                    div [ class "filters" ]
                        [ lx_ "listing.filter" appState
                        , span [ class "badge badge-pill badge-secondary" ]
                            [ text questionnaire.name
                            , linkTo appState (Routes.DocumentsRoute (IndexRoute Nothing)) [] [ faSet "_global.remove" appState ]
                            ]
                        ]

                Nothing ->
                    emptyNode
    in
    div [ listClass "Documents__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , questionnaireView
        , FormResult.successOnlyView appState model.deletingDocument
        , Listing.view appState (listingConfig appState) documents
        , deleteModal appState model
        , submitModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    [ linkTo appState
        (Routes.DocumentsRoute <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]
    ]


listingConfig : AppState -> ListingConfig Document Msg
listingConfig appState =
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
    }


listingTitle : AppState -> Document -> Html Msg
listingTitle appState document =
    let
        name =
            if document.state == DoneDocumentState then
                a
                    [ href <| DocumentsApi.downloadDocumentUrl document.uuid appState
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
                            Routes.QuestionnairesRoute <|
                                Wizard.Questionnaires.Routes.DetailRoute questionnaire.uuid
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
                , msg = ListingActionExternalLink (DocumentsApi.downloadDocumentUrl document.uuid appState)
                }

        submit =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.submit" appState
                , label = l_ "action.submit" appState
                , msg = ListingActionMsg (ShowHideSubmitDocument <| Just document)
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg = ListingActionMsg (ShowHideDeleteDocument <| Just document)
                }

        submitEnabled =
            (document.state == DoneDocumentState)
                && appState.config.submission.enabled
                && Permission.hasPerm appState.jwt Permission.submission
    in
    []
        |> listInsertIf download (document.state == DoneDocumentState)
        |> listInsertIf submit submitEnabled
        |> listInsertIf Listing.dropdownSeparator (document.state == DoneDocumentState)
        |> listInsertIf delete (Document.isEditable appState document)


stateBadge : AppState -> DocumentState -> Html msg
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            span [ class "badge badge-info" ]
                [ faSet "_global.spinner" appState
                , lx_ "badge.queued" appState
                ]

        InProgressDocumentState ->
            span [ class "badge badge-info" ]
                [ faSet "_global.spinner" appState
                , lx_ "badge.inProgress" appState
                ]

        DoneDocumentState ->
            emptyNode

        ErrorDocumentState ->
            span [ class "badge badge-danger" ]
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

        submissionBody submissionServices =
            div []
                [ FormResult.errorOnlyView appState model.submittingDocument
                , div [ class "form-radio-group" ]
                    (List.map viewOption submissionServices)
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
            }
    in
    Modal.simple modalConfig
