module Wizard.Documents.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, input, label, p, span, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, type_)
import Html.Events exposing (onCheck, onClick)
import Maybe.Extra as Maybe
import Shared.Common.ByteUnits as ByteUnits
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.QuestionnaireCommon exposing (QuestionnaireCommon)
import Shared.Data.Submission as Submission exposing (Submission)
import Shared.Data.Submission.SubmissionState as SubmissionState
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Markdown as Markdown
import String.Format as String
import Time.Distance as TimeDistance
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass, tooltip, tooltipCustom)
import Wizard.Common.TimeDistance as TimeDistance
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Documents.Index.Models exposing (Model)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))


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


viewDocuments : AppState -> Model -> Maybe QuestionnaireCommon -> Html Msg
viewDocuments appState model mbQuestionnaire =
    let
        questionnaireFilterView questionnaire =
            div [ class "listing-toolbar-extra questionnaire-filter" ]
                [ linkTo appState
                    (Routes.projectsDetail questionnaire.uuid)
                    [ class "questionnaire-name" ]
                    [ text questionnaire.name ]
                , linkTo appState
                    Routes.documentsIndex
                    [ class "text-danger" ]
                    [ faSet "_global.remove" appState ]
                ]

        mbQuestionnaireFilterView =
            Maybe.map questionnaireFilterView mbQuestionnaire
    in
    div [ listClass "Documents__Index" ]
        [ Page.headerWithGuideLink appState (gettext "Project Documents" appState.locale) GuideLinks.projectsDocuments
        , Listing.view appState (listingConfig appState model mbQuestionnaireFilterView) model.documents
        , deleteModal appState model
        , submitModal appState model
        , documentErrorModal appState model
        , submissionErrorModal appState model
        ]


listingConfig : AppState -> Model -> Maybe (Html Msg) -> Listing.ViewConfig Document Msg
listingConfig appState model mbQuestionnaireFilterView =
    let
        itemAdditionalData document =
            if List.isEmpty document.submissions then
                Nothing

            else
                Just <|
                    [ strong [] [ text (gettext "Submissions" appState.locale) ]
                    , table [ class "table table-sm table-borderless table-striped" ]
                        [ tbody []
                            (List.map (viewSubmission appState) (List.sortWith Submission.compare document.submissions))
                        ]
                    ]
    in
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , itemAdditionalData = itemAdditionalData
    , textTitle = .name
    , emptyText = gettext "There are no documents." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search documents..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.DocumentsRoute << IndexRoute model.questionnaireUuid
    , toolbarExtra = mbQuestionnaireFilterView
    }


listingTitle : AppState -> Document -> Html Msg
listingTitle appState document =
    let
        ( name, downloadTooltip ) =
            if document.state == DoneDocumentState then
                ( a
                    [ onClick (DownloadDocument document) ]
                    [ text document.name ]
                , tooltipCustom "with-tooltip-right with-tooltip-align-left" (gettext "Click to download the document" appState.locale)
                )

            else
                ( span [] [ text document.name ], [] )
    in
    span downloadTooltip
        [ name
        , stateBadge appState document.state
        ]


listingDescription : AppState -> Document -> Html Msg
listingDescription appState document =
    let
        questionnaireLink =
            case document.questionnaire of
                Just questionnaire ->
                    span [ class "fragment" ]
                        [ linkTo appState
                            (Routes.projectsDetail questionnaire.uuid)
                            []
                            [ text questionnaire.name ]
                        ]

                Nothing ->
                    emptyNode

        formatFragment =
            case document.format of
                Just format ->
                    span [ class "fragment" ] [ fa format.icon, text format.name ]

                Nothing ->
                    emptyNode

        fileSizeFragment =
            case document.fileSize of
                Just fileSize ->
                    span [ class "fragment" ] [ text (ByteUnits.toReadable fileSize) ]

                Nothing ->
                    emptyNode

        documentTemplateLink =
            span [ class "fragment" ]
                [ linkTo appState
                    (Routes.documentTemplatesDetail document.documentTemplateId)
                    []
                    [ text document.documentTemplateName ]
                ]
    in
    span []
        [ formatFragment
        , fileSizeFragment
        , questionnaireLink
        , documentTemplateLink
        ]


listingActions : AppState -> Document -> List (ListingDropdownItem Msg)
listingActions appState document =
    let
        downloadEnabled =
            Feature.documentDownload appState document

        download =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.download" appState
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (DownloadDocument document)
                , dataCy = "download"
                }

        submitEnabled =
            Feature.documentSubmit appState document

        submit =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.submit" appState
                , label = gettext "Submit" appState.locale
                , msg = ListingActionMsg (ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        viewErrorEnabled =
            Maybe.isJust document.workerLog && document.state == ErrorDocumentState

        viewError =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.viewError" appState
                , label = gettext "View error" appState.locale
                , msg = ListingActionMsg (SetDocumentErrorModal document.workerLog)
                , dataCy = "view-error"
                }

        deleteEnabled =
            Feature.documentDelete appState document

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }

        groups =
            [ [ ( download, downloadEnabled )
              , ( submit, submitEnabled )
              , ( viewError, viewErrorEnabled )
              ]
            , [ ( delete, deleteEnabled )
              ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


stateBadge : AppState -> DocumentState -> Html msg
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            Badge.info [ dataCy "documents_state-badge" ]
                [ faSet "_global.spinner" appState
                , text (gettext "Queued" appState.locale)
                ]

        InProgressDocumentState ->
            Badge.info [ dataCy "documents_state-badge" ]
                [ faSet "_global.spinner" appState
                , text (gettext "In Progress" appState.locale)
                ]

        DoneDocumentState ->
            emptyNode

        ErrorDocumentState ->
            Badge.danger [ dataCy "documents_state-badge" ]
                [ text (gettext "Error" appState.locale) ]


viewSubmission : AppState -> Submission -> Html Msg
viewSubmission appState submission =
    let
        viewSubmissionState submissionState =
            case submissionState of
                SubmissionState.InProgress ->
                    Badge.info []
                        [ faSet "_global.spinner" appState
                        , text (gettext "Submitting" appState.locale)
                        ]

                SubmissionState.Done ->
                    Badge.success [] [ text (gettext "Submitted" appState.locale) ]

                SubmissionState.Error ->
                    Badge.danger [] [ text (gettext "Error" appState.locale) ]

        readableTime =
            TimeUtils.toReadableDateTime appState.timeZone submission.updatedAt

        updatedText =
            TimeDistance.inWordsWithConfig { withAffix = True } (TimeDistance.locale appState) submission.updatedAt appState.currentTime

        link =
            case ( submission.state, submission.location, submission.returnedData ) of
                ( SubmissionState.Done, Just location, _ ) ->
                    a [ href location, class "with-icon-after", target "_blank" ]
                        [ text (gettext "View submission" appState.locale)
                        , faSet "_global.externalLink" appState
                        ]

                ( SubmissionState.Error, _, Just _ ) ->
                    a [ onClick (SetSubmissionErrorModal (Just (Submission.getReturnedData submission))) ]
                        [ text (gettext "View error" appState.locale) ]

                _ ->
                    emptyNode
    in
    tr []
        [ td [] [ text (Submission.visibleName submission) ]
        , td [] [ viewSubmissionState submission.state ]
        , td []
            [ span [ class "fragment-user" ]
                [ UserIcon.viewSmall { gravatarHash = submission.createdBy.gravatarHash, imageUrl = submission.createdBy.imageUrl }
                , text (User.fullName submission.createdBy)
                ]
            ]
        , td [] [ link ]
        , td [] [ span (class "timestamp" :: tooltip readableTime) [ text updatedText ] ]
        ]


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
                (String.formatHtml (gettext "Are you sure you want to permanently delete %s?" appState.locale) [ strong [] [ text name ] ])
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete document" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingDocument
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteDocument
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteDocument Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "document-delete"
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
                    [ text (gettext "Done" appState.locale) ]

            else if ActionResult.isSuccess model.submissionServices && Maybe.isJust model.selectedSubmissionServiceId then
                ActionButton.button appState
                    { label = gettext "Submit" appState.locale
                    , result = model.submittingDocument
                    , msg = SubmitDocument
                    , dangerous = False
                    }

            else
                button [ class "btn btn-primary", disabled True ]
                    [ text (gettext "Submit" appState.locale) ]

        cancelButton =
            button [ onClick <| ShowHideSubmitDocument Nothing, class "btn btn-secondary", disabled <| ActionResult.isLoading model.submittingDocument ]
                [ text (gettext "Cancel" appState.locale) ]

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
                Flash.info appState <| gettext "There are no submission services configured for this type of document." appState.locale

        submissionBody submissionServices =
            div []
                [ FormResult.errorOnlyView appState model.submittingDocument
                , options submissionServices
                ]

        resultBody submission =
            case submission.state of
                SubmissionState.Done ->
                    let
                        link =
                            case submission.location of
                                Just location ->
                                    div [ class "mt-2" ]
                                        [ text (gettext "You can find it here: " appState.locale)
                                        , a [ href location, target "_blank" ]
                                            [ text location ]
                                        ]

                                Nothing ->
                                    emptyNode
                    in
                    div [ class "alert alert-success" ]
                        [ faSet "_global.success" appState
                        , text (gettext "The document was successfully submitted." appState.locale)
                        , link
                        ]

                SubmissionState.Error ->
                    div [ class "alert alert-danger" ]
                        [ faSet "_global.error" appState
                        , text (gettext "The document submission failed." appState.locale)
                        ]

                _ ->
                    emptyNode

        body =
            if ActionResult.isSuccess model.submittingDocument then
                ActionResultBlock.view appState resultBody model.submittingDocument

            else
                ActionResultBlock.view appState submissionBody model.submissionServices

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text <| String.format (gettext "Submit %s" appState.locale) [ name ] ]
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


documentErrorModal : AppState -> Model -> Html Msg
documentErrorModal appState model =
    let
        ( visible, message ) =
            case model.documentErrorModal of
                Just error ->
                    ( True, error )

                Nothing ->
                    ( False, "" )

        modalConfig =
            { title = gettext "Document error" appState.locale
            , message = message
            , visible = visible
            , actionMsg = SetDocumentErrorModal Nothing
            , dataCy = "document-error"
            }
    in
    Modal.error appState modalConfig


submissionErrorModal : AppState -> Model -> Html Msg
submissionErrorModal appState model =
    let
        ( visible, message ) =
            case model.submissionErrorModal of
                Just error ->
                    ( True, error )

                Nothing ->
                    ( False, "" )

        modalConfig =
            { title = gettext "Submission error" appState.locale
            , message = message
            , visible = visible
            , actionMsg = SetSubmissionErrorModal Nothing
            , dataCy = "submission-error"
            }
    in
    Modal.error appState modalConfig
