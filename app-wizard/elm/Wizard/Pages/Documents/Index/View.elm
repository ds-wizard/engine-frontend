module Wizard.Pages.Documents.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.ActionButton as ActionButton
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Badge as Badge
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (fa, faDelete, faDocumentsDownload, faDocumentsSubmit, faDocumentsViewError, faExternalLink, faRemove, faSpinner)
import Common.Components.FormResult as FormResult
import Common.Components.GuideLink as GuideLink
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltip, tooltipCustom)
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.Markdown as Markdown
import Common.Utils.TimeDistance as TimeDistance
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, input, label, p, span, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onCheck, onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Time.Distance as TimeDistance
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.Submission as Submission exposing (Submission)
import Wizard.Api.Models.Submission.SubmissionState as SubmissionState
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Components.PluginModal as PluginModal
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Documents.Common.DocumentPluginActions as DocumentPluginActions
import Wizard.Pages.Documents.Index.Models exposing (Model)
import Wizard.Pages.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Pages.Documents.Routes exposing (Route(..))
import Wizard.Plugins.PluginElement as PluginElement
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    let
        projectActionResult =
            case model.project of
                Just project ->
                    ActionResult.map Just project

                Nothing ->
                    Success Nothing
    in
    Page.actionResultView appState (viewDocuments appState model) projectActionResult


viewDocuments : AppState -> Model -> Maybe ProjectCommon -> Html Msg
viewDocuments appState model mbProject =
    let
        projectFilterView project =
            div [ class "listing-toolbar-extra project-filter" ]
                [ linkTo (Routes.projectsDetail project.uuid)
                    [ class "project-name" ]
                    [ text project.name ]
                , linkTo Routes.documentsIndex
                    [ class "text-danger" ]
                    [ faRemove ]
                ]

        mbProjectFilterView =
            Maybe.map projectFilterView mbProject
    in
    div [ listClass "Documents__Index" ]
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsDocuments) (gettext "Project Documents" appState.locale)
        , Listing.view appState (listingConfig appState model mbProjectFilterView) model.documents
        , deleteModal appState model
        , submitModal appState model
        , documentErrorModal appState model
        , submissionErrorModal appState model
        , pluginModal appState model
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
    , description = listingDescription
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
    , toRoute = \_ -> Routes.DocumentsRoute << IndexRoute model.projectUuid
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


listingDescription : Document -> Html Msg
listingDescription document =
    let
        projectLink =
            case document.project of
                Just project ->
                    span [ class "fragment" ]
                        [ linkTo (Routes.projectsDetail project.uuid)
                            []
                            [ text project.name ]
                        ]

                Nothing ->
                    Html.nothing

        formatFragment =
            span [ class "fragment" ] [ fa document.format.icon, text document.format.name ]

        fileSizeFragment =
            case document.fileSize of
                Just fileSize ->
                    span [ class "fragment" ] [ text (ByteUnits.toReadable fileSize) ]

                Nothing ->
                    Html.nothing

        documentTemplateLink =
            span [ class "fragment" ]
                [ linkTo (Routes.documentTemplatesDetail document.documentTemplateId)
                    []
                    [ text document.documentTemplateName ]
                ]
    in
    span []
        [ formatFragment
        , fileSizeFragment
        , projectLink
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
                , icon = faDocumentsDownload
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (DownloadDocument document)
                , dataCy = "download"
                }

        submitEnabled =
            Feature.documentSubmit appState document

        submit =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentsSubmit
                , label = gettext "Submit" appState.locale
                , msg = ListingActionMsg (ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        viewErrorEnabled =
            Maybe.isJust document.workerLog && document.state == ErrorDocumentState

        viewError =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentsViewError
                , label = gettext "View error" appState.locale
                , msg = ListingActionMsg (SetDocumentErrorModal document.workerLog)
                , dataCy = "view-error"
                }

        pluginActions =
            DocumentPluginActions.documentPluginActions appState document PluginModalMsg

        deleteEnabled =
            Feature.documentDelete appState document

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }

        groups =
            [ [ ( download, downloadEnabled )
              , ( submit, submitEnabled )
              , ( viewError, viewErrorEnabled )
              ]
            , pluginActions
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
                [ faSpinner
                , text (gettext "Queued" appState.locale)
                ]

        InProgressDocumentState ->
            Badge.info [ dataCy "documents_state-badge" ]
                [ faSpinner
                , text (gettext "In Progress" appState.locale)
                ]

        DoneDocumentState ->
            Html.nothing

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
                        [ faSpinner
                        , text (gettext "Submitting" appState.locale)
                        ]

                SubmissionState.Done ->
                    Badge.success [] [ text (gettext "Submitted" appState.locale) ]

                SubmissionState.Error ->
                    Badge.danger [] [ text (gettext "Error" appState.locale) ]

        readableTime =
            TimeUtils.toReadableDateTime appState.timeZone submission.updatedAt

        updatedText =
            TimeDistance.inWordsWithConfig { withAffix = True } (TimeDistance.locale appState.locale) submission.updatedAt appState.currentTime

        link =
            case ( submission.state, submission.location, submission.returnedData ) of
                ( SubmissionState.Done, Just location, _ ) ->
                    a [ href location, class "with-icon-after", target "_blank" ]
                        [ text (gettext "View submission" appState.locale)
                        , faExternalLink
                        ]

                ( SubmissionState.Error, _, Just _ ) ->
                    a [ onClick (SetSubmissionErrorModal (Just (Submission.getReturnedData submission))) ]
                        [ text (gettext "View error" appState.locale) ]

                _ ->
                    Html.nothing
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
                ActionButton.button
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
            if List.isEmpty submissionServices then
                Flash.info <| gettext "There are no submission services configured for this type of document." appState.locale

            else
                div [ class "form-radio-group" ]
                    (List.map viewOption submissionServices)

        submissionBody submissionServices =
            div []
                [ FormResult.errorOnlyView model.submittingDocument
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
                                    Html.nothing
                    in
                    Flash.successHtml
                        (div [ class "ms-2" ]
                            [ text (gettext "The document was successfully submitted." appState.locale)
                            , link
                            ]
                        )

                SubmissionState.Error ->
                    Flash.error (gettext "The document submission failed." appState.locale)

                _ ->
                    Html.nothing

        body =
            if ActionResult.isSuccess model.submittingDocument then
                ActionResultBlock.view
                    { viewContent = resultBody
                    , actionResult = model.submittingDocument
                    , locale = appState.locale
                    }

            else
                ActionResultBlock.view
                    { viewContent = submissionBody
                    , actionResult = model.submissionServices
                    , locale = appState.locale
                    }

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text <| String.format (gettext "Submit %s" appState.locale) [ name ] ]
                , GuideLink.guideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsDocumentSubmission)
                ]
            , div [ class "modal-body" ]
                [ body
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        ( enterMsg, escMsg ) =
            if ActionResult.isLoading model.submittingDocument then
                ( Nothing, Nothing )

            else
                ( Just SubmitDocument, Just (ShowHideSubmitDocument Nothing) )

        modalConfig =
            { modalContent = content
            , visible = visible
            , enterMsg = enterMsg
            , escMsg = escMsg
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
            , locale = appState.locale
            }
    in
    Modal.error modalConfig


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
            , locale = appState.locale
            }
    in
    Modal.error modalConfig


pluginModal : AppState -> Model -> Html Msg
pluginModal appState model =
    let
        pluginModalViewConfig =
            { attributes =
                \document ->
                    [ PluginElement.documentValue document
                    ]
            , wrapMsg = PluginModalMsg
            }
    in
    PluginModal.view appState pluginModalViewConfig model.pluginModal
