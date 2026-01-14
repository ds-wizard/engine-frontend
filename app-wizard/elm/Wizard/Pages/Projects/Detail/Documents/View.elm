module Wizard.Pages.Projects.Detail.Documents.View exposing (ViewConfig, view)

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Badge as Badge
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (fa, faDelete, faDocumentsDownload, faDocumentsSubmit, faDocumentsViewError, faExternalLink, faQuestionnaire, faSpinner)
import Common.Components.FormResult as FormResult
import Common.Components.GuideLink as GuideLink
import Common.Components.Modal as Modal
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
import Uuid exposing (Uuid)
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
import Wizard.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Pages.Documents.Common.DocumentPluginActions as DocumentPluginActions
import Wizard.Pages.Projects.Detail.Documents.Models exposing (Model)
import Wizard.Pages.Projects.Detail.Documents.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Pages.Projects.Routes exposing (Route(..))
import Wizard.Plugins.PluginElement as PluginElement
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


type alias ViewConfig msg =
    { project : ProjectCommon
    , projectEditable : Bool
    , wrapMsg : Msg -> msg
    , previewProjectEventMsg : Maybe (Uuid -> msg)
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    div [ class "Projects__Detail__Content" ]
        [ div [ class "container my-4" ]
            [ FormResult.successOnlyView model.deletingDocument
            , Listing.view appState (listingConfig cfg appState) model.documents
            , deleteModal cfg appState model
            , submitModal cfg appState model
            , documentErrorModal cfg appState model
            , submissionErrorModal cfg appState model
            , pluginModal cfg appState model
            ]
        ]


listingConfig : ViewConfig msg -> AppState -> Listing.ViewConfig Document msg
listingConfig cfg appState =
    let
        itemAdditionalData document =
            if List.isEmpty document.submissions then
                Nothing

            else
                Just <|
                    [ strong [] [ text (gettext "Submissions" appState.locale) ]
                    , table [ class "table table-sm" ]
                        [ tbody []
                            (List.map (viewSubmission cfg appState) (List.sortWith Submission.compare document.submissions))
                        ]
                    ]
    in
    { title = listingTitle cfg appState
    , description = listingDescription
    , itemAdditionalData = itemAdditionalData
    , dropdownItems = listingActions appState cfg
    , textTitle = .name
    , emptyText =
        if Session.exists appState.session then
            gettext "Click \"New document\" button to add a new document." appState.locale

        else
            gettext "Log in to add a new document." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Nothing
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.ProjectsRoute << DetailRoute cfg.project.uuid << PlanDetailRoute.Documents
    , toolbarExtra =
        if cfg.projectEditable && Session.exists appState.session then
            Just <|
                linkTo (Routes.projectsDetailDocumentsNew cfg.project.uuid Nothing)
                    [ class "btn btn-primary" ]
                    [ text (gettext "New document" appState.locale) ]

        else
            Nothing
    }


listingTitle : ViewConfig msg -> AppState -> Document -> Html msg
listingTitle cfg appState document =
    let
        ( name, downloadTooltip ) =
            if document.state == DoneDocumentState then
                ( a
                    [ onClick (cfg.wrapMsg <| DownloadDocument document) ]
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


listingDescription : Document -> Html msg
listingDescription document =
    let
        viewVersion versionName =
            span [ class "fragment" ]
                [ QuestionnaireVersionTag.version { name = versionName }
                ]

        formatFragment =
            span [ class "fragment" ] [ fa document.format.icon, text document.format.name ]

        fileSizeFragment =
            case document.fileSize of
                Just fileSize ->
                    span [ class "fragment" ] [ text (ByteUnits.toReadable fileSize) ]

                Nothing ->
                    Html.nothing

        versionFragment =
            Maybe.unwrap Html.nothing viewVersion document.projectVersion
    in
    span []
        [ formatFragment
        , fileSizeFragment
        , span [ class "fragment" ] [ text document.documentTemplateName ]
        , versionFragment
        ]


listingActions : AppState -> ViewConfig msg -> Document -> List (ListingDropdownItem msg)
listingActions appState cfg document =
    let
        downloadEnabled =
            document.state == DoneDocumentState

        download =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentsDownload
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| DownloadDocument document)
                , dataCy = "download"
                }

        submitEnabled =
            Feature.documentSubmit appState document && cfg.projectEditable

        submit =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentsSubmit
                , label = gettext "Submit" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        ( viewQuestionnaire, viewQuestionnaireEnabled ) =
            case ( document.projectEventUuid, cfg.previewProjectEventMsg ) of
                ( Just questionnaireEventUuid, Just previewQuestionnaireEventMsg ) ->
                    ( ListingDropdown.dropdownAction
                        { extraClass = Nothing
                        , icon = faQuestionnaire
                        , label = gettext "View questionnaire" appState.locale
                        , msg = ListingActionMsg (previewQuestionnaireEventMsg questionnaireEventUuid)
                        , dataCy = "view-questionnaire"
                        }
                    , True
                    )

                _ ->
                    ( ListingDropdown.dropdownSeparator, False )

        viewErrorEnabled =
            Maybe.isJust document.workerLog && document.state == ErrorDocumentState

        viewError =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faDocumentsViewError
                , label = gettext "View error" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| SetDocumentErrorModal document.workerLog)
                , dataCy = "view-error"
                }

        pluginActions =
            DocumentPluginActions.documentPluginActions appState document (cfg.wrapMsg << PluginModalMsg)

        deleteEnabled =
            cfg.projectEditable && Session.exists appState.session

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }

        groups =
            [ [ ( download, downloadEnabled )
              , ( submit, submitEnabled )
              ]
            , [ ( viewError, viewErrorEnabled ) ]
            , [ ( viewQuestionnaire, viewQuestionnaireEnabled ) ]
            , pluginActions
            , [ ( delete, deleteEnabled ) ]
            ]
    in
    ListingDropdown.itemsFromGroups groups


stateBadge : AppState -> DocumentState -> Html msg
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            Badge.info [ dataCy "badge_doc_queued" ]
                [ faSpinner
                , text (gettext "Queued" appState.locale)
                ]

        InProgressDocumentState ->
            Badge.info [ dataCy "badge_doc_in-progress" ]
                [ faSpinner
                , text (gettext "In Progress" appState.locale)
                ]

        DoneDocumentState ->
            Html.nothing

        ErrorDocumentState ->
            Badge.danger [ dataCy "badge_doc_error" ] [ text (gettext "Error" appState.locale) ]


viewSubmission : ViewConfig msg -> AppState -> Submission -> Html msg
viewSubmission cfg appState submission =
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
                    a [ onClick (cfg.wrapMsg <| SetSubmissionErrorModal (Just (Submission.getReturnedData submission))) ]
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


deleteModal : ViewConfig msg -> AppState -> Model -> Html msg
deleteModal cfg appState model =
    let
        ( visible, name ) =
            case model.documentToBeDeleted of
                Just document ->
                    ( True, document.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text name ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete document" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingDocument
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) (cfg.wrapMsg DeleteDocument)
                |> Modal.confirmConfigCancelMsg (cfg.wrapMsg (ShowHideDeleteDocument Nothing))
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "documents-delete"
    in
    Modal.confirm appState modalConfig


submitModal : ViewConfig msg -> AppState -> Model -> Html msg
submitModal cfg appState model =
    let
        ( visible, name ) =
            case model.documentToBeSubmitted of
                Just document ->
                    ( True, document.name )

                Nothing ->
                    ( False, "" )

        submitButton =
            if ActionResult.isSuccess model.submittingDocument then
                button [ class "btn btn-primary", onClick <| cfg.wrapMsg <| ShowHideSubmitDocument Nothing ]
                    [ text (gettext "Done" appState.locale) ]

            else if ActionResult.isSuccess model.submissionServices && Maybe.isJust model.selectedSubmissionServiceId then
                ActionButton.button
                    { label = gettext "Submit" appState.locale
                    , result = model.submittingDocument
                    , msg = cfg.wrapMsg <| SubmitDocument
                    , dangerous = False
                    }

            else
                button [ class "btn btn-primary", disabled True ]
                    [ text (gettext "Submit" appState.locale) ]

        cancelButton =
            button [ onClick <| cfg.wrapMsg <| ShowHideSubmitDocument Nothing, class "btn btn-secondary", disabled <| ActionResult.isLoading model.submittingDocument ]
                [ text (gettext "Cancel" appState.locale) ]

        viewOption submissionService =
            div [ class "form-check", classList [ ( "form-check-selected", model.selectedSubmissionServiceId == Just submissionService.id ) ] ]
                [ input
                    [ type_ "radio"
                    , class "form-check-input"
                    , id submissionService.id
                    , checked (model.selectedSubmissionServiceId == Just submissionService.id)
                    , onCheck (\_ -> cfg.wrapMsg <| SelectSubmissionService submissionService.id)
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
                ( Just (cfg.wrapMsg SubmitDocument), Just (cfg.wrapMsg (ShowHideSubmitDocument Nothing)) )

        modalConfig =
            { modalContent = content
            , visible = visible
            , enterMsg = enterMsg
            , escMsg = escMsg
            , dataCy = "document-submit"
            }
    in
    Modal.simple modalConfig


documentErrorModal : ViewConfig msg -> AppState -> Model -> Html msg
documentErrorModal cfg appState model =
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
            , actionMsg = cfg.wrapMsg (SetDocumentErrorModal Nothing)
            , dataCy = "document-error"
            , locale = appState.locale
            }
    in
    Modal.error modalConfig


submissionErrorModal : ViewConfig msg -> AppState -> Model -> Html msg
submissionErrorModal cfg appState model =
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
            , actionMsg = cfg.wrapMsg (SetSubmissionErrorModal Nothing)
            , dataCy = "submission-error"
            , locale = appState.locale
            }
    in
    Modal.error modalConfig


pluginModal : ViewConfig msg -> AppState -> Model -> Html msg
pluginModal cfg appState model =
    let
        pluginModalViewConfig =
            { attributes =
                \document ->
                    [ PluginElement.documentValue document
                    ]
            , wrapMsg = cfg.wrapMsg << PluginModalMsg
            }
    in
    PluginModal.view appState pluginModalViewConfig model.pluginModal
