module Wizard.Projects.Detail.Documents.View exposing (ViewConfig, view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, input, label, p, span, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, type_)
import Html.Events exposing (onCheck, onClick)
import Maybe.Extra as Maybe
import Shared.Auth.Session as Session
import Shared.Common.ByteUnits as ByteUnits
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Submission as Submission exposing (Submission)
import Shared.Data.Submission.SubmissionState as SubmissionState
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listInsertIf)
import String.Format as String
import Time.Distance as TimeDistance
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip, tooltipCustom)
import Wizard.Common.TimeDistance as TimeDistance
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Projects.Detail.Documents.Models exposing (Model)
import Wizard.Projects.Detail.Documents.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))


type alias ViewConfig msg =
    { questionnaire : QuestionnaireDetail
    , questionnaireEditable : Bool
    , wrapMsg : Msg -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Documents" ]
        [ div [ class "container" ]
            [ FormResult.successOnlyView appState model.deletingDocument
            , Listing.view appState (listingConfig cfg appState) model.documents
            , deleteModal cfg appState model
            , submitModal cfg appState model
            , documentErrorModal cfg appState model
            , submissionErrorModal cfg appState model
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
    , description = listingDescription cfg appState
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
    , toRoute = \_ -> Routes.ProjectsRoute << DetailRoute cfg.questionnaire.uuid << PlanDetailRoute.Documents
    , toolbarExtra =
        if cfg.questionnaireEditable && Session.exists appState.session then
            Just <|
                linkTo appState
                    (Routes.projectsDetailDocumentsNew cfg.questionnaire.uuid Nothing)
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


listingDescription : ViewConfig msg -> AppState -> Document -> Html msg
listingDescription cfg _ document =
    let
        ( icon, formatName ) =
            case Document.getFormat document of
                Just format ->
                    ( fa format.icon, format.name )

                Nothing ->
                    ( emptyNode, "" )

        viewVersion version =
            span [ class "fragment" ]
                [ QuestionnaireVersionTag.version version
                ]

        fileSizeFragment =
            case document.fileSize of
                Just fileSize ->
                    span [ class "fragment" ] [ text (ByteUnits.toReadable fileSize) ]

                Nothing ->
                    emptyNode

        versionFragment =
            document.questionnaireEventUuid
                |> Maybe.andThen (QuestionnaireDetail.getVersionByEventUuid cfg.questionnaire)
                |> Maybe.unwrap emptyNode viewVersion
    in
    span []
        [ span [ class "fragment" ] [ icon, text formatName ]
        , fileSizeFragment
        , span [ class "fragment" ] [ text document.documentTemplate.name ]
        , versionFragment
        ]


listingActions : AppState -> ViewConfig msg -> Document -> List (ListingDropdownItem msg)
listingActions appState cfg document =
    let
        downloadEnabled =
            document.state == DoneDocumentState

        download =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.download" appState
                , label = gettext "Download" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| DownloadDocument document)
                , dataCy = "download"
                }

        submitEnabled =
            Feature.documentSubmit appState document && cfg.questionnaireEditable

        submit =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.submit" appState
                , label = gettext "Submit" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        ( viewQuestionnaire, viewQuestionnaireEnabled ) =
            case ( document.questionnaireEventUuid, cfg.previewQuestionnaireEventMsg ) of
                ( Just questionnaireEventUuid, Just previewQuestionnaireEventMsg ) ->
                    ( Listing.dropdownAction
                        { extraClass = Nothing
                        , icon = faSet "_global.questionnaire" appState
                        , label = gettext "View questionnaire" appState.locale
                        , msg = ListingActionMsg (previewQuestionnaireEventMsg questionnaireEventUuid)
                        , dataCy = "view-questionnaire"
                        }
                    , True
                    )

                _ ->
                    ( Listing.dropdownSeparator, False )

        viewErrorEnabled =
            Maybe.isJust document.workerLog && document.state == ErrorDocumentState

        viewError =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "documents.viewError" appState
                , label = "View error"
                , msg = ListingActionMsg (cfg.wrapMsg <| SetDocumentErrorModal document.workerLog)
                , dataCy = "view-error"
                }

        deleteEnabled =
            cfg.questionnaireEditable && Session.exists appState.session

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }
    in
    []
        |> listInsertIf download downloadEnabled
        |> listInsertIf submit submitEnabled
        |> listInsertIf viewError viewErrorEnabled
        |> listInsertIf Listing.dropdownSeparator ((downloadEnabled || submitEnabled || viewErrorEnabled) && viewQuestionnaireEnabled)
        |> listInsertIf viewQuestionnaire viewQuestionnaireEnabled
        |> listInsertIf Listing.dropdownSeparator deleteEnabled
        |> listInsertIf delete deleteEnabled


stateBadge : AppState -> DocumentState -> Html msg
stateBadge appState state =
    case state of
        QueuedDocumentState ->
            Badge.info [ dataCy "badge_doc_queued" ]
                [ faSet "_global.spinner" appState
                , text (gettext "Queued" appState.locale)
                ]

        InProgressDocumentState ->
            Badge.info [ dataCy "badge_doc_in-progress" ]
                [ faSet "_global.spinner" appState
                , text (gettext "In Progress" appState.locale)
                ]

        DoneDocumentState ->
            emptyNode

        ErrorDocumentState ->
            Badge.danger [ dataCy "badge_doc_error" ] [ text (gettext "Error" appState.locale) ]


viewSubmission : ViewConfig msg -> AppState -> Submission -> Html msg
viewSubmission cfg appState submission =
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
                    a [ onClick (cfg.wrapMsg <| SetSubmissionErrorModal (Just (Submission.getReturnedData submission))) ]
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
            { modalTitle = gettext "Delete document" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingDocument
            , actionName = gettext "Delete" appState.locale
            , actionMsg = cfg.wrapMsg <| DeleteDocument
            , cancelMsg = Just <| cfg.wrapMsg <| ShowHideDeleteDocument Nothing
            , dangerous = True
            , dataCy = "documents-delete"
            }
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
                ActionButton.button appState
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
            }
    in
    Modal.error appState modalConfig


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
            }
    in
    Modal.error appState modalConfig
