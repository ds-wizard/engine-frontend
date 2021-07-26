module Wizard.Projects.Detail.Documents.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, disabled, for, href, id, target, title, type_)
import Html.Events exposing (onCheck, onClick)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Shared.Api.Documents as DocumentsApi
import Shared.Auth.Permission as Perm
import Shared.Auth.Session as Session
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lf, lg, lh, lx)
import Shared.Utils exposing (listInsertIf)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Components.QuestionnaireVersionTag as QuestionnaireVersionTag
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Detail.Documents.Models exposing (Model)
import Wizard.Projects.Detail.Documents.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Documents.View"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Projects.Detail.Documents.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Documents.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Detail.Documents.View"


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
            ]
        ]


listingConfig : ViewConfig msg -> AppState -> Listing.ViewConfig Document msg
listingConfig cfg appState =
    { title = listingTitle appState
    , description = listingDescription cfg appState
    , dropdownItems = listingActions appState cfg
    , textTitle = .name
    , emptyText =
        if Session.exists appState.session then
            l_ "listing.empty" appState

        else
            l_ "listing.emptyAnonymous" appState
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , iconView = Nothing
    , sortOptions =
        [ ( "name", lg "document.name" appState )
        , ( "createdAt", lg "document.createdAt" appState )
        ]
    , filters = []
    , toRoute = \_ -> Routes.ProjectsRoute << DetailRoute cfg.questionnaire.uuid << PlanDetailRoute.Documents
    , toolbarExtra =
        if cfg.questionnaireEditable && Session.exists appState.session then
            Just <|
                linkTo appState
                    (Routes.ProjectsRoute <| DetailRoute cfg.questionnaire.uuid <| PlanDetailRoute.NewDocument Nothing)
                    [ class "btn btn-primary" ]
                    [ lx_ "newDocument" appState ]

        else
            Nothing
    }


listingTitle : AppState -> Document -> Html msg
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


listingDescription : ViewConfig msg -> AppState -> Document -> Html msg
listingDescription cfg appState document =
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

        versionFragment =
            document.questionnaireEventUuid
                |> Maybe.andThen (QuestionnaireDetail.getVersionByEventUuid cfg.questionnaire)
                |> Maybe.unwrap emptyNode viewVersion
    in
    span []
        [ span [ class "fragment" ] [ icon, text formatName ]
        , span [ class "fragment" ] [ text document.template.name ]
        , versionFragment
        ]


listingActions : AppState -> ViewConfig msg -> Document -> List (ListingDropdownItem msg)
listingActions appState cfg document =
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
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideSubmitDocument <| Just document)
                , dataCy = "submit"
                }

        ( viewQuestionnaire, viewQuestionnaireEnabled ) =
            case ( document.questionnaireEventUuid, cfg.previewQuestionnaireEventMsg ) of
                ( Just questionnaireEventUuid, Just previewQuestionnaireEventMsg ) ->
                    ( Listing.dropdownAction
                        { extraClass = Nothing
                        , icon = faSet "_global.questionnaire" appState
                        , label = l_ "action.viewQuestionnaire" appState
                        , msg = ListingActionMsg (previewQuestionnaireEventMsg questionnaireEventUuid)
                        , dataCy = "view-questionnaire"
                        }
                    , True
                    )

                _ ->
                    ( Listing.dropdownSeparator, False )

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg = ListingActionMsg (cfg.wrapMsg <| ShowHideDeleteDocument <| Just document)
                , dataCy = "delete"
                }

        submitEnabled =
            (document.state == DoneDocumentState)
                && appState.config.submission.enabled
                && Perm.hasPerm appState.session Perm.submission
    in
    []
        |> listInsertIf download (document.state == DoneDocumentState)
        |> listInsertIf submit submitEnabled
        |> listInsertIf Listing.dropdownSeparator (cfg.questionnaireEditable && document.state == DoneDocumentState)
        |> listInsertIf viewQuestionnaire viewQuestionnaireEnabled
        |> listInsertIf Listing.dropdownSeparator viewQuestionnaireEnabled
        |> listInsertIf delete cfg.questionnaireEditable


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
                (lh_ "deleteModal.message" [ strong [] [ text name ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingDocument
            , actionName = l_ "deleteModal.action" appState
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
                    [ lx_ "submitModal.button.done" appState ]

            else if ActionResult.isSuccess model.submissionServices && Maybe.isJust model.selectedSubmissionServiceId then
                ActionButton.button appState
                    { label = l_ "submitModal.button.submit" appState
                    , result = model.submittingDocument
                    , msg = cfg.wrapMsg <| SubmitDocument
                    , dangerous = False
                    }

            else
                button [ class "btn btn-primary", disabled True ]
                    [ lx_ "submitModal.button.submit" appState ]

        cancelButton =
            button [ onClick <| cfg.wrapMsg <| ShowHideSubmitDocument Nothing, class "btn btn-secondary", disabled <| ActionResult.isLoading model.submittingDocument ]
                [ lx_ "submitModal.button.cancel" appState ]

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
