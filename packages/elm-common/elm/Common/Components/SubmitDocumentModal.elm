module Common.Components.SubmitDocumentModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

{-| Submit a document to one of the submission services configured for its
kind: choose the service, send it, and see where it ended up.

A form dialog (`Ui.Modal.withForm`): Cmd/Ctrl+Enter sends it, and the submit
button is busy while the request is out. The services are asked for when the
dialog opens, and the first one is chosen. A failed submission is said inside the
dialog above the choice, so it can be sent again; a finished one replaces the
choice with the outcome and a "Done" button.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Documents as DocumentsApi
import Common.Api.Models.Document exposing (Document)
import Common.Api.Models.Submission exposing (Submission)
import Common.Api.Models.Submission.SubmissionState as SubmissionState
import Common.Api.Models.SubmissionService exposing (SubmissionService)
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task.Extra as Task
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Field as Field
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Modal as Modal
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { documentToBeSubmitted : Maybe Document
    , submittingDocument : ActionResult Submission
    , submissionServices : ActionResult (List SubmissionService)
    , selectedSubmissionServiceId : Maybe String
    }


initialModel : Model
initialModel =
    { documentToBeSubmitted = Nothing
    , submittingDocument = Unset
    , submissionServices = Unset
    , selectedSubmissionServiceId = Nothing
    }


type Msg
    = ShowHideSubmitDocument (Maybe Document)
    | GetSubmissionServicesCompleted (Result ApiError (List SubmissionService))
    | SelectSubmissionService String
    | SubmitDocument
    | SubmitDocumentCompleted (Result ApiError Submission)


open : Document -> Msg
open =
    ShowHideSubmitDocument << Just


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , submittedMsg : Submission -> msg
    , closeCmd : Uuid -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ShowHideSubmitDocument mbDocument ->
            ( { model
                | documentToBeSubmitted = mbDocument
                , submittingDocument = Unset
                , submissionServices = Loading
                , selectedSubmissionServiceId = Nothing
              }
            , case ( mbDocument, model.documentToBeSubmitted ) of
                ( Just document, _ ) ->
                    DocumentsApi.getSubmissionServices appState (Uuid.toString document.uuid) (cfg.wrapMsg << GetSubmissionServicesCompleted)

                ( Nothing, Just document ) ->
                    -- Closed: `closeCmd` returns focus to where it was opened
                    -- from. The dialog reports closing again once it has gone.
                    cfg.closeCmd document.uuid

                ( Nothing, Nothing ) ->
                    Cmd.none
            )

        GetSubmissionServicesCompleted result ->
            RequestHelpers.applyResult
                { setResult =
                    \value record ->
                        { record
                            | submissionServices = value
                            , selectedSubmissionServiceId =
                                value
                                    |> ActionResult.map (Maybe.map .id << List.head)
                                    |> ActionResult.withDefault Nothing
                        }
                , defaultError = gettext "Unable to get submission services for the document." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SelectSubmissionService id ->
            ( { model | selectedSubmissionServiceId = Just id }, Cmd.none )

        SubmitDocument ->
            case ( model.documentToBeSubmitted, model.selectedSubmissionServiceId, model.submittingDocument ) of
                ( _, _, Loading ) ->
                    ( model, Cmd.none )

                ( _, _, Success _ ) ->
                    ( model, Cmd.none )

                ( Just document, Just serviceId, _ ) ->
                    ( { model | submittingDocument = Loading }
                    , DocumentsApi.postSubmission appState serviceId (Uuid.toString document.uuid) (cfg.wrapMsg << SubmitDocumentCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitDocumentCompleted result ->
            let
                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = \value record -> { record | submittingDocument = value }
                        , defaultError = gettext "Unable to submit the document." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , locale = appState.locale
                        }
            in
            case result of
                Ok submission ->
                    ( newModel, Cmd.batch [ cmd, Task.dispatch (cfg.submittedMsg submission) ] )

                Err _ ->
                    ( newModel, cmd )


view : AppState -> Model -> Html Msg
view appState model =
    let
        name =
            Maybe.unwrap "" .name model.documentToBeSubmitted

        close =
            ShowHideSubmitDocument Nothing

        buttons =
            case ( model.submittingDocument, model.submissionServices ) of
                ( Success _, _ ) ->
                    [ Button.primary (gettext "Done" appState.locale) close ]

                ( _, Success (_ :: _) ) ->
                    let
                        submitting =
                            ActionResult.isLoading model.submittingDocument
                    in
                    -- Cancel first and the action last, as every other dialog's
                    -- footer has them.
                    [ Button.ghost (gettext "Cancel" appState.locale) close
                        |> Button.withDisabled submitting
                    , Button.submit (gettext "Submit" appState.locale)
                        |> Button.withLoading submitting
                    ]

                _ ->
                    -- Nothing to submit to: closing is all there is to do.
                    [ Button.primary (gettext "Close" appState.locale) close ]

        content =
            case model.submittingDocument of
                Success submission ->
                    viewResult appState submission

                _ ->
                    viewServices appState model
    in
    Modal.plain
        { title = String.format (gettext "Submit %s" appState.locale) [ name ]
        , open = Maybe.isJust model.documentToBeSubmitted
        , onClose = close
        , body = [ content ]
        }
        |> Modal.withForm { onSubmit = SubmitDocument, submitLabel = gettext "Submit" appState.locale }
        |> Modal.withActions buttons
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsDocumentSubmission)
        |> Modal.withTestId "document-submit"
        |> Modal.toHtml


viewServices : AppState -> Model -> Html Msg
viewServices appState model =
    case model.submissionServices of
        Success [] ->
            Alert.toHtml (Alert.info (gettext "There are no submission services configured for this type of document." appState.locale))

        Success submissionServices ->
            let
                serviceChoice service =
                    Field.choice { value = service.id, label = service.name }
                        |> Field.withChoiceDescription service.description

                errorAlert =
                    case model.submittingDocument of
                        Error message ->
                            Alert.toHtml (Alert.danger message)

                        _ ->
                            Html.text ""
            in
            Layout.stack
                [ errorAlert
                , Field.radioGroup
                    { id = "document-submission-service"
                    , label = gettext "Submission service" appState.locale
                    , value = Maybe.withDefault "" model.selectedSubmissionServiceId
                    , options = List.map serviceChoice submissionServices
                    , onChange = SelectSubmissionService
                    }
                    |> Field.withDisabled (ActionResult.isLoading model.submittingDocument)
                    |> Field.toHtml
                ]

        Error message ->
            Alert.toHtml (Alert.danger message)

        _ ->
            AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }


viewResult : AppState -> Submission -> Html Msg
viewResult appState submission =
    case submission.state of
        SubmissionState.Done ->
            Layout.stack
                [ Alert.toHtml (Alert.success (gettext "The document was successfully submitted." appState.locale))
                , case submission.location of
                    Just location ->
                        Layout.inline
                            [ Text.body (gettext "You can find it here: " appState.locale)
                            , Link.link { label = location, url = location }
                                |> Link.external
                                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                                |> Link.toHtml
                            ]

                    Nothing ->
                        Html.text ""
                ]

        SubmissionState.Error ->
            Alert.toHtml (Alert.danger (gettext "The document submission failed." appState.locale))

        SubmissionState.InProgress ->
            Html.text ""
