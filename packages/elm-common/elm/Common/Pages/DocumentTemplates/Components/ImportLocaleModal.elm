module Common.Pages.DocumentTemplates.Components.ImportLocaleModal exposing
    ( ImportLocaleForm
    , Model
    , Msg
    , init
    , open
    , subscriptions
    , update
    , view
    )

{-| Importing a `.po` file as a locale of a document template.

Import is never disabled on validity: submitting without a file marks the upload
as invalid and says so, next to the name's own error.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.FileUpload as FileUpload
import Ui.Layout as Layout
import Ui.Modal as Modal
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { visible : Bool
    , documentTemplateUuid : Maybe Uuid
    , form : Form FormError ImportLocaleForm
    , upload : FileUpload.Model
    , importResult : ActionResult ()
    }


type alias ImportLocaleForm =
    { name : String }


formValidation : Validation FormError ImportLocaleForm
formValidation =
    Validate.map ImportLocaleForm
        (Validate.field "name" Validate.string)


init : Model
init =
    { visible = False
    , documentTemplateUuid = Nothing
    , form = Form.initial [] formValidation
    , upload = FileUpload.init
    , importResult = Unset
    }


open : Uuid -> Model
open documentTemplateUuid =
    { init | visible = True, documentTemplateUuid = Just documentTemplateUuid }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | UploadMsg FileUpload.Msg
    | ClearFile
    | ImportCompleted (Result ApiError DocumentTemplateLocale)
    | Close


update : { wrapMsg : Msg -> msg, logoutMsg : msg } -> AppState -> Msg -> Model -> ( Model, Cmd msg, Maybe DocumentTemplateLocale )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, ( model.documentTemplateUuid, FileUpload.file model.upload ) ) of
                ( Form.Submit, Just form, ( Just uuid, Just poContent ) ) ->
                    if ActionResult.isLoading model.importResult then
                        ( model, Cmd.none, Nothing )

                    else
                        ( { model | importResult = Loading }
                        , DocumentTemplatesApi.importLocale appState uuid form.name poContent (cfg.wrapMsg << ImportCompleted)
                        , Nothing
                        )

                _ ->
                    ( { model | form = Form.update formValidation formMsg model.form }
                    , Cmd.none
                    , Nothing
                    )

        UploadMsg uploadMsg ->
            let
                ( upload, uploadCmd ) =
                    FileUpload.update { mimes = [ "application/x-po", ".po" ], read = False } uploadMsg model.upload
            in
            ( { model | upload = upload, importResult = Unset }
            , Cmd.map (cfg.wrapMsg << UploadMsg) uploadCmd
            , Nothing
            )

        ClearFile ->
            ( { model | upload = FileUpload.clear, importResult = Unset }, Cmd.none, Nothing )

        ImportCompleted result ->
            case result of
                Ok locale ->
                    ( { model | visible = False, importResult = Success () }, Cmd.none, Just locale )

                Err error ->
                    ( { model | importResult = ApiError.toActionResult appState (gettext "Importing the locale failed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    , Nothing
                    )

        Close ->
            ( { model | visible = False }, Cmd.none, Nothing )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if model.visible then
        FormLayout.submitShortcut appState { result = model.importResult, onSubmit = FormMsg Form.Submit }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        importError =
            case model.importResult of
                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    Html.text ""
    in
    Modal.plain
        { title = gettext "Import locale" appState.locale
        , open = model.visible
        , onClose = Close
        , body =
            [ Layout.stack
                [ importError
                , Field.input appState model.form "name" (gettext "Name" appState.locale)
                    |> Field.withRequired True
                    |> Field.toHtml
                    |> Html.map FormMsg
                , Field.fileUploadWithTestId appState
                    model.form
                    { path = "poFile"
                    , label = gettext "PO file" appState.locale
                    , buttonLabel = gettext "Select .po file" appState.locale
                    , hint = gettext "or drop it here" appState.locale
                    , required = True
                    , upload = model.upload
                    , toMsg = UploadMsg
                    , onClear = Just ClearFile
                    , testId = Just "document-template-locale-dropzone"
                    }
                ]
            ]
        }
        |> Modal.withForm { onSubmit = FormMsg Form.Submit, submitLabel = gettext "Import" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.importResult)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "document-template-import-locale"
        |> Modal.toHtml
