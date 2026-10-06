module Wizard.Pages.DocumentTemplates.Detail.ImportLocaleModal exposing
    ( ImportLocaleForm
    , Model
    , Msg
    , init
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Utils.Form.FormError exposing (FormError)
import File exposing (File)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Wizard.Components.Dropzone as Dropzone
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { visible : Bool
    , documentTemplateUuid : Maybe Uuid
    , form : Form FormError ImportLocaleForm
    , dropzone : Dropzone.State
    , poContent : Maybe File
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
    , dropzone = Dropzone.initialState
    , poContent = Nothing
    , importResult = Unset
    }


open : Uuid -> Model
open documentTemplateUuid =
    { init | visible = True, documentTemplateUuid = Just documentTemplateUuid }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | DropzoneMsg Dropzone.Msg
    | ImportCompleted (Result ApiError DocumentTemplateLocale)
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg, Maybe DocumentTemplateLocale )
update appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, ( model.documentTemplateUuid, model.poContent ) ) of
                ( Form.Submit, Just form, ( Just uuid, Just poContent ) ) ->
                    ( { model | importResult = Loading }
                    , DocumentTemplatesApi.importLocale appState uuid form.name poContent ImportCompleted
                    , Nothing
                    )

                _ ->
                    ( { model | form = Form.update formValidation formMsg model.form }
                    , Cmd.none
                    , Nothing
                    )

        DropzoneMsg dropzoneMsg ->
            let
                ( newDropzone, dropzoneCmd ) =
                    Dropzone.update { mimes = [ "application/x-po", ".po" ], readFile = False } dropzoneMsg model.dropzone
            in
            ( { model | dropzone = newDropzone, poContent = Dropzone.getFile newDropzone, importResult = Unset }
            , Cmd.map DropzoneMsg dropzoneCmd
            , Nothing
            )

        ImportCompleted result ->
            case result of
                Ok locale ->
                    ( { model | visible = False, importResult = Success () }, Cmd.none, Just locale )

                Err error ->
                    ( { model | importResult = ApiError.toActionResult appState (gettext "Importing the locale failed." appState.locale) error }
                    , Cmd.none
                    , Nothing
                    )

        Close ->
            ( { model | visible = False }, Cmd.none, Nothing )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
            , div [ class "form-group", dataCy "document-template-locale-dropzone" ]
                [ label [] [ text (gettext "PO file" appState.locale) ]
                , Dropzone.dropzone
                    { wrapMsg = DropzoneMsg
                    , buttonText = gettext "Select .po file" appState.locale
                    , dropzoneText = gettext "or drop it here" appState.locale
                    , fileIcon = Nothing
                    , invalid = False
                    }
                    model.dropzone
                ]
            ]

        cfg =
            Modal.confirmConfig (gettext "Import locale" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible model.visible
                |> Modal.confirmConfigActionResult (ActionResult.map (always "") model.importResult)
                |> Modal.confirmConfigAction (gettext "Import" appState.locale) (FormMsg Form.Submit)
                |> Modal.confirmConfigActionEnabled (Maybe.isJust model.poContent)
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDataCy "document-template-import-locale"
    in
    Modal.confirm appState cfg
