module Common.Pages.KnowledgeModels.Components.ImportLocaleModal exposing
    ( ImportLocaleForm
    , Model
    , Msg
    , init
    , open
    , subscriptions
    , update
    , view
    )

{-| Importing a locale into a knowledge model package: a name and a `.po` file.

The PO file is read in the browser and converted to JSON through the
`Common.Ports.Locale` ports, and both files are uploaded.

Import is never disabled on validity: submitting without a file marks the
upload invalid and says so beside it, and the name's error comes from the form.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Components.Form as C2Form
import Common.Components.Form.Field as FormField
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Ports.Locale as Locale
import Common.Utils.Form.FormError exposing (FormError)
import File exposing (File)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.FileUpload as FileUpload
import Ui.Modal as Modal
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { visible : Bool
    , kmPackageUuid : Maybe Uuid
    , form : Form FormError ImportLocaleForm
    , upload : FileUpload.Model
    , jsonContent : Maybe File
    , conversionError : Maybe String
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
    , kmPackageUuid = Nothing
    , form = Form.initial [] formValidation
    , upload = FileUpload.init
    , jsonContent = Nothing
    , conversionError = Nothing
    , importResult = Unset
    }


open : Uuid -> Model
open kmPackageUuid =
    { init | visible = True, kmPackageUuid = Just kmPackageUuid }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | UploadMsg FileUpload.Msg
    | ClearFile
    | LocaleConverted D.Value
    | LocaleConversionFailed
    | ImportCompleted (Result ApiError KnowledgeModelLocale)
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg, Maybe KnowledgeModelLocale )
update appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, ( model.kmPackageUuid, FileUpload.file model.upload, model.jsonContent ) ) of
                ( Form.Submit, Just form, ( Just uuid, Just poContent, Just jsonContent ) ) ->
                    if ActionResult.isLoading model.importResult then
                        ( model, Cmd.none, Nothing )

                    else
                        ( { model | importResult = Loading }
                        , KnowledgeModelPackagesApi.importLocale appState uuid form.name poContent jsonContent ImportCompleted
                        , Nothing
                        )

                _ ->
                    ( { model | form = Form.update formValidation formMsg model.form }
                    , Cmd.none
                    , Nothing
                    )

        UploadMsg uploadMsg ->
            let
                ( newUpload, uploadCmd ) =
                    FileUpload.update { mimes = [ "application/x-po", ".po" ], read = True } uploadMsg model.upload

                convertCmd =
                    case FileUpload.fileContent newUpload of
                        Just fileContent ->
                            if FileUpload.fileContent model.upload /= Just fileContent then
                                Locale.convertLocaleFile <|
                                    E.object
                                        [ ( "fileName", E.string "locale" )
                                        , ( "fileContent", E.string fileContent )
                                        ]

                            else
                                Cmd.none

                        Nothing ->
                            Cmd.none
            in
            ( { model | upload = newUpload, conversionError = Nothing }
            , Cmd.batch [ Cmd.map UploadMsg uploadCmd, convertCmd ]
            , Nothing
            )

        ClearFile ->
            ( { model | upload = FileUpload.clear, jsonContent = Nothing }, Cmd.none, Nothing )

        LocaleConverted value ->
            case D.decodeValue File.decoder value of
                Ok file ->
                    ( { model | jsonContent = Just file, conversionError = Nothing, importResult = Unset }, Cmd.none, Nothing )

                Err _ ->
                    ( model, Cmd.none, Nothing )

        LocaleConversionFailed ->
            ( { model
                | upload = FileUpload.clear
                , jsonContent = Nothing
                , conversionError = Just (gettext "You have uploaded an invalid PO file." appState.locale)
              }
            , Cmd.none
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



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if model.visible then
        Sub.batch
            [ Locale.localeConverted LocaleConverted
            , Locale.localeConversionFailed (always LocaleConversionFailed)
            , C2Form.submitShortcut appState { result = model.importResult, onSubmit = FormMsg Form.Submit }
            ]

    else
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        fileError =
            case model.conversionError of
                Just conversionError ->
                    Just conversionError

                Nothing ->
                    if Form.isSubmitted model.form && Maybe.isNothing (FileUpload.file model.upload) then
                        Just (gettext "File is required." appState.locale)

                    else
                        Nothing

        resultError =
            case model.importResult of
                Error message ->
                    Alert.toHtml (Alert.danger message)

                _ ->
                    text ""
    in
    Modal.plain
        { title = gettext "Import locale" appState.locale
        , open = model.visible
        , onClose = Close
        , body =
            [ Html.map FormMsg <|
                FormField.toHtml <|
                    FormField.withRequired True <|
                        FormField.input appState model.form "name" (gettext "Name" appState.locale)
            , FileUpload.toHtmlWithErrorAndTestId (UiWords.fileUpload appState)
                { id = "km-import-locale-file"
                , toMsg = UploadMsg
                , label = gettext "PO file" appState.locale
                , buttonLabel = gettext "Select .po file" appState.locale
                , hint = gettext "or drop it here" appState.locale
                , error = fileError
                , onClear = Just ClearFile
                , testId = Just "km-locale-dropzone"
                }
                model.upload
            , resultError
            ]
        }
        |> Modal.withForm { onSubmit = FormMsg Form.Submit, submitLabel = gettext "Import" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.importResult)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "km-import-locale"
        |> Modal.toHtml
