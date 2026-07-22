module Wizard.Pages.KnowledgeModels.Detail.ImportLocaleModal exposing
    ( ImportLocaleForm
    , Model
    , Msg
    , init
    , open
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Ports.Locale as Locale
import Common.Utils.Form.FormError exposing (FormError)
import File exposing (File)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Wizard.Components.Dropzone as Dropzone
import Wizard.Data.AppState exposing (AppState)



-- MODEL


type alias Model =
    { visible : Bool
    , kmPackageUuid : Maybe Uuid
    , form : Form FormError ImportLocaleForm
    , dropzone : Dropzone.State
    , poContent : Maybe File
    , jsonContent : Maybe File
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
    , dropzone = Dropzone.initialState
    , poContent = Nothing
    , jsonContent = Nothing
    , importResult = Unset
    }


open : Uuid -> Model
open kmPackageUuid =
    { init | visible = True, kmPackageUuid = Just kmPackageUuid }



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | DropzoneMsg Dropzone.Msg
    | LocaleConverted D.Value
    | LocaleConversionFailed
    | ImportCompleted (Result ApiError KnowledgeModelLocale)
    | Close


update : AppState -> Msg -> Model -> ( Model, Cmd Msg, Maybe KnowledgeModelLocale )
update appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, ( model.kmPackageUuid, model.poContent, model.jsonContent ) ) of
                ( Form.Submit, Just form, ( Just uuid, Just poContent, Just jsonContent ) ) ->
                    ( { model | importResult = Loading }
                    , KnowledgeModelPackagesApi.importLocale appState uuid form.name poContent jsonContent ImportCompleted
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
                    Dropzone.update { mimes = [ "application/x-po", ".po" ], readFile = True } dropzoneMsg model.dropzone

                convertCmd =
                    case Dropzone.getFileContent newDropzone of
                        Just fileContent ->
                            Locale.convertLocaleFile <|
                                E.object
                                    [ ( "fileName", E.string "locale" )
                                    , ( "fileContent", E.string fileContent )
                                    ]

                        Nothing ->
                            Cmd.none
            in
            ( { model | dropzone = newDropzone, poContent = Dropzone.getFile newDropzone }
            , Cmd.batch [ Cmd.map DropzoneMsg dropzoneCmd, convertCmd ]
            , Nothing
            )

        LocaleConverted value ->
            case D.decodeValue File.decoder value of
                Ok file ->
                    ( { model | jsonContent = Just file, importResult = Unset }, Cmd.none, Nothing )

                Err _ ->
                    ( model, Cmd.none, Nothing )

        LocaleConversionFailed ->
            ( { model
                | dropzone = Dropzone.initialState
                , poContent = Nothing
                , jsonContent = Nothing
                , importResult = Error (gettext "You have uploaded an invalid PO file." appState.locale)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.visible then
        Sub.batch
            [ Locale.localeConverted LocaleConverted
            , Locale.localeConversionFailed (always LocaleConversionFailed)
            ]

    else
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
            , div [ class "form-group", dataCy "km-locale-dropzone" ]
                [ label [] [ text (gettext "PO file" appState.locale) ]
                , Dropzone.dropzone
                    { wrapMsg = DropzoneMsg
                    , buttonText = gettext "Select .po file" appState.locale
                    , dropzoneText = gettext "or drop it here" appState.locale
                    , fileIcon = Nothing
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
                |> Modal.confirmConfigActionEnabled (Maybe.isJust model.poContent && Maybe.isJust model.jsonContent)
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDataCy "km-import-locale"
    in
    Modal.confirm appState cfg
