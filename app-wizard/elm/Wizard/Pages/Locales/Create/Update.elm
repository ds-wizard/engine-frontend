module Wizard.Pages.Locales.Create.Update exposing (update)

import ActionResult
import File exposing (File)
import Form
import Gettext exposing (gettext)
import Json.Decode as D
import Json.Encode as E
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import String exposing (fromInt)
import Wizard.Api.Locales as LocalesApi
import Wizard.Components.Dropzone as Dropzone
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Locales.Common.LocaleCreateForm as LocaleCreateForm
import Wizard.Pages.Locales.Create.Models exposing (Model, combineContentFiles)
import Wizard.Pages.Locales.Create.Msgs exposing (Msg(..))
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


update : AppState -> Msg -> (Msg -> Wizard.Msgs.Msg) -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg wrapMsg model =
    case msg of
        WizardContentFileDropzoneMsg dropzoneMsg ->
            updateDropzoneState
                { dropzoneMsg = dropzoneMsg
                , dropzoneState = model.wizardContentFileDropzone
                , createModel = \ds -> { model | wizardContentFileDropzone = ds }
                , wrapDropzoneMsg = wrapMsg << WizardContentFileDropzoneMsg
                , dropzoneType = LocaleJed "wizard"
                }

        MailContentFileDropzoneMsg dropzoneMsg ->
            updateDropzoneState
                { dropzoneMsg = dropzoneMsg
                , dropzoneState = model.mailContentFileDropzone
                , createModel = \ds -> { model | mailContentFileDropzone = ds }
                , wrapDropzoneMsg = wrapMsg << MailContentFileDropzoneMsg
                , dropzoneType = LocalePO (\file -> \m -> { m | mailContent = file })
                }

        CreateCompleted result ->
            handleCreateCompleted appState model result

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl Routes.localesIndex) )

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        LocaleConverted value ->
            case D.decodeValue File.decoder value of
                Ok file ->
                    case File.name file of
                        "wizard" ->
                            ( { model | wizardContent = Just file }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


type alias UpdateDropzoneConfig msg =
    { dropzoneMsg : Dropzone.Msg
    , dropzoneState : Dropzone.State
    , createModel : Dropzone.State -> Model
    , wrapDropzoneMsg : Dropzone.Msg -> msg
    , dropzoneType : UpdateDropzoneType
    }


type UpdateDropzoneType
    = LocaleJed String
    | LocalePO (Maybe File -> Model -> Model)


updateDropzoneState : UpdateDropzoneConfig msg -> ( Model, Cmd msg )
updateDropzoneState cfg =
    let
        dropzoneUpdateConfig =
            { mimes = [ "application/x-po", ".po" ]
            , readFile = True
            }

        ( newDropzoneState, dropzoneCmd ) =
            Dropzone.update dropzoneUpdateConfig cfg.dropzoneMsg cfg.dropzoneState
                |> Tuple.mapSecond (Cmd.map cfg.wrapDropzoneMsg)

        newModel =
            cfg.createModel newDropzoneState
    in
    case cfg.dropzoneType of
        LocaleJed fileName ->
            let
                localeCmd =
                    case Dropzone.getFileContent newDropzoneState of
                        Just fileContent ->
                            Ports.convertLocaleFile <|
                                E.object
                                    [ ( "fileName", E.string fileName )
                                    , ( "fileContent", E.string fileContent )
                                    ]

                        Nothing ->
                            Cmd.none
            in
            ( newModel
            , Cmd.batch
                [ dropzoneCmd
                , localeCmd
                ]
            )

        LocalePO setFile ->
            ( setFile (Dropzone.getFile newDropzoneState) newModel
            , dropzoneCmd
            )


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, combineContentFiles model ) of
        ( Form.Submit, Just form, Just contentFiles ) ->
            let
                data =
                    [ ( "name", form.name )
                    , ( "localeId", form.localeId )
                    , ( "code", form.code )
                    , ( "license", form.license )
                    , ( "description", form.description )
                    , ( "readme", form.readme )
                    , ( "version", String.join "." <| List.map fromInt [ form.localeMajor, form.localeMinor, form.localePatch ] )
                    , ( "recommendedAppVersion", String.join "." <| List.map fromInt [ form.appMajor, form.appMinor, form.appPatch ] )
                    ]

                cmd =
                    Cmd.map wrapMsg <|
                        LocalesApi.createFromPO appState data contentFiles.wizard contentFiles.mail CreateCompleted
            in
            ( { model | creatingLocale = ActionResult.Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update (LocaleCreateForm.validation appState) formMsg model.form }
            in
            ( newModel, Cmd.none )


handleCreateCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleCreateCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.localesIndex )

        Err error ->
            ( { model | creatingLocale = ApiError.toActionResult appState (gettext "Creating of the locale failed." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
