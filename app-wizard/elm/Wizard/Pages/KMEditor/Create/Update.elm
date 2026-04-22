module Wizard.Pages.KMEditor.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import String.Normalize as Normalize
import Uuid
import Version exposing (Version)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorCreateForm as KnowledgeModelEditorCreateForm exposing (KnowledgeModelEditorCreateForm)
import Wizard.Pages.KMEditor.Create.Models exposing (Model)
import Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        fetchPackageCmd =
            case model.selectedKmPackageUuid of
                Just kmPackageId ->
                    KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageId GetPackageCompleted

                _ ->
                    Cmd.none
    in
    Cmd.batch [ fetchPackageCmd, Dom.focus "#name" ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Cancel ->
            ( model, Window.historyBack (Routing.toUrl Routes.kmEditorIndex) )

        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

        FormSetVersion version ->
            handleFormSetVersion appState version model

        PostKmEditorCompleted result ->
            handlePostKmEditorCompleted appState model result

        KnowledgeModelPackageTypeHintInputMsg typeHintInputMsg ->
            handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model

        GetPackageCompleted result ->
            case result of
                Ok kmPackage ->
                    let
                        form =
                            if model.edit then
                                model.form
                                    |> setKmEditorCreateFormValue appState "name" kmPackage.name
                                    |> setKmEditorCreateFormValue appState "kmId" kmPackage.kmId

                            else
                                model.form
                    in
                    ( { model | kmPackage = Success kmPackage, form = form }, Cmd.none )

                Err error ->
                    ( { model | kmPackage = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }, Cmd.none )


handleFormMsg : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                body =
                    KnowledgeModelEditorCreateForm.encode kmCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelEditorsApi.postKnowledgeModelEditor appState body PostKmEditorCompleted
            in
            ( { model | savingKmEditor = Loading }, cmd )

        _ ->
            let
                newForm =
                    Form.update (KnowledgeModelEditorCreateForm.validation appState) formMsg model.form

                kmIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "kmId" model.form).value

                formWithKmId =
                    case ( formMsg, kmIdEmpty ) of
                        ( Form.Blur "name", True ) ->
                            let
                                suggestedKmId =
                                    (Form.getFieldAsString "name" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setKmEditorCreateFormValue appState "kmId" suggestedKmId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithKmId }
            , FormUtils.scrollToInvalidField formMsg
            )


handleFormSetVersion : AppState -> Version -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormSetVersion appState version model =
    let
        form =
            model.form
                |> setKmEditorCreateFormValue appState "versionMajor" (String.fromInt (Version.getMajor version))
                |> setKmEditorCreateFormValue appState "versionMinor" (String.fromInt (Version.getMinor version))
                |> setKmEditorCreateFormValue appState "versionPatch" (String.fromInt (Version.getPatch version))
    in
    ( { model | form = form }, Cmd.none )


handlePostKmEditorCompleted : AppState -> Model -> Result ApiError UuidResponse -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostKmEditorCompleted appState model result =
    case result of
        Ok kmEditor ->
            ( model
            , cmdNavigate appState (Routes.kmEditorEditor kmEditor.uuid Nothing)
            )

        Err error ->
            ( { model
                | form = Form.setFormErrors appState error model.form
                , savingKmEditor = ApiError.toActionResult appState (gettext "Knowledge model could not be created." appState.locale) error
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg KnowledgeModelPackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "previousPackageUuid" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << KnowledgeModelPackageTypeHintInputMsg
            , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState (Just False)
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << Uuid.toString << .uuid
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg model.kmPackageTypeHintInputModel
    in
    ( { model | kmPackageTypeHintInputModel = packageTypeHintInputModel }, cmd )


setKmEditorCreateFormValue : AppState -> String -> String -> Form FormError KnowledgeModelEditorCreateForm -> Form FormError KnowledgeModelEditorCreateForm
setKmEditorCreateFormValue appState field value =
    Form.update (KnowledgeModelEditorCreateForm.validation appState) (Form.Input field Form.Text (Field.String value))
