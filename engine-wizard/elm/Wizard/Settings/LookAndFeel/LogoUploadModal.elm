module Wizard.Settings.LookAndFeel.LogoUploadModal exposing
    ( Model
    , Msg(..)
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import File exposing (File)
import File.Select as Select
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, hr, p, span, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Maybe.Extra as Maybe
import Shared.Api.Configs as ConfigsApi
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode)
import Task
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal


type alias Model =
    { hover : Bool
    , preview : Maybe String
    , file : Maybe File
    , open : Bool
    , submitting : ActionResult ()
    , defaultLogo : Bool
    }


initialModel : Model
initialModel =
    { hover = False
    , preview = Nothing
    , file = Nothing
    , open = False
    , submitting = Unset
    , defaultLogo = False
    }


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFile File
    | GotPreview String
    | SetOpen Bool
    | Upload
    | Delete
    | UseDefault
    | SubmitComplete (Result ApiError ())


update : (Msg -> msg) -> Cmd msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update wrapMsg reloadCmd msg appState model =
    case msg of
        Pick ->
            ( model, Cmd.map wrapMsg <| Select.file [ "image/*" ] GotFile )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFile file ->
            ( { model | hover = False, file = Just file }
            , Cmd.map wrapMsg <| Task.perform GotPreview (File.toUrl file)
            )

        GotPreview url ->
            ( { model | preview = Just url }
            , Cmd.none
            )

        SetOpen open ->
            ( { model | preview = Nothing, hover = False, open = open }, Cmd.none )

        Upload ->
            case model.file of
                Just file ->
                    let
                        cmd =
                            Cmd.map wrapMsg <|
                                ConfigsApi.uploadLogo file appState SubmitComplete
                    in
                    ( { model | submitting = Loading }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        UseDefault ->
            ( { model | defaultLogo = True }, Cmd.none )

        Delete ->
            let
                cmd =
                    Cmd.map wrapMsg <|
                        ConfigsApi.deleteLogo appState SubmitComplete
            in
            ( { model | submitting = Loading }, cmd )

        SubmitComplete result ->
            case result of
                Ok _ ->
                    ( model, reloadCmd )

                Err error ->
                    ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload logo." appState.locale) error }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        submitButtonMsg =
            if model.defaultLogo then
                Delete

            else
                Upload

        submitButtonDisabled =
            not model.defaultLogo && Maybe.isNothing model.preview

        submitButton =
            ActionButton.buttonWithAttrs appState
                { label = gettext "Save" appState.locale
                , result = model.submitting
                , msg = submitButtonMsg
                , dangerous = False
                , attrs = [ disabled submitButtonDisabled ]
                }

        cancelButton =
            button [ class "btn btn-secondary", onClick (SetOpen False), disabled (ActionResult.isLoading model.submitting) ]
                [ text (gettext "Cancel" appState.locale) ]

        viewPreview url =
            div
                [ class "LogoPreview mt-4" ]
                [ span [ class "LogoPreview__Logo", style "background-image" ("url('" ++ url ++ "')") ] []
                , text (LookAndFeelConfig.getAppTitleShort appState.config.lookAndFeel)
                ]

        preview =
            if model.defaultLogo then
                viewPreview "/wizard/img/logo.svg"

            else
                Maybe.unwrap emptyNode viewPreview model.preview

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Change logo" appState.locale) ] ]
            , div [ class "modal-body logo-upload" ]
                [ FormResult.errorOnlyView appState model.submitting
                , div
                    [ class "dropzone"
                    , classList [ ( "active", model.hover ) ]
                    , alwaysPreventDefaultOn "dragenter" (D.succeed DragEnter)
                    , alwaysPreventDefaultOn "dragover" (D.succeed DragEnter)
                    , alwaysPreventDefaultOn "dragleave" (D.succeed DragLeave)
                    , alwaysPreventDefaultOn "drop" dropDecoder
                    ]
                    [ button [ onClick Pick, class "btn btn-secondary" ] [ text (gettext "Choose logo" appState.locale) ]
                    , p [] [ text (gettext "Or just drop it here" appState.locale) ]
                    ]
                , hr [] []
                , p [ class "text-center" ]
                    [ a [ onClick UseDefault ] [ text (gettext "Use default logo" appState.locale) ]
                    ]
                , preview
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        modalConfig =
            { modalContent = content
            , visible = model.open
            , dataCy = "logo-upload"
            }
    in
    Modal.simple modalConfig


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files", "0" ] (D.map GotFile File.decoder)
