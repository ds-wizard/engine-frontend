module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.AssetUploadModal exposing
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
import Html exposing (Html, button, div, h5, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Json.Decode as D
import Maybe.Extra as Maybe
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal


type alias Model =
    { hover : Bool
    , file : Maybe File
    , open : Bool
    , submitting : ActionResult ()
    }


initialModel : Model
initialModel =
    { hover = False
    , file = Nothing
    , open = False
    , submitting = Unset
    }


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFile File
    | SetOpen Bool
    | Upload
    | SubmitComplete (Result ApiError DocumentTemplateAsset)


update : (Msg -> msg) -> String -> String -> Msg -> AppState -> Model -> ( Maybe DocumentTemplateAsset, Model, Cmd msg )
update wrapMsg documentTemplateId path msg appState model =
    let
        withoutAsset ( m, cmd ) =
            ( Nothing, m, cmd )
    in
    case msg of
        Pick ->
            withoutAsset ( model, Cmd.map wrapMsg <| Select.file [ "*/*" ] GotFile )

        DragEnter ->
            withoutAsset
                ( { model | hover = True }
                , Cmd.none
                )

        DragLeave ->
            withoutAsset
                ( { model | hover = False }
                , Cmd.none
                )

        GotFile file ->
            withoutAsset
                ( { model | hover = False, file = Just file }
                , Cmd.none
                )

        SetOpen open ->
            withoutAsset ( { model | hover = False, open = open, file = Nothing, submitting = Unset }, Cmd.none )

        Upload ->
            case model.file of
                Just file ->
                    let
                        fileName =
                            String.join "/" (List.filter (not << String.isEmpty) [ path, File.name file ])

                        cmd =
                            Cmd.map wrapMsg <|
                                DocumentTemplateDraftsApi.uploadAsset documentTemplateId fileName file appState SubmitComplete
                    in
                    withoutAsset ( { model | submitting = Loading }, cmd )

                _ ->
                    withoutAsset ( model, Cmd.none )

        SubmitComplete result ->
            case result of
                Ok documentTemplateAsset ->
                    ( Just documentTemplateAsset, { model | open = False }, Cmd.none )

                Err error ->
                    withoutAsset ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload asset." appState.locale) error }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        submitButtonDisabled =
            Maybe.isNothing model.file

        submitButton =
            ActionButton.buttonWithAttrs appState
                { label = gettext "Upload" appState.locale
                , result = model.submitting
                , msg = Upload
                , dangerous = False
                , attrs = [ disabled submitButtonDisabled, dataCy "modal_action-button" ]
                }

        cancelButton =
            button [ class "btn btn-secondary", onClick (SetOpen False), disabled (ActionResult.isLoading model.submitting) ]
                [ text (gettext "Cancel" appState.locale) ]

        fileContent =
            case model.file of
                Just file ->
                    fileView appState file

                Nothing ->
                    dropzone appState model

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Upload an asset" appState.locale) ] ]
            , div [ class "modal-body logo-upload" ]
                [ FormResult.errorOnlyView appState model.submitting
                , fileContent
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


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div
        [ class "dropzone"
        , classList [ ( "active", model.hover ) ]
        , alwaysPreventDefaultOn "dragenter" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragover" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragleave" (D.succeed DragLeave)
        , alwaysPreventDefaultOn "drop" dropDecoder
        ]
        [ button [ onClick Pick, class "btn btn-secondary" ] [ text (gettext "Choose file" appState.locale) ]
        , p [] [ text (gettext "Or just drop it here" appState.locale) ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files", "0" ] (D.map GotFile File.decoder)


fileView : AppState -> File -> Html Msg
fileView appState file =
    div [ class "rounded-3 file-view" ]
        [ div [ class "file" ]
            [ faSet "import.file" appState
            , div [ class "filename" ]
                [ text (File.name file) ]
            ]
        ]
