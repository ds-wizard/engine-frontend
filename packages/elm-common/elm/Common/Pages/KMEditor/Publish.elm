module Common.Pages.KMEditor.Publish exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Publishing a new version of a knowledge model from its editor.

The previous version, when there is
one, fills in the description, the readme and the license and is the base of the
version suggestions. The editor's locales are offered for copying. On success
the published knowledge model opens.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.KnowledgeModelEditorPublishForm as KnowledgeModelEditorPublishForm exposing (KnowledgeModelEditorPublishForm)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.KMEditor.Components.PublishLocaleSelection as PublishLocaleSelection
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html, p)
import String.Format as String
import Ui.Button as Button
import Ui.Field
import Ui.Form
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Page.Content as Content
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { kmEditor : ActionResult KnowledgeModelEditorDetail
    , publishingKnowledgeModelEditor : ActionResult String
    , form : Form FormError KnowledgeModelEditorPublishForm
    , localeSelection : PublishLocaleSelection.Model
    }


initialModel : Model
initialModel =
    { kmEditor = Loading
    , publishingKnowledgeModelEditor = Unset
    , form = KnowledgeModelEditorPublishForm.init
    , localeSelection = PublishLocaleSelection.initialModel
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel
    , Cmd.batch
        [ KnowledgeModelEditorsApi.getKnowledgeModelEditor appState uuid GetKnowledgeModelEditorCompleted
        , Cmd.map PublishLocaleSelectionMsg (PublishLocaleSelection.fetchLocales appState uuid)
        ]
    )



-- UPDATE


type Msg
    = GetKnowledgeModelEditorCompleted (Result ApiError KnowledgeModelEditorDetail)
    | GetPreviousKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | Cancel
    | FormMsg Form.Msg
    | FormSetVersion Field.VersionSuggestion
    | PutKnowledgeModelEditorCompleted (Result ApiError KnowledgeModelPackage)
    | PublishLocaleSelectionMsg PublishLocaleSelection.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetKnowledgeModelEditorCompleted result ->
            case result of
                Ok kmEditor ->
                    ( { model | kmEditor = Success kmEditor }
                    , case kmEditor.previousPackage of
                        Just previousPackage ->
                            Cmd.map cfg.wrapMsg <|
                                KnowledgeModelPackagesApi.getKnowledgeModelPackage appState previousPackage.uuid GetPreviousKnowledgeModelPackageCompleted

                        Nothing ->
                            Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( model
                        , UpdateConfig.notFound cfg (NotFound.knowledgeModelEditor appState)
                        )

                    else
                        ( { model | kmEditor = ApiError.toActionResult appState (gettext "Unable to get the knowledge model details." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        GetPreviousKnowledgeModelPackageCompleted result ->
            case result of
                Ok kmPackage ->
                    let
                        form =
                            model.form
                                |> setFormValue "description" kmPackage.description
                                |> setFormValue "readme" kmPackage.readme
                                |> setFormValue "license" kmPackage.license
                    in
                    ( { model | form = form }, Cmd.none )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.knowledgeModelsIndex) )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, model.kmEditor ) of
                ( Form.Submit, Just form, Success kmEditor ) ->
                    let
                        body =
                            KnowledgeModelEditorPublishForm.encode kmEditor.uuid (PublishLocaleSelection.selectedLocaleUuids model.localeSelection) form
                    in
                    ( { model | publishingKnowledgeModelEditor = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        KnowledgeModelPackagesApi.postFromMigration appState body PutKnowledgeModelEditorCompleted
                    )

                _ ->
                    ( { model | form = Form.update KnowledgeModelEditorPublishForm.validation formMsg model.form }
                    , Cmd.none
                    )

        FormSetVersion suggestion ->
            ( { model | form = Field.setVersion KnowledgeModelEditorPublishForm.validation versionFields suggestion model.form }
            , Cmd.none
            )

        PutKnowledgeModelEditorCompleted result ->
            case result of
                Ok kmPackage ->
                    ( model
                    , Navigation.pushUrl appState.key (Routes.toUrl appState (Routes.knowledgeModelsDetail kmPackage.uuid))
                    )

                Err error ->
                    ( { model | publishingKnowledgeModelEditor = ApiError.toActionResult appState (gettext "Publishing the new version failed." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        PublishLocaleSelectionMsg subMsg ->
            ( { model | localeSelection = PublishLocaleSelection.update appState subMsg model.localeSelection }
            , Cmd.none
            )


versionFields : { majorField : String, minorField : String, patchField : String }
versionFields =
    { majorField = "major"
    , minorField = "minor"
    , patchField = "patch"
    }


setFormValue : String -> String -> Form FormError KnowledgeModelEditorPublishForm -> Form FormError KnowledgeModelEditorPublishForm
setFormValue field value =
    Form.update KnowledgeModelEditorPublishForm.validation (Form.Input field Form.Text (Form.Field.String value))



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if ActionResult.isSuccess model.kmEditor then
        CommonForm.submitShortcut appState
            { result = model.publishingKnowledgeModelEditor
            , onSubmit = FormMsg Form.Submit
            }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (contentView appState model) model.kmEditor


contentView : AppState -> Model -> KnowledgeModelEditorDetail -> Html Msg
contentView appState model kmEditor =
    let
        actions =
            Ui.Form.footer
                { cancel = Button.ghost (gettext "Cancel" appState.locale) Cancel
                , submit =
                    Button.submit (gettext "Publish" appState.locale)
                        |> Button.withLoading (ActionResult.isLoading model.publishingKnowledgeModelEditor)
                        |> Button.withTestId "km-publish_publish-button"
                }
    in
    Content.content
        { title = gettext "Publish new version" appState.locale
        , body =
            [ CommonForm.view
                { onSubmit = FormMsg Form.Submit
                , content =
                    CommonForm.errorSummary model.publishingKnowledgeModelEditor
                        :: formView appState model.form kmEditor
                        ++ [ Html.map PublishLocaleSelectionMsg (PublishLocaleSelection.view appState model.localeSelection)
                           , actions
                           ]
                }
            ]
        }
        |> Content.narrow
        |> Content.toHtml


formView : AppState -> Form FormError KnowledgeModelEditorPublishForm -> KnowledgeModelEditorDetail -> List (Html Msg)
formView appState form kmEditor =
    let
        mbVersion =
            Maybe.map .version kmEditor.previousPackage

        lastVersion =
            mbVersion
                |> Maybe.map Version.toString
                |> Maybe.withDefault (gettext "No version of this knowledge model has been published yet." appState.locale)

        licenseLink =
            Link.link
                { label = gettext "license" appState.locale
                , url = "https://spdx.org/licenses/"
                }
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml
    in
    [ Ui.Field.readOnly
        { id = "km-publish-name"
        , label = gettext "Knowledge Model" appState.locale
        , value = kmEditor.name
        }
        |> Ui.Field.toHtml
    , Ui.Field.readOnlyHtml
        { id = "km-publish-km-id"
        , label = gettext "Knowledge Model ID" appState.locale
        , value = [ Text.code kmEditor.id ]
        }
        |> Ui.Field.toHtml
    , Ui.Field.readOnly
        { id = "km-publish-last-version"
        , label = gettext "Last version" appState.locale
        , value = lastVersion
        }
        |> Ui.Field.toHtml
    , Field.versionWithSuggestions appState
        form
        { majorField = versionFields.majorField
        , minorField = versionFields.minorField
        , patchField = versionFields.patchField
        , label = gettext "New version" appState.locale
        , hint = Just (Field.versionHint appState)
        , suggestions = Field.versionSuggestions mbVersion
        , onSuggestion = FormSetVersion
        , toMsg = FormMsg
        }
    , Layout.stack
        [ Field.input appState form "license" (gettext "License" appState.locale)
            |> Field.toHtml
            |> Html.map FormMsg
        , p [] (String.formatHtml (gettext "Choose a %s so others can use your knowledge model." appState.locale) [ licenseLink ])
        ]
    , Field.input appState form "description" (gettext "Description" appState.locale)
        |> Field.withHint (gettext "Short description of the knowledge model." appState.locale)
        |> Field.toHtml
        |> Html.map FormMsg
    , Field.markdown appState form "readme" (gettext "Readme" appState.locale)
        |> Field.toHtml
        |> Html.map FormMsg
    ]
