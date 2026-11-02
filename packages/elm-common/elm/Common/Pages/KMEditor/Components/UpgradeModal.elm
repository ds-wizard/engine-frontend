module Common.Pages.KMEditor.Components.UpgradeModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

{-| Creating a migration of a knowledge model editor to a newer parent.

Opening it loads the parent package, and the newer versions of that package are offered in a select.
The dialog is a form (`Ui.Modal.withForm`), so Cmd/Ctrl+Enter and the submit
button send the same message; an invalid submit moves focus to the
select, and a failed request is said inside the dialog.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.KnowledgeModelEditorUpgradeForm as KnowledgeModelEditorUpgradeForm exposing (KnowledgeModelEditorUpgradeForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, span, strong, text)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Modal as Modal
import Uuid exposing (Uuid)


type alias Model =
    { kmEditor : Maybe ( Uuid, String )
    , kmEditorUpgradeForm : Form FormError KnowledgeModelEditorUpgradeForm
    , creatingMigration : ActionResult String
    , kmPackage : ActionResult KnowledgeModelPackageDetail
    }


initialModel : Model
initialModel =
    { kmEditor = Nothing
    , kmEditorUpgradeForm = KnowledgeModelEditorUpgradeForm.init
    , creatingMigration = Unset
    , kmPackage = Unset
    }


type Msg
    = Open Uuid String Uuid
    | FormMsg Form.Msg
    | UpgradeComplete (Result ApiError ())
    | GetKnowledgeModelPackageComplete (Result ApiError KnowledgeModelPackageDetail)
    | Close


open : Uuid -> String -> Uuid -> Msg
open =
    Open


type alias UpdateConfig msg =
    { cmdUpgraded : Uuid -> Cmd msg
    , cmdClosed : Uuid -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open uuid name forkOfPackageUuid ->
            ( { model
                | kmEditor = Just ( uuid, name )
                , creatingMigration = Unset
                , kmPackage = Loading
              }
            , Cmd.map cfg.wrapMsg <| KnowledgeModelPackagesApi.getKnowledgeModelPackage appState forkOfPackageUuid GetKnowledgeModelPackageComplete
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.kmEditorUpgradeForm, model.kmEditor ) of
                ( Form.Submit, Just kmEditorUpgradeForm, Just ( uuid, _ ) ) ->
                    let
                        body =
                            KnowledgeModelEditorUpgradeForm.encode kmEditorUpgradeForm
                    in
                    ( { model | creatingMigration = Loading }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelEditorsApi.postMigration appState uuid body UpgradeComplete
                    )

                _ ->
                    ( { model | kmEditorUpgradeForm = Form.update KnowledgeModelEditorUpgradeForm.validation formMsg model.kmEditorUpgradeForm }
                    , Cmd.none
                    )

        UpgradeComplete result ->
            case result of
                Ok _ ->
                    let
                        kmUuid =
                            Maybe.unwrap Uuid.nil Tuple.first model.kmEditor
                    in
                    ( { model | kmEditor = Nothing }, cfg.cmdUpgraded kmUuid )

                Err error ->
                    ( { model | creatingMigration = ApiError.toActionResult appState (gettext "Migration could not be created." appState.locale) error }
                    , Cmd.none
                    )

        GetKnowledgeModelPackageComplete result ->
            case result of
                Ok kmPackage ->
                    ( { model | kmPackage = Success kmPackage }, Cmd.none )

                Err error ->
                    ( { model | kmPackage = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            -- `cmdClosed` puts focus back where the dialog was opened from.
            ( { model | kmEditor = Nothing }
            , Maybe.unwrap Cmd.none (cfg.cmdClosed << Tuple.first) model.kmEditor
            )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.kmEditor of
                Just ( _, kmEditorName ) ->
                    ( True, kmEditorName )

                Nothing ->
                    ( False, "" )

        packageContent =
            case model.kmPackage of
                Success kmPackage ->
                    let
                        options =
                            ( "", gettext "- select parent knowledge model -" appState.locale )
                                :: KnowledgeModelPackageDetail.createFormOptions kmPackage
                    in
                    [ Alert.info ""
                        |> Alert.toHtmlWithContent
                            -- One element, because an alert's content is a
                            -- column: the pieces of a sentence handed over
                            -- loose become rows of it, and the name would sit
                            -- on a line of its own.
                            [ span []
                                (String.formatHtml (gettext "Select the new parent knowledge model for %s." appState.locale)
                                    [ strong [] [ text name ] ]
                                )
                            ]
                    , Field.select appState model.kmEditorUpgradeForm "targetPackageUuid" (gettext "New parent knowledge model" appState.locale) options
                        |> Field.toHtml
                        |> Html.map FormMsg
                    ]

                _ ->
                    []

        -- The body keeps the same children for the life of the dialog; only
        -- what is inside them changes (see `Ui.Modal`).
        body =
            [ AsyncBlock.view
                { loading = ActionResult.isLoading model.kmPackage
                , error =
                    case model.kmPackage of
                        Error message ->
                            Just message

                        _ ->
                            Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = packageContent
                }
            , div []
                [ case model.creatingMigration of
                    Error message ->
                        Alert.toHtml (Alert.danger message)

                    _ ->
                        text ""
                ]
            ]
    in
    Modal.plain
        { title = gettext "Create migration" appState.locale
        , open = visible
        , onClose = Close
        , body = body
        }
        |> Modal.withForm
            { onSubmit = FormMsg Form.Submit
            , submitLabel = gettext "Create" appState.locale
            }
        |> Modal.withLoading (ActionResult.isLoading model.creatingMigration)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.kmEditorMigration)
        |> Modal.withTestId "km-editor-update"
        |> Modal.toHtml
