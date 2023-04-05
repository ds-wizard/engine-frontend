module Wizard.KMEditor.Editor.Components.PublishModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , initialModel
    , openMsg
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, strong, text)
import Shared.Api.Packages as Packages
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.Package exposing (Package)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String.Format as String
import Uuid exposing (Uuid)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)



-- MODEL


type alias Model =
    { open : Bool
    , publishing : ActionResult String
    }


initialModel : Model
initialModel =
    { open = False
    , publishing = ActionResult.Unset
    }



-- MSG


type Msg
    = SetOpen Bool
    | Publish
    | PublishCompleted (Result ApiError Package)


openMsg : Msg
openMsg =
    SetOpen True



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , branchUuid : Uuid
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        SetOpen open ->
            ( { model | open = open }, Cmd.none )

        Publish ->
            ( { model | publishing = ActionResult.Loading }
            , Packages.postFromBranch cfg.branchUuid appState (cfg.wrapMsg << PublishCompleted)
            )

        PublishCompleted result ->
            case result of
                Ok package ->
                    ( model, cmdNavigate appState (Routes.knowledgeModelsDetail package.id) )

                Err error ->
                    ( { model | publishing = ApiError.toActionResult appState (gettext "Unable to publish knowledge model" appState.locale) error }
                    , Cmd.none
                    )



-- VIEW


type alias ViewConfig =
    { branch : BranchDetail }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    Modal.confirm appState
        { modalTitle = gettext "Publish" appState.locale
        , modalContent =
            String.formatHtml (gettext "Are you sure you want to publish %s, version %s?" appState.locale)
                [ strong [] [ text cfg.branch.name ]
                , strong [] [ text (Version.toString cfg.branch.version) ]
                ]
        , visible = model.open
        , actionResult = model.publishing
        , actionName = gettext "Publish" appState.locale
        , actionMsg = Publish
        , cancelMsg = Just (SetOpen False)
        , dangerous = False
        , dataCy = "km-editor_publish"
        }
