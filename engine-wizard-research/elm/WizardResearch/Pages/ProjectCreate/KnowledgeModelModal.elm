module WizardResearch.Pages.ProjectCreate.KnowledgeModelModal exposing
    ( Model
    , Msg
    , init
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html, fromUnstyled, h1, text)
import Markdown
import Shared.Api.Packages as PackagesApi
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Components.Modal as Modal
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Error.ApiError as ApiError exposing (ApiError)
import WizardResearch.Common.AppState exposing (AppState)



-- MODEL


type alias Model =
    { knowledgeModel : ActionResult PackageDetail }


init : Model
init =
    { knowledgeModel = Unset }



-- UPDATE


type Msg
    = Open String
    | GetPackageComplete (Result ApiError PackageDetail)
    | Close


open : String -> Msg
open =
    Open


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        Open packageId ->
            ( { model | knowledgeModel = Loading }
            , PackagesApi.getPackage packageId appState GetPackageComplete
            )

        GetPackageComplete result ->
            case result of
                Ok package ->
                    ( { model | knowledgeModel = Success package }, Cmd.none )

                Err error ->
                    ( { model | knowledgeModel = ApiError.toActionResult "Unable to get knowledge model" error }
                    , Cmd.none
                    )

        Close ->
            ( { model | knowledgeModel = Unset }, Cmd.none )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Modal.view
        { visible = not <| ActionResult.isUnset model.knowledgeModel
        , closeMsg = Close
        }
        appState.theme
        [ ActionResultWrapper.blockLG appState.theme (viewPackage appState) model.knowledgeModel
        ]


viewPackage : AppState -> PackageDetail -> Html Msg
viewPackage appState package =
    let
        grid =
            Grid.cozy
    in
    grid.container []
        [ grid.row []
            [ grid.col 12
                []
                [ fromUnstyled <| Markdown.toHtml [] package.readme
                ]
            ]
        ]
