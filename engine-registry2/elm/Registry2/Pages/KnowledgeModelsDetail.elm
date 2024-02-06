module Registry2.Pages.KnowledgeModelsDetail exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, i, text)
import Html.Attributes exposing (class, href)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Registry2.Api.KnowledgeModels as KnowledgeModelsApi
import Registry2.Api.Models.KnowledgeModelDetail as KnowledgeModel exposing (KnowledgeModelDetail)
import Registry2.Components.DetailPage as DetailPage
import Registry2.Components.ItemIdBox as ItemIdBox
import Registry2.Components.Page as Page
import Registry2.Components.SidebarRow as SidebarRow
import Registry2.Components.VersionList as VersionList
import Registry2.Data.AppState exposing (AppState)
import Registry2.Routes as Routes
import Shared.Error.ApiError as ApiError exposing (ApiError)


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModelDetail
    , itemIdBoxState : ItemIdBox.State
    , versionListState : VersionList.State
    }


initialModel : Model
initialModel =
    { knowledgeModel = ActionResult.Loading
    , itemIdBoxState = ItemIdBox.initialState
    , versionListState = VersionList.initialState
    }


setKnowledgeModel : ActionResult KnowledgeModelDetail -> Model -> Model
setKnowledgeModel result model =
    { model | knowledgeModel = result }


init : AppState -> String -> ( Model, Cmd Msg )
init appState knowledgeModelId =
    ( initialModel
    , KnowledgeModelsApi.getKnowledgeModel appState knowledgeModelId GetKnowledgeModelCompleted
    )


type Msg
    = GetKnowledgeModelCompleted (Result ApiError KnowledgeModelDetail)
    | ItemIdMsg ItemIdBox.Msg
    | VersionListMsg VersionList.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetKnowledgeModelCompleted result ->
            ( ActionResult.apply setKnowledgeModel
                (ApiError.toActionResult appState (gettext "Unable to get knowledge model." appState.locale))
                result
                model
            , Cmd.none
            )

        ItemIdMsg itemIdBoxMsg ->
            let
                ( newState, cmd ) =
                    ItemIdBox.update itemIdBoxMsg model.itemIdBoxState
            in
            ( { model | itemIdBoxState = newState }
            , Cmd.map ItemIdMsg cmd
            )

        VersionListMsg versionListMsg ->
            ( { model | versionListState = VersionList.update versionListMsg model.versionListState }
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewKnowledgeModel appState model) model.knowledgeModel


viewKnowledgeModel : AppState -> Model -> KnowledgeModelDetail -> Html Msg
viewKnowledgeModel appState model knowledgeModel =
    let
        viewForkOfRow packageId =
            SidebarRow.view
                { title = gettext "Fork of" appState.locale
                , content =
                    [ a [ href (Routes.toUrl (Routes.knowledgeModelDetail packageId)) ]
                        [ text packageId ]
                    ]
                }
    in
    DetailPage.view appState
        { icon = "fas fa-sitemap"
        , title = knowledgeModel.name
        , version = knowledgeModel.version
        , published = knowledgeModel.createdAt
        , readme = knowledgeModel.readme
        , sidebar =
            [ SidebarRow.viewId appState
                { title = gettext "Knowledge Model ID" appState.locale
                , id = knowledgeModel.id
                , wrapMsg = ItemIdMsg
                , itemIdBoxState = model.itemIdBoxState
                }
            , SidebarRow.viewLicense appState knowledgeModel.license
            , SidebarRow.viewVersion appState knowledgeModel.version
            , SidebarRow.viewOtherVersions appState
                { versions = knowledgeModel.versions
                , currentVersion = knowledgeModel.version
                , toUrl = Routes.toUrl << Routes.knowledgeModelDetail << KnowledgeModel.otherVersionId knowledgeModel
                , wrapMsg = VersionListMsg
                , versionListState = model.versionListState
                }
            , SidebarRow.viewMetamodelVersion appState knowledgeModel.metamodelVersion
            , Maybe.unwrap Html.nothing viewForkOfRow knowledgeModel.forkOfPackageId
            , SidebarRow.viewPublishedOn appState knowledgeModel.createdAt
            , SidebarRow.viewOrganization appState knowledgeModel.organization
            ]
        }
