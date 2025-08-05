module Registry.Pages.DocumentTemplatesDetail exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Api.DocumentTemplates as DocumentTemplatesApi
import Registry.Api.Models.DocumentTemplateDetail as DocumentTemplate exposing (DocumentTemplateDetail)
import Registry.Components.DetailPage as DetailPage
import Registry.Components.ItemIdBox as ItemIdBox
import Registry.Components.Page as Page
import Registry.Components.SidebarRow as SidebarRow
import Registry.Components.VersionList as VersionList
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Shared.Data.ApiError as ApiError exposing (ApiError)


type alias Model =
    { documentTemplate : ActionResult DocumentTemplateDetail
    , itemIdBoxState : ItemIdBox.State
    , versionListState : VersionList.State
    }


initialModel : Model
initialModel =
    { documentTemplate = ActionResult.Loading
    , itemIdBoxState = ItemIdBox.initialState
    , versionListState = VersionList.initialState
    }


setDocumentTemplate : ActionResult DocumentTemplateDetail -> Model -> Model
setDocumentTemplate result model =
    { model | documentTemplate = result }


init : AppState -> String -> ( Model, Cmd Msg )
init appState knowledgeModelId =
    ( initialModel
    , DocumentTemplatesApi.getDocumentTemplate appState knowledgeModelId GetDocumentTemplateCompleted
    )


type Msg
    = GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
    | ItemIdBoxMsg ItemIdBox.Msg
    | VersionListMsg VersionList.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetDocumentTemplateCompleted result ->
            ( ActionResult.apply setDocumentTemplate
                (ApiError.toActionResult appState (gettext "Unable to get document template." appState.locale))
                result
                model
            , Cmd.none
            )

        ItemIdBoxMsg itemIdBoxMsg ->
            let
                ( newState, cmd ) =
                    ItemIdBox.update itemIdBoxMsg model.itemIdBoxState
            in
            ( { model | itemIdBoxState = newState }
            , Cmd.map ItemIdBoxMsg cmd
            )

        VersionListMsg versionListMsg ->
            ( { model | versionListState = VersionList.update versionListMsg model.versionListState }
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    Page.view appState (viewKnowledgeModel appState model) model.documentTemplate


viewKnowledgeModel : AppState -> Model -> DocumentTemplateDetail -> Html Msg
viewKnowledgeModel appState model documentTemplate =
    DetailPage.view appState
        { icon = "fas fa-file-code"
        , title = documentTemplate.name
        , version = documentTemplate.version
        , published = documentTemplate.createdAt
        , readme = documentTemplate.readme
        , sidebar =
            [ SidebarRow.viewId appState
                { title = gettext "Document Template ID" appState.locale
                , id = documentTemplate.id
                , wrapMsg = ItemIdBoxMsg
                , itemIdBoxState = model.itemIdBoxState
                }
            , SidebarRow.viewLicense appState documentTemplate.license
            , SidebarRow.viewVersion appState documentTemplate.version
            , SidebarRow.viewOtherVersions appState
                { versions = documentTemplate.versions
                , currentVersion = documentTemplate.version
                , toUrl = Routes.toUrl << Routes.documentTemplateDetail << DocumentTemplate.otherVersionId documentTemplate
                , wrapMsg = VersionListMsg
                , versionListState = model.versionListState
                }
            , SidebarRow.viewMetamodelVersion appState documentTemplate.metamodelVersion
            , SidebarRow.viewPublishedOn appState documentTemplate.createdAt
            , SidebarRow.viewOrganization appState documentTemplate.organization
            ]
        }
