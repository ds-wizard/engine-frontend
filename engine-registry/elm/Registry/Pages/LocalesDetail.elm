module Registry.Pages.LocalesDetail exposing
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
import Registry.Api.Locales as LocalesApi
import Registry.Api.Models.LocaleDetail as Locale exposing (LocaleDetail)
import Registry.Components.DetailPage as DetailPage
import Registry.Components.ItemIdBox as ItemIdBox
import Registry.Components.Page as Page
import Registry.Components.SidebarRow as SidebarRow
import Registry.Components.VersionList as VersionList
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Shared.Data.ApiError as ApiError exposing (ApiError)


type alias Model =
    { locale : ActionResult LocaleDetail
    , itemIdBoxState : ItemIdBox.State
    , versionListState : VersionList.State
    }


initialModel : Model
initialModel =
    { locale = ActionResult.Loading
    , itemIdBoxState = ItemIdBox.initialState
    , versionListState = VersionList.initialState
    }


setLocale : ActionResult LocaleDetail -> Model -> Model
setLocale result model =
    { model | locale = result }


init : AppState -> String -> ( Model, Cmd Msg )
init appState knowledgeModelId =
    ( initialModel
    , LocalesApi.getLocale appState knowledgeModelId GetLocaleCompleted
    )


type Msg
    = GetLocaleCompleted (Result ApiError LocaleDetail)
    | ItemIdBoxMsg ItemIdBox.Msg
    | VersionListMsg VersionList.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetLocaleCompleted result ->
            ( ActionResult.apply setLocale
                (ApiError.toActionResult appState (gettext "Unable to get locale." appState.locale))
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
    Page.view appState (viewKnowledgeModel appState model) model.locale


viewKnowledgeModel : AppState -> Model -> LocaleDetail -> Html Msg
viewKnowledgeModel appState model locale =
    DetailPage.view appState
        { icon = "fas fa-language"
        , title = locale.name
        , version = locale.version
        , published = locale.createdAt
        , readme = locale.readme
        , sidebar =
            [ SidebarRow.viewId appState
                { title = gettext "Locale ID" appState.locale
                , id = locale.id
                , wrapMsg = ItemIdBoxMsg
                , itemIdBoxState = model.itemIdBoxState
                }
            , SidebarRow.viewLicense appState locale.license
            , SidebarRow.viewVersion appState locale.version
            , SidebarRow.viewOtherVersions appState
                { versions = locale.versions
                , currentVersion = locale.version
                , toUrl = Routes.toUrl << Routes.localeDetail << Locale.otherVersionId locale
                , wrapMsg = VersionListMsg
                , versionListState = model.versionListState
                }
            , SidebarRow.viewPublishedOn appState locale.createdAt
            , SidebarRow.viewOrganization appState locale.organization
            ]
        }
