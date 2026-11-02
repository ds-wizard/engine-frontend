module Common.Pages.KnowledgeModels.Import exposing
    ( ImportModel
    , Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Importing a knowledge model package from the registry or from a file.

The sources are gated: the registry tab only when the registry
is enabled, and then it is the one open first. Switching source starts that
source afresh, as it did.

-}

import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.FileImport as FileImport
import Common.Components.GuideLink as GuideLink
import Common.Components.ImportPage as ImportPage
import Common.Components.RegistryImport as RegistryImport
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.FileUtils as FileUtils
import File exposing (File)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String



-- MODEL


type ImportModel
    = FileImportModel FileImport.Model
    | RegistryImportModel (RegistryImport.Model UuidResponse)


type alias Model =
    { importModel : ImportModel }


registryEnabled : AppState -> Bool
registryEnabled appState =
    case RegistryConfig.url appState.clientDefaults.registryUrl appState.config.registry of
        Just _ ->
            True

        _ ->
            False


initialModel : AppState -> Maybe String -> Model
initialModel appState kmPackageId =
    if registryEnabled appState then
        { importModel = RegistryImportModel (RegistryImport.init kmPackageId) }

    else
        { importModel = FileImportModel FileImport.init }


init : AppState -> Maybe String -> ( Model, Cmd Msg )
init appState kmPackageId =
    ( initialModel appState kmPackageId, Cmd.none )



-- UPDATE


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg (RegistryImport.Msg UuidResponse)
    | ShowSource String
    | Noop


fileSource : String
fileSource =
    "file"


registrySource : String
registrySource =
    "registry"


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case ( msg, model.importModel ) of
        ( FileImportMsg fileImportMsg, FileImportModel fileImportModel ) ->
            let
                ( newFileImportModel, fileImportCmd ) =
                    FileImport.update appState
                        { mimes = [ "*/*" ]
                        , upload = KnowledgeModelPackagesApi.importKnowledgeModelPackage appState
                        , wrapMsg = cfg.wrapMsg << FileImportMsg
                        , logoutMsg = cfg.logoutMsg
                        , getError = gettext "Unable to upload file." appState.locale
                        }
                        fileImportMsg
                        fileImportModel
            in
            ( { model | importModel = FileImportModel newFileImportModel }, fileImportCmd )

        ( RegistryImportMsg registryImportMsg, RegistryImportModel registryImportModel ) ->
            let
                ( newRegistryImportModel, registryImportCmd ) =
                    RegistryImport.update appState
                        { importRequest = KnowledgeModelPackagesApi.pullKnowledgeModelPackage appState
                        , wrapMsg = cfg.wrapMsg << RegistryImportMsg
                        , logoutMsg = cfg.logoutMsg
                        , getError = gettext "Unable to import knowledge model." appState.locale
                        }
                        registryImportMsg
                        registryImportModel
            in
            ( { model | importModel = RegistryImportModel newRegistryImportModel }, registryImportCmd )

        ( ShowSource source, _ ) ->
            let
                importModel =
                    if source == registrySource then
                        RegistryImportModel (RegistryImport.init Nothing)

                    else
                        FileImportModel FileImport.init
            in
            ( { model | importModel = importModel }, ImportPage.focusTab (cfg.wrapMsg Noop) source )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    case model.importModel of
        FileImportModel fileImportModel ->
            Sub.map FileImportMsg (FileImport.subscriptions appState fileImportModel)

        RegistryImportModel registryImportModel ->
            Sub.map RegistryImportMsg (RegistryImport.subscriptions appState registryImportModel)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        activeSource =
            case model.importModel of
                FileImportModel _ ->
                    fileSource

                RegistryImportModel _ ->
                    registrySource

        content =
            case model.importModel of
                FileImportModel fileImportModel ->
                    [ FileImport.view appState
                        { label = gettext "File" appState.locale
                        , hint = gettext "Or just drop them here" appState.locale
                        , doneUrl = Just (Routes.toUrl appState Routes.knowledgeModelsIndex)
                        , doneLabel = gettext "Done" appState.locale
                        , validate = Just (validateKmFile appState)
                        , wrapMsg = FileImportMsg
                        }
                        fileImportModel
                    ]

                RegistryImportModel registryImportModel ->
                    [ viewRegistryImport appState registryImportModel ]

        tab source label =
            { id = source, label = label, content = content }

        tabs =
            Maybe.values
                [ if registryEnabled appState then
                    Just (tab registrySource (gettext "From registry" appState.locale))

                  else
                    Nothing
                , Just (tab fileSource (gettext "From file" appState.locale))
                ]
    in
    ImportPage.view appState
        { title = gettext "Import Knowledge Model" appState.locale
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.kmImport ]
        , tabs = tabs
        , active = activeSource
        , onSelect = ShowSource
        }


validateKmFile : AppState -> File -> Maybe String
validateKmFile appState file =
    if FileUtils.matchExtension [ "km", "json" ] file then
        Nothing

    else
        Just (gettext "This doesn't look like a knowledge model. Are you sure you picked the correct file?" appState.locale)


viewRegistryImport : AppState -> RegistryImport.Model UuidResponse -> Html Msg
viewRegistryImport appState registryImportModel =
    let
        registryLink =
            case RegistryConfig.url appState.clientDefaults.registryUrl appState.config.registry of
                Just url ->
                    Just { label = appState.clientDefaults.registryName, url = url ++ "/knowledge-models" }

                _ ->
                    Nothing
    in
    RegistryImport.view appState
        { label = gettext "Knowledge Model ID" appState.locale
        , placeholder = "my-organization.my-knowledge-model:1.0.0"
        , hint = gettext "You can find knowledge models in %s." appState.locale
        , registryLink = registryLink
        , submitLabel = gettext "Import" appState.locale
        , done =
            \response ->
                { message = String.format (gettext "Knowledge model %s has been imported!" appState.locale) [ Maybe.withDefault "" (RegistryImport.submittedId registryImportModel) ]
                , url = Routes.toUrl appState (Routes.knowledgeModelsDetail response.uuid)
                }
        , doneLabel = gettext "View detail" appState.locale
        , wrapMsg = RegistryImportMsg
        }
        registryImportModel
