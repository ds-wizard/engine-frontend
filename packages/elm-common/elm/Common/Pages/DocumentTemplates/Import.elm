module Common.Pages.DocumentTemplates.Import exposing
    ( ImportModel
    , Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Importing a document template from the registry or from a file. With the
registry enabled, the registry is the first source and is prefilled from
`?documentTemplateId`; without it there is only the file.
-}

import Common.Api.DocumentTemplates as DocumentTemplatesApi
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
import String.Format as String



-- MODEL


type ImportModel
    = FileImportModel FileImport.Model
    | RegistryImportModel (RegistryImport.Model UuidResponse)


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState documentTemplateId =
    case RegistryConfig.url appState.clientDefaults.registryUrl appState.config.registry of
        Just _ ->
            { importModel = RegistryImportModel (RegistryImport.init documentTemplateId) }

        _ ->
            { importModel = FileImportModel FileImport.init }



-- UPDATE


type Msg
    = FileImportMsg FileImport.Msg
    | RegistryImportMsg (RegistryImport.Msg UuidResponse)
    | SourceSelected String
    | NoOp


registrySourceId : String
registrySourceId =
    "registry"


fileSourceId : String
fileSourceId =
    "file"


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case ( msg, model.importModel ) of
        ( FileImportMsg fileImportMsg, FileImportModel fileImportModel ) ->
            let
                ( newFileImportModel, fileImportCmd ) =
                    FileImport.update appState
                        { mimes = [ ".zip" ]
                        , upload = DocumentTemplatesApi.importTemplate appState
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
                        { importRequest = DocumentTemplatesApi.pullTemplate appState
                        , wrapMsg = cfg.wrapMsg << RegistryImportMsg
                        , logoutMsg = cfg.logoutMsg
                        , getError = gettext "Unable to import the document template." appState.locale
                        }
                        registryImportMsg
                        registryImportModel
            in
            ( { model | importModel = RegistryImportModel newRegistryImportModel }, registryImportCmd )

        ( SourceSelected sourceId, _ ) ->
            let
                importModel =
                    if sourceId == registrySourceId then
                        RegistryImportModel (RegistryImport.init Nothing)

                    else
                        FileImportModel FileImport.init
            in
            ( { model | importModel = importModel }, ImportPage.focusTab (cfg.wrapMsg NoOp) sourceId )

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
        fileImport fileImportModel =
            [ FileImport.view appState
                { label = gettext "File" appState.locale
                , hint = gettext "Or just drop them here" appState.locale
                , doneUrl = Just (Routes.toUrl appState Routes.documentTemplatesIndex)
                , doneLabel = gettext "Done" appState.locale
                , validate = Just (validateDocumentTemplateFile appState)
                , wrapMsg = FileImportMsg
                }
                fileImportModel
            ]

        ( active, activeContent ) =
            case model.importModel of
                FileImportModel fileImportModel ->
                    ( fileSourceId, fileImport fileImportModel )

                RegistryImportModel registryImportModel ->
                    ( registrySourceId, [ viewRegistryImport appState registryImportModel ] )

        tabs =
            case RegistryConfig.url appState.clientDefaults.registryUrl appState.config.registry of
                Just _ ->
                    [ { id = registrySourceId
                      , label = gettext "From registry" appState.locale
                      , content = activeContent
                      }
                    , { id = fileSourceId
                      , label = gettext "From file" appState.locale
                      , content = activeContent
                      }
                    ]

                _ ->
                    [ { id = fileSourceId
                      , label = gettext "From file" appState.locale
                      , content = activeContent
                      }
                    ]
    in
    ImportPage.view appState
        { title = gettext "Import Document Template" appState.locale
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.documentTemplatesImport ]
        , tabs = tabs
        , active = active
        , onSelect = SourceSelected
        }


viewRegistryImport : AppState -> RegistryImport.Model UuidResponse -> Html Msg
viewRegistryImport appState registryImportModel =
    let
        registryLink =
            case RegistryConfig.url appState.clientDefaults.registryUrl appState.config.registry of
                Just url ->
                    Just { label = appState.clientDefaults.registryName, url = url ++ "/document-templates" }

                _ ->
                    Nothing
    in
    RegistryImport.view appState
        { label = gettext "Document Template ID" appState.locale
        , placeholder = "my-organization.my-template:1.0.0"
        , hint = gettext "You can find document templates in %s." appState.locale
        , registryLink = registryLink
        , submitLabel = gettext "Import" appState.locale
        , done =
            \result ->
                { message = String.format (gettext "Document template %s has been imported!" appState.locale) [ Maybe.withDefault "" (RegistryImport.submittedId registryImportModel) ]
                , url = Routes.toUrl appState (Routes.documentTemplatesDetail result.uuid)
                }
        , doneLabel = gettext "View detail" appState.locale
        , wrapMsg = RegistryImportMsg
        }
        registryImportModel


validateDocumentTemplateFile : AppState -> File -> Maybe String
validateDocumentTemplateFile appState file =
    if FileUtils.matchExtension [ "zip" ] file || File.mime file == "application/json" then
        Nothing

    else
        Just (gettext "This doesn't look like a document. Are you sure you picked the correct file?" appState.locale)
