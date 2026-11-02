module Common.Components.AboutModal exposing
    ( Model
    , Msg
    , ViewConfig
    , initialModel
    , open
    , update
    , view
    )

{-| Which versions are running: this client, the server and its components,
the metamodels and the plugins — what a bug report needs, and the button that
puts all of it on the clipboard in one go.

The server half is fetched when the dialog opens rather than when the
application starts, because almost nobody opens it. While it loads, the body is
a polite status region, so a screen reader hears that something is coming rather
than an empty dialog.

Each component is a heading and a description list, each label beside its
value. The previous version was a table per component with the component in a
header cell spanning two columns, which is a list of facts pretending to be
tabular data; a `dl` under an `h3` is the same information in the shape a screen
reader can walk, and laid out in two columns it keeps the table's compactness.

The copy button is in the header, where the guide link goes elsewhere, so it is
the dialog's first control and where focus lands when it opens — it is the one
thing most people open this for. It is there only once the server has answered,
since until then there is nothing to copy.

-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.BuildInfo as BuildInfo exposing (BuildInfo, MetamodelVersionInfo)
import Common.Api.Request exposing (ToMsg)
import Common.Data.Context exposing (WithLocale)
import Common.Utils.TimeUtils as TimeUtils
import Common.Utils.UrlUtils as Url
import Gettext exposing (gettext)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Json.Decode as D
import Json.Decode.Extra as D
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Time
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Copy as Copy
import Ui.DescriptionList as DescriptionList
import Ui.Link as Link
import Ui.Modal as Modal
import Ui.Text as Text


type Model
    = Model
        { open : Bool
        , buildInfo : ActionResult BuildInfo
        , copy : Copy.Model
        }


initialModel : Model
initialModel =
    Model
        { open = False
        , buildInfo = ActionResult.Unset
        , copy = Copy.init
        }


type Msg
    = SetOpen Bool
    | GetBuildInfoComplete (Result ApiError BuildInfo)
    | CopyMsg Copy.Msg


open : Msg
open =
    SetOpen True


update : WithLocale a -> { getBuildInfo : ToMsg BuildInfo Msg -> Cmd Msg } -> Msg -> Model -> ( Model, Cmd Msg )
update appState cfg msg (Model model) =
    case msg of
        SetOpen isOpen ->
            if isOpen then
                ( Model { model | open = True, buildInfo = ActionResult.Loading }
                , cfg.getBuildInfo GetBuildInfoComplete
                )

            else
                ( Model { model | open = False, buildInfo = ActionResult.Unset }, Cmd.none )

        GetBuildInfoComplete result ->
            case result of
                Ok buildInfo ->
                    ( Model { model | buildInfo = ActionResult.Success buildInfo }, Cmd.none )

                Err error ->
                    ( Model { model | buildInfo = ApiError.toActionResult appState (gettext "Unable to get the build info" appState.locale) error }
                    , Cmd.none
                    )

        CopyMsg copyMsg ->
            let
                ( copy, cmd ) =
                    Copy.update copyMsg model.copy
            in
            ( Model { model | copy = copy }, Cmd.map CopyMsg cmd )


{-| `appTitle` names what this is about ("About Admin Center"); without one the
title is simply "About". `swaggerUrl` adds the API documentation link when the
server has one. `plugins` are the client-side plugins, which only the
application knows about.
-}
type alias ViewConfig msg =
    { appTitle : Maybe String
    , apiUrl : String
    , swaggerUrl : Maybe String
    , plugins : List { name : String, version : String }
    , toMsg : Msg -> msg
    }


view : { a | locale : Gettext.Locale, timeZone : Time.Zone } -> ViewConfig msg -> Model -> Html msg
view appState cfg (Model model) =
    let
        title =
            case cfg.appTitle of
                Just appTitle ->
                    String.format (gettext "About %s" appState.locale) [ appTitle ]

                Nothing ->
                    gettext "About" appState.locale

        copyButton =
            case model.buildInfo of
                ActionResult.Success serverBuildInfo ->
                    Modal.withHeaderControl
                        (Copy.labeledWithWords
                            { prompt = gettext "Click to copy" appState.locale
                            , done = gettext "Copied!" appState.locale
                            }
                            { key = "about"
                            , label = gettext "Copy" appState.locale
                            , value = buildInfoString appState cfg serverBuildInfo
                            , model = model.copy
                            , toMsg = cfg.toMsg << CopyMsg
                            }
                        )

                _ ->
                    identity

        body =
            case model.buildInfo of
                ActionResult.Success serverBuildInfo ->
                    viewContent appState cfg serverBuildInfo

                _ ->
                    AsyncBlock.view
                        { loading = ActionResult.isLoading model.buildInfo
                        , error =
                            case model.buildInfo of
                                ActionResult.Error error ->
                                    Just error

                                _ ->
                                    Nothing
                        , loadingLabel = gettext "Loading..." appState.locale
                        , content = []
                        }
    in
    Modal.plain
        { title = title
        , open = model.open
        , onClose = cfg.toMsg (SetOpen False)
        , body = [ div [ class "c-about-modal" ] [ body ] ]
        }
        |> copyButton
        |> Modal.withWide
        |> Modal.withActions [ Button.primary (gettext "OK" appState.locale) (cfg.toMsg (SetOpen False)) ]
        |> Modal.withTestId "about"
        |> Modal.toHtml


viewContent : { a | locale : Gettext.Locale, timeZone : Time.Zone } -> ViewConfig msg -> BuildInfo -> Html msg
viewContent appState cfg serverBuildInfo =
    let
        serverExtra =
            Maybe.values
                [ Just ( gettext "API URL" appState.locale, cfg.apiUrl )
                , Maybe.map (Tuple.pair (gettext "API Docs" appState.locale)) cfg.swaggerUrl
                ]
                |> List.map
                    (\( term, url ) ->
                        DescriptionList.withHtml
                            { term = term
                            , description =
                                [ Link.link { label = url, url = url }
                                    |> Link.external
                                    |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                                    |> Link.toHtml
                                ]
                            }
                    )

        componentSections =
            List.map (\component -> viewBuildInfo appState component.name component [])
                (List.sortBy .name serverBuildInfo.components)

        metamodelSection =
            case serverBuildInfo.metamodelVersions of
                Just versions ->
                    [ viewMetamodelVersions appState versions ]

                Nothing ->
                    []

        pluginsSection =
            if List.isEmpty cfg.plugins then
                []

            else
                [ viewSectionWith DescriptionList.toHtmlColumnsWideTerms
                    (gettext "Plugins" appState.locale)
                    (List.map
                        (\plugin -> DescriptionList.entry { term = plugin.name, description = plugin.version })
                        (List.sortBy (String.toLower << .name) cfg.plugins)
                    )
                ]
    in
    div [ class "c-about-modal__sections" ]
        (viewBuildInfo appState (gettext "Client" appState.locale) BuildInfo.client []
            :: viewBuildInfo appState (gettext "Server" appState.locale) serverBuildInfo serverExtra
            :: componentSections
            ++ metamodelSection
            ++ pluginsSection
        )


viewBuildInfo : { a | locale : Gettext.Locale, timeZone : Time.Zone } -> String -> { b | version : String, builtAt : String } -> List (DescriptionList.Entry msg) -> Html msg
viewBuildInfo appState name buildInfo extra =
    let
        builtAtValue =
            D.decodeString D.datetime ("\"" ++ buildInfo.builtAt ++ "\"")
                |> Result.map (TimeUtils.toReadableDateTime appState.timeZone)
                |> Result.withDefault buildInfo.builtAt
    in
    viewSection name
        (DescriptionList.withHtml { term = gettext "Version" appState.locale, description = [ Text.code buildInfo.version ] }
            :: DescriptionList.entry { term = gettext "Built at" appState.locale, description = builtAtValue }
            :: extra
        )


viewMetamodelVersions : WithLocale a -> List MetamodelVersionInfo -> Html msg
viewMetamodelVersions appState versions =
    viewSection (gettext "Metamodel Versions" appState.locale)
        (List.map (\info -> DescriptionList.entry { term = info.name, description = info.version }) versions)


viewSection : String -> List (DescriptionList.Entry msg) -> Html msg
viewSection =
    viewSectionWith DescriptionList.toHtmlColumns


{-| The plugins are names with their versions, the name the long part, so they
give the labels more room.
-}
viewSectionWith : (List (DescriptionList.Entry msg) -> Html msg) -> String -> List (DescriptionList.Entry msg) -> Html msg
viewSectionWith toList name entries =
    div [ class "c-about-modal__section" ]
        [ h3 [ class "c-about-modal__heading" ] [ text name ]
        , toList entries
        ]


buildInfoString : WithLocale a -> ViewConfig msg -> BuildInfo -> String
buildInfoString appState cfg buildInfo =
    let
        componentToString name component =
            name ++ "\nVersion: " ++ component.version ++ "\nBuilt at: " ++ component.builtAt

        metamodelVersionsString =
            case buildInfo.metamodelVersions of
                Just versions ->
                    "Metamodel Versions\n"
                        ++ String.join "\n" (List.map (\v -> "- " ++ v.name ++ ": " ++ v.version) versions)

                Nothing ->
                    ""

        pluginsString =
            if List.isEmpty cfg.plugins then
                ""

            else
                "Plugins\n"
                    ++ String.join "\n" (List.map (\p -> "- " ++ p.name ++ ": " ++ p.version) cfg.plugins)

        parts =
            ("Instance: " ++ Maybe.withDefault cfg.apiUrl (Url.getDomain cfg.apiUrl))
                :: componentToString (gettext "Client" appState.locale) BuildInfo.client
                :: componentToString (gettext "Server" appState.locale) buildInfo
                :: List.map (\c -> componentToString c.name c) (List.sortBy .name buildInfo.components)
                ++ List.filterNot String.isEmpty [ metamodelVersionsString, pluginsString ]
    in
    String.join "\n---\n" parts
