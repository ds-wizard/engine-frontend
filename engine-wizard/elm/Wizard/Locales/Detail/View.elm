module Wizard.Locales.Detail.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, code, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Shared.Data.Locale.LocaleState as LocaleState
import Shared.Data.LocaleDetail as LocaleDetail exposing (LocaleDetail)
import Shared.Data.OrganizationInfo exposing (OrganizationInfo)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust, listInsertIf)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Locales.Detail.Models exposing (Model)
import Wizard.Locales.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewLocale appState model) model.locale


viewLocale : AppState -> Model -> LocaleDetail -> Html Msg
viewLocale appState model locale =
    DetailPage.container
        [ header appState locale
        , readme appState locale
        , sidePanel appState locale
        , deleteVersionModal appState model locale
        ]


header : AppState -> LocaleDetail -> Html Msg
header appState locale =
    let
        exportAction =
            a [ onClick (ExportLocale locale) ]
                [ faSet "_global.export" appState
                , text (gettext "Export" appState.locale)
                ]

        exportActionVisible =
            Feature.localeExport appState

        setDefaultAction =
            a [ onClick <| SetDefault, class "with-icon" ]
                [ faSet "locale.default" appState
                , text (gettext "Set default" appState.locale)
                ]

        setDefaultActionVisible =
            locale.enabled && not locale.defaultLocale && Feature.localeEdit appState

        enableAction =
            if locale.enabled then
                a [ onClick <| SetEnabled False, class "with-icon" ]
                    [ faSet "_global.disable" appState
                    , text (gettext "Disable" appState.locale)
                    ]

            else
                a [ onClick <| SetEnabled True, class "with-icon" ]
                    [ faSet "_global.enable" appState
                    , text (gettext "Enable" appState.locale)
                    ]

        enableActionVisible =
            Feature.localeEdit appState

        deleteAction =
            a [ onClick <| ShowDeleteDialog True, class "text-danger with-icon" ]
                [ faSet "_global.delete" appState
                , text (gettext "Delete" appState.locale)
                ]

        deleteActionVisible =
            not locale.defaultLocale && Feature.localeDelete appState

        actions =
            []
                |> listInsertIf exportAction exportActionVisible
                |> listInsertIf setDefaultAction setDefaultActionVisible
                |> listInsertIf enableAction enableActionVisible
                |> listInsertIf deleteAction deleteActionVisible

        defaultBadge =
            if locale.defaultLocale then
                Badge.info [] [ text (gettext "default" appState.locale) ]

            else
                emptyNode

        headerText =
            span []
                [ text locale.name
                , defaultBadge
                ]
    in
    DetailPage.header headerText actions


readme : AppState -> LocaleDetail -> Html msg
readme appState locale =
    let
        containsNewerVersions =
            not <| LocaleDetail.isLatestVersion locale

        warning =
            if containsNewerVersions then
                div [ class "alert alert-warning" ]
                    [ faSet "_global.warning" appState
                    , text (gettext "This is not the latest available version of this locale." appState.locale)
                    ]

            else
                newVersionInRegistryWarning appState locale
    in
    DetailPage.content
        [ warning
        , Markdown.toHtml [ DetailPage.contentInnerClass ] locale.readme
        ]


newVersionInRegistryWarning : AppState -> LocaleDetail -> Html msg
newVersionInRegistryWarning appState locale =
    case ( locale.remoteLatestVersion, locale.state == LocaleState.Outdated, appState.config.registry ) of
        ( Just remoteLatestVersion, True, RegistryEnabled _ ) ->
            let
                importLink =
                    if Feature.localeImport appState && Version.greaterThan locale.version remoteLatestVersion then
                        let
                            localeId =
                                locale.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString remoteLatestVersion
                        in
                        [ linkTo appState
                            (Routes.localesImport (Just localeId))
                            [ class "btn btn-primary btn-sm with-icon ms-2" ]
                            [ faSet "kmImport.fromRegistry" appState, text (gettext "Import" appState.locale) ]
                        ]

                    else
                        []
            in
            div [ class "alert alert-warning" ]
                (faSet "_global.warning" appState
                    :: String.formatHtml
                        (gettext "There is a newer version (%s) available." appState.locale)
                        [ strong [] [ text (Version.toString remoteLatestVersion) ] ]
                    ++ importLink
                )

        _ ->
            emptyNode


sidePanel : AppState -> LocaleDetail -> Html msg
sidePanel appState locale =
    let
        sections =
            [ sidePanelLocaleInfo appState locale
            , sidePanelOtherVersions appState locale
            , sidePanelOrganizationInfo appState locale
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| listFilterJust sections ]


sidePanelLocaleInfo : AppState -> LocaleDetail -> Maybe ( String, String, Html msg )
sidePanelLocaleInfo appState locale =
    let
        enabledBadge =
            if locale.enabled then
                Badge.success [] [ text (gettext "Enabled" appState.locale) ]

            else
                Badge.danger [] [ text (gettext "Disabled" appState.locale) ]

        localeInfoList =
            [ ( gettext "ID" appState.locale, "id", text locale.id )
            , ( gettext "Language Code" appState.locale, "code", code [] [ text locale.code ] )
            , ( gettext "Version" appState.locale, "version", text <| Version.toString locale.version )
            , ( gettext "DSW Version" appState.locale, "dsw-version", text <| Version.toString locale.recommendedAppVersion )
            , ( gettext "License" appState.locale, "license", text locale.license )
            , ( gettext "Enabled" appState.locale, "license", enabledBadge )
            ]
    in
    Just ( gettext "Locale" appState.locale, "locale", DetailPage.sidePanelList 4 8 localeInfoList )


sidePanelOtherVersions : AppState -> LocaleDetail -> Maybe ( String, String, Html msg )
sidePanelOtherVersions appState locale =
    let
        versionLink version =
            li []
                [ linkTo appState
                    (Routes.localesDetail <| locale.organizationId ++ ":" ++ locale.localeId ++ ":" ++ Version.toString version)
                    []
                    [ text <| Version.toString version ]
                ]

        versionLinks =
            locale.versions
                |> List.filter ((/=) locale.version)
                |> List.sortWith Version.compare
                |> List.reverse
                |> List.map versionLink
    in
    if List.length versionLinks > 0 then
        Just ( gettext "Other versions" appState.locale, "other-versions", ul [] versionLinks )

    else
        Nothing


sidePanelOrganizationInfo : AppState -> LocaleDetail -> Maybe ( String, String, Html msg )
sidePanelOrganizationInfo appState locale =
    Maybe.map (\organization -> ( gettext "Published by" appState.locale, "published-by", viewOrganization organization )) locale.organization


viewOrganization : OrganizationInfo -> Html msg
viewOrganization organization =
    DetailPage.sidePanelItemWithIcon organization.name
        (text organization.organizationId)
        (ItemIcon.view { text = organization.name, image = organization.logo })


deleteVersionModal : AppState -> Model -> { a | id : String } -> Html Msg
deleteVersionModal appState model locale =
    let
        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text locale.id ] ]
                )
            ]

        modalConfig =
            { modalTitle = gettext "Delete version" appState.locale
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionResult = model.deletingVersion
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteVersion
            , cancelMsg = Just <| ShowDeleteDialog False
            , dangerous = True
            , dataCy = "locale-delete"
            }
    in
    Modal.confirm appState modalConfig
