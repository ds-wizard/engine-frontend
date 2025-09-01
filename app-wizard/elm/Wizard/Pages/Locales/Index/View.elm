module Wizard.Pages.Locales.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Html.Extra as Html
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (faLocaleCreate, faLocaleImport)
import Shared.Components.FormResult as FormResult
import Shared.Components.Modal as Modal
import Shared.Components.Page as Page
import Shared.Components.Tooltip exposing (tooltip)
import String.Format as String
import Version
import Wizard.Api.Models.Locale as Locale exposing (Locale)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Locales.Common.LocaleActionsDropdown as LocaleActionDropdown
import Wizard.Pages.Locales.Index.Models exposing (Model)
import Wizard.Pages.Locales.Index.Msgs exposing (Msg(..))
import Wizard.Pages.Locales.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Locales" appState.locale) []
        , FormResult.errorOnlyView model.changingLocale
        , Listing.view appState (listingConfig appState) model.locales
        , deleteModal appState model
        ]


listingConfig : AppState -> ViewConfig Locale Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems =
        LocaleActionDropdown.actions appState
            { exportMsg = ExportLocale
            , setDefaultMsg = SetDefault
            , setEnabledMsg = SetEnabled
            , deleteMsg = ShowHideDeleteLocale << Just
            , viewActionVisible = True
            }
    , textTitle = .name
    , emptyText = gettext "Click \"Import\" or \"Create\" button to add a new locale." appState.locale
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        ]
    , filters = []
    , toRoute = \_ -> Routes.LocalesRoute << IndexRoute
    , toolbarExtra = Just (buttons appState)
    }


listingTitle : AppState -> Locale -> Html Msg
listingTitle appState locale =
    let
        defaultBadge =
            if locale.defaultLocale then
                Badge.info (tooltip (gettext "Default locale to be used" appState.locale))
                    [ text (gettext "default" appState.locale) ]

            else
                Html.nothing

        disabledBadge =
            if locale.enabled then
                Html.nothing

            else
                Badge.danger []
                    [ text (gettext "disabled" appState.locale) ]

        outdatedBadge =
            if Locale.isOutdated locale then
                let
                    localeId =
                        Maybe.map ((++) (locale.organizationId ++ ":" ++ locale.localeId ++ ":") << Version.toString) locale.remoteLatestVersion
                in
                linkTo (Routes.localesImport localeId)
                    [ class Badge.warningClass ]
                    [ text (gettext "update available" appState.locale) ]

            else
                Html.nothing
    in
    span []
        [ linkTo (Routes.localesDetail locale.id) [] [ text locale.name ]
        , Badge.light (tooltip (gettext "Latest version" appState.locale))
            [ text <| Version.toString locale.version ]
        , Badge.dark []
            [ text locale.code ]
        , defaultBadge
        , disabledBadge
        , outdatedBadge
        ]


listingDescription : AppState -> Locale -> Html Msg
listingDescription appState locale =
    let
        organizationFragment =
            case locale.organization of
                Just organization ->
                    let
                        logo =
                            case organization.logo of
                                Just organizationLogo ->
                                    img [ class "organization-image", src organizationLogo ] []

                                Nothing ->
                                    Html.nothing
                    in
                    span [ class "fragment", title <| gettext "Published by" appState.locale ]
                        [ logo
                        , text organization.name
                        ]

                Nothing ->
                    Html.nothing
    in
    span []
        [ code [ class "fragment" ] [ text locale.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text locale.description ]
        ]


buttons : AppState -> Html Msg
buttons appState =
    if Feature.localeCreate appState then
        div []
            [ linkTo (Routes.localesImport Nothing)
                [ class "btn btn-primary with-icon" ]
                [ faLocaleImport
                , text (gettext "Import" appState.locale)
                ]
            , linkTo Routes.localesCreate
                [ class "btn btn-primary with-icon ms-2" ]
                [ faLocaleCreate
                , text (gettext "Create" appState.locale)
                ]
            ]

    else
        Html.nothing


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, localeName ) =
            case model.localeToBeDeleted of
                Just locale ->
                    ( True, locale.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s and all its versions?" appState.locale)
                    [ strong [] [ text localeName ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete locale" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingLocale
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteLocale
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteLocale Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "locales-delete"
    in
    Modal.confirm appState modalConfig
