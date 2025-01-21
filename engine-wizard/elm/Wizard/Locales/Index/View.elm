module Wizard.Locales.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.Locale as Locale exposing (Locale)
import Shared.Html exposing (emptyNode, faSet)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Locales.Common.LocaleActionsDropdown as LocaleActionDropdown
import Wizard.Locales.Index.Models exposing (Model)
import Wizard.Locales.Index.Msgs exposing (Msg(..))
import Wizard.Locales.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Locales" appState.locale) []
        , FormResult.errorOnlyView appState model.changingLocale
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
                emptyNode

        disabledBadge =
            if locale.enabled then
                emptyNode

            else
                Badge.danger []
                    [ text (gettext "disabled" appState.locale) ]

        outdatedBadge =
            if Locale.isOutdated locale then
                let
                    localeId =
                        Maybe.map ((++) (locale.organizationId ++ ":" ++ locale.localeId ++ ":") << Version.toString) locale.remoteLatestVersion
                in
                linkTo appState
                    (Routes.localesImport localeId)
                    [ class Badge.warningClass ]
                    [ text (gettext "update available" appState.locale) ]

            else
                emptyNode
    in
    span []
        [ linkTo appState (Routes.localesDetail locale.id) [] [ text locale.name ]
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
                                    emptyNode
                    in
                    span [ class "fragment", title <| gettext "Published by" appState.locale ]
                        [ logo
                        , text organization.name
                        ]

                Nothing ->
                    emptyNode
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
            [ linkTo appState
                (Routes.localesImport Nothing)
                [ class "btn btn-primary with-icon" ]
                [ faSet "locale.import" appState
                , text (gettext "Import" appState.locale)
                ]
            , linkTo appState
                Routes.localesCreate
                [ class "btn btn-primary with-icon ms-2" ]
                [ faSet "locale.create" appState
                , text (gettext "Create" appState.locale)
                ]
            ]

    else
        emptyNode


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
