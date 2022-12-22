module Wizard.Locales.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.Locale exposing (Locale)
import Shared.Data.Locale.LocaleState as LocaleState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (listInsertIf)
import String.Format as String
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
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
    , dropdownItems = listingActions appState
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
            if locale.state == LocaleState.Outdated then
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


listingActions : AppState -> Locale -> List (ListingDropdownItem Msg)
listingActions appState locale =
    let
        viewAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.view" appState
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.localesDetail locale.id)
                , dataCy = "view"
                }

        viewActionVisible =
            Feature.localeView appState

        exportAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (ExportLocale locale)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.localeExport appState locale

        setDefaultAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "locale.default" appState
                , label = gettext "Set default" appState.locale
                , msg = ListingActionMsg (SetDefault locale)
                , dataCy = "set-default"
                }

        setDefaultActionVisible =
            Feature.localeSetDefault appState locale

        changeEnabledAction =
            if locale.enabled then
                Listing.dropdownAction
                    { extraClass = Nothing
                    , icon = faSet "_global.disable" appState
                    , label = gettext "Disable" appState.locale
                    , msg = ListingActionMsg (SetEnabled False locale)
                    , dataCy = "disable"
                    }

            else
                Listing.dropdownAction
                    { extraClass = Nothing
                    , icon = faSet "_global.enable" appState
                    , label = gettext "Enable" appState.locale
                    , msg = ListingActionMsg (SetEnabled True locale)
                    , dataCy = "enable"
                    }

        changeEnabledActionVisible =
            Feature.localeChangeEnabled appState locale

        deleteAction =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg <| ShowHideDeleteLocale <| Just locale
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.localeDelete appState locale && not locale.defaultLocale
    in
    []
        |> listInsertIf viewAction viewActionVisible
        |> listInsertIf exportAction exportActionVisible
        |> listInsertIf Listing.dropdownSeparator setDefaultActionVisible
        |> listInsertIf setDefaultAction setDefaultActionVisible
        |> listInsertIf changeEnabledAction changeEnabledActionVisible
        |> listInsertIf Listing.dropdownSeparator deleteActionVisible
        |> listInsertIf deleteAction deleteActionVisible


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
            { modalTitle = gettext "Delete locale" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingLocale
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteLocale
            , cancelMsg = Just <| ShowHideDeleteLocale Nothing
            , dangerous = True
            , dataCy = "locales-delete"
            }
    in
    Modal.confirm appState modalConfig
