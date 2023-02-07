module Wizard.DocumentTemplates.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, code, div, img, p, span, strong, text)
import Html.Attributes exposing (class, src, title)
import Shared.Components.Badge as Badge
import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Shared.Data.DocumentTemplate.DocumentTemplateState as DocumentTemplateState
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
import Wizard.DocumentTemplates.Index.Models exposing (Model)
import Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "" ]
        [ Page.header (gettext "Document Templates" appState.locale) []
        , FormResult.successOnlyView appState model.deletingDocumentTemplate
        , Listing.view appState (listingConfig appState) model.documentTemplates
        , deleteModal appState model
        ]


importButton : AppState -> Html Msg
importButton appState =
    if Feature.templatesImport appState then
        linkTo appState
            (Routes.documentTemplatesImport Nothing)
            [ class "btn btn-primary with-icon" ]
            [ faSet "kms.upload" appState
            , text (gettext "Import" appState.locale)
            ]

    else
        emptyNode


listingConfig : AppState -> ViewConfig DocumentTemplate Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "Click \"Import\" button to import a new document template." appState.locale
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
    , toRoute = \_ -> Routes.DocumentTemplatesRoute << IndexRoute
    , toolbarExtra = Just (importButton appState)
    }


listingTitle : AppState -> DocumentTemplate -> Html Msg
listingTitle appState template =
    span []
        [ linkTo appState (Routes.documentTemplatesDetail template.id) [] [ text template.name ]
        , Badge.light
            (tooltip (gettext "Latest version" appState.locale))
            [ text <| Version.toString template.version ]
        , listingTitleOutdatedBadge appState template
        , listingTitleUnsupportedBadge appState template
        , listingTitleDeprecatedBadge appState template
        ]


listingTitleOutdatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleOutdatedBadge appState template =
    if template.state == DocumentTemplateState.Outdated then
        let
            documentTemplateId =
                Maybe.map ((++) (template.organizationId ++ ":" ++ template.templateId ++ ":")) template.remoteLatestVersion
        in
        linkTo appState
            (Routes.documentTemplatesImport documentTemplateId)
            [ class Badge.warningClass ]
            [ text (gettext "update available" appState.locale) ]

    else
        emptyNode


listingTitleUnsupportedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleUnsupportedBadge appState template =
    if template.state == DocumentTemplateState.UnsupportedMetamodelVersion then
        Badge.danger [] [ text (gettext "unsupported metamodel" appState.locale) ]

    else
        emptyNode


listingTitleDeprecatedBadge : AppState -> DocumentTemplate -> Html Msg
listingTitleDeprecatedBadge appState template =
    if template.phase == DocumentTemplatePhase.Deprecated then
        Badge.danger [] [ text (gettext "deprecated" appState.locale) ]

    else
        emptyNode


listingDescription : AppState -> DocumentTemplate -> Html Msg
listingDescription appState template =
    let
        organizationFragment =
            case template.organization of
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
        [ code [ class "fragment" ] [ text template.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text template.description ]
        ]


listingActions : AppState -> DocumentTemplate -> List (ListingDropdownItem Msg)
listingActions appState template =
    let
        viewAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.view" appState
                , label = gettext "View detail" appState.locale
                , msg = ListingActionLink (Routes.documentTemplatesDetail template.id)
                , dataCy = "view"
                }

        viewActionVisible =
            Feature.templatesView appState

        exportAction =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = gettext "Export" appState.locale
                , msg = ListingActionMsg (ExportDocumentTemplate template)
                , dataCy = "export"
                }

        exportActionVisible =
            Feature.templatesExport appState

        deleteAction =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg <| ShowHideDeleteDocumentTemplate <| Just template
                , dataCy = "delete"
                }

        deleteActionVisible =
            Feature.templatesDelete appState
    in
    []
        |> listInsertIf viewAction viewActionVisible
        |> listInsertIf exportAction exportActionVisible
        |> listInsertIf Listing.dropdownSeparator deleteActionVisible
        |> listInsertIf deleteAction deleteActionVisible


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, templateName ) =
            case model.documentTemplateToBeDeleted of
                Just template ->
                    ( True, template.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s and all its versions?" appState.locale)
                    [ strong [] [ text templateName ] ]
                )
            ]

        modalConfig =
            { modalTitle = gettext "Delete document template" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingDocumentTemplate
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteDocumentTemplate
            , cancelMsg = Just <| ShowHideDeleteDocumentTemplate Nothing
            , dangerous = True
            , dataCy = "templates-delete"
            }
    in
    Modal.confirm appState modalConfig
