module Wizard.Templates.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Api.Templates as TemplatesApi
import Shared.Auth.Permission as Perm
import Shared.Data.Template exposing (Template)
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionConfig, ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes
import Wizard.Templates.Index.Models exposing (..)
import Wizard.Templates.Index.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Templates.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Templates.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.successOnlyView appState model.deletingTemplate
        , Listing.view appState (listingConfig appState) model.templates
        , deleteModal appState model
        ]


createButton : AppState -> Html Msg
createButton appState =
    if Perm.hasPerm appState.session Perm.packageManagementWrite then
        linkTo appState
            (Routes.TemplatesRoute <| ImportRoute Nothing)
            [ class "btn btn-primary link-with-icon" ]
            [ faSet "kms.upload" appState
            , lx_ "header.import" appState
            ]

    else
        emptyNode


listingConfig : AppState -> ViewConfig Template Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "template.name" appState )
        ]
    , filters = []
    , toRoute = \_ -> Routes.TemplatesRoute << IndexRoute
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> Template -> Html Msg
listingTitle appState template =
    span []
        [ linkTo appState (detailRoute template) [] [ text template.name ]
        , span
            [ class "badge badge-light"
            , title <| lg "package.latestVersion" appState
            ]
            [ text <| Version.toString template.version ]
        , listingTitleOutdatedBadge appState template
        , listingTitleUnsupportedBadge appState template
        ]


listingTitleOutdatedBadge : AppState -> Template -> Html Msg
listingTitleOutdatedBadge appState template =
    if template.state == TemplateState.Outdated then
        let
            templateId =
                Maybe.map ((++) (template.organizationId ++ ":" ++ template.templateId ++ ":")) template.remoteLatestVersion
        in
        linkTo appState
            (Routes.TemplatesRoute <| ImportRoute templateId)
            [ class "badge badge-warning" ]
            [ lx_ "badge.outdated" appState ]

    else
        emptyNode


listingTitleUnsupportedBadge : AppState -> Template -> Html Msg
listingTitleUnsupportedBadge appState template =
    if template.state == TemplateState.UnsupportedMetamodelVersion then
        span [ class "badge badge-danger" ] [ lx_ "badge.unsupported" appState ]

    else
        emptyNode


listingDescription : AppState -> Template -> Html Msg
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
                    span [ class "fragment", title <| lg "package.publishedBy" appState ]
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


listingActions : AppState -> Template -> List (ListingDropdownItem Msg)
listingActions appState template =
    let
        actions =
            [ Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.view" appState
                , label = l_ "action.viewDetail" appState
                , msg = ListingActionLink (detailRoute template)
                , dataCy = "view"
                }
            , Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.export" appState
                , label = l_ "action.export" appState
                , msg = ListingActionExternalLink (TemplatesApi.exportTemplateUrl template.id appState)
                , dataCy = "export"
                }
            ]
    in
    if Perm.hasPerm appState.session Perm.packageManagementWrite then
        actions
            ++ [ Listing.dropdownSeparator
               , Listing.dropdownAction
                    { extraClass = Just "text-danger"
                    , icon = faSet "_global.delete" appState
                    , label = l_ "action.delete" appState
                    , msg = ListingActionMsg <| ShowHideDeleteTemplate <| Just template
                    , dataCy = "delete"
                    }
               ]

    else
        actions


detailRoute : Template -> Routes.Route
detailRoute template =
    Routes.TemplatesRoute <| DetailRoute template.id


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, templateName ) =
            case model.templateToBeDeleted of
                Just template ->
                    ( True, template.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text templateName ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingTemplate
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteTemplate
            , cancelMsg = Just <| ShowHideDeleteTemplate Nothing
            , dangerous = True
            , dataCy = "templates-delete"
            }
    in
    Modal.confirm appState modalConfig
