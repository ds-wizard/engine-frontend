module Wizard.Templates.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Auth.Permission as Perm
import Shared.Data.Template exposing (Template)
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig, ListingDropdownItem)
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
    Page.actionResultView appState (viewKnowledgeModels appState model) model.templates


viewKnowledgeModels : AppState -> Model -> Listing.Model Template -> Html Msg
viewKnowledgeModels appState model templates =
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.successOnlyView appState model.deletingTemplate
        , Listing.view appState (listingConfig appState) templates
        , deleteModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    if Perm.hasPerm appState.session Perm.packageManagementWrite then
        [ linkTo appState
            (Routes.TemplatesRoute <| ImportRoute Nothing)
            [ class "btn btn-primary link-with-icon" ]
            [ faSet "kms.upload" appState
            , lx_ "header.import" appState
            ]
        ]

    else
        []


listingConfig : AppState -> ListingConfig Template Msg
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
        ]


listingTitleOutdatedBadge : AppState -> Template -> Html Msg
listingTitleOutdatedBadge appState template =
    if TemplateState.isOutdated template.state then
        span [ class "badge badge-warning" ] [ lx_ "badge.outdated" appState ]

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
            }
    in
    Modal.confirm appState modalConfig
