module Wizard.Pages.Dev.PersistentCommandsIndex.View exposing (view)

import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (faPersistentCommandRetry)
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Components.PersistentCommandBadge as PersistentCommandBadge
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Extra as Html
import Wizard.Api.Models.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.TenantIcon as TenantIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Common.PersistentCommandActionsDropdown as PersistentCommandActionsDropdown
import Wizard.Pages.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "PersistentCommands__Index" ]
        [ Page.header "Persistent Commands" []
        , FormResult.errorOnlyView model.updating
        , Listing.view appState (listingConfig appState model) model.persistentCommands
        ]


listingConfig : AppState -> Model -> ViewConfig PersistentCommand Msg
listingConfig appState model =
    let
        retryFailedButton =
            ActionButton.buttonCustom
                { content =
                    [ faPersistentCommandRetry
                    , text "Retry failed"
                    ]
                , result = model.updating
                , msg = RetryFailed
                , btnClass = "btn-outline-secondary with-icon"
                }
    in
    { title = listingTitle
    , description = listingDescription
    , itemAdditionalData = always Nothing
    , dropdownItems =
        PersistentCommandActionsDropdown.actions
            { retryMsg = RerunCommand
            , setIgnoredMsg = SetIgnored
            , viewActionVisible = True
            }
    , textTitle = PersistentCommand.visibleName
    , emptyText = "There are no persistent commands"
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just (TenantIcon.view << .tenant)
    , searchPlaceholderText = Just "Search commands..."
    , sortOptions =
        [ ( "createdAt", "Created" )
        ]
    , filters =
        [ Listing.SimpleMultiFilter "state"
            { name = "State"
            , options =
                [ ( "NewPersistentCommandState", "New" )
                , ( "DonePersistentCommandState", "Done" )
                , ( "ErrorPersistentCommandState", "Error" )
                , ( "IgnorePersistentCommandState", "Ignore" )
                ]
            , maxVisibleValues = 2
            }
        ]
    , toRoute = Routes.persistentCommandsIndexWithFilters
    , toolbarExtra = Just retryFailedButton
    }


listingTitle : PersistentCommand -> Html Msg
listingTitle persistentCommand =
    span []
        [ linkTo (Routes.persistentCommandsDetail persistentCommand.uuid)
            []
            [ text (PersistentCommand.visibleName persistentCommand) ]
        , PersistentCommandBadge.badge persistentCommand
        ]


listingDescription : PersistentCommand -> Html Msg
listingDescription persistentCommand =
    let
        attempts =
            "Attempts: " ++ String.fromInt persistentCommand.attempts ++ "/" ++ String.fromInt persistentCommand.maxAttempts

        createdByFragment =
            case persistentCommand.createdBy of
                Just createdBy ->
                    span [ class "fragment" ]
                        [ img [ src (User.imageUrlOrGravatar createdBy), class "user-icon user-icon-small" ] []
                        , text <| User.fullName createdBy
                        ]

                Nothing ->
                    Html.nothing
    in
    span []
        [ linkTo (Routes.tenantsDetail persistentCommand.tenant.uuid) [ class "fragment" ] [ text persistentCommand.tenant.name ]
        , createdByFragment
        , span [ class "fragment" ] [ text attempts ]
        ]
