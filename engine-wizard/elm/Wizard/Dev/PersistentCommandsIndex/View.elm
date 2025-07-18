module Wizard.Dev.PersistentCommandsIndex.View exposing (view)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (faPersistentCommandRetry)
import Wizard.Api.Models.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Wizard.Api.Models.User as User
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.PersistentCommandBadge as PersistentCommandBadge
import Wizard.Common.View.TenantIcon as TenantIcon
import Wizard.Dev.Common.PersistentCommandActionsDropdown as PersistentCommandActionsDropdown
import Wizard.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


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
        , PersistentCommandBadge.view persistentCommand
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
