module Wizard.Dev.PersistentCommandsIndex.View exposing (view)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, src)
import Shared.Data.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Shared.Data.User as User
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.AppIcon as AppIcon
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.PersistentCommandBadge as PersistentCommandBadge
import Wizard.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "PersistentCommands__Index" ]
        [ Page.header "Persistent Commands" []
        , FormResult.errorOnlyView appState model.retryFailed
        , Listing.view appState (listingConfig appState model) model.persistentCommands
        ]


listingConfig : AppState -> Model -> ViewConfig PersistentCommand Msg
listingConfig appState model =
    let
        retryFailedButton =
            ActionButton.buttonCustom appState
                { content =
                    [ faSet "persistentCommand.retry" appState
                    , text "Retry failed"
                    ]
                , result = model.retryFailed
                , msg = RetryFailed
                , btnClass = "btn-outline-secondary with-icon"
                }
    in
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = always []
    , textTitle = PersistentCommand.visibleName
    , emptyText = "There are no persistent commands"
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Just (AppIcon.view << .app)
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


listingTitle : AppState -> PersistentCommand -> Html Msg
listingTitle appState persistentCommand =
    span []
        [ linkTo appState
            (Routes.persistentCommandsDetail persistentCommand.uuid)
            []
            [ text (PersistentCommand.visibleName persistentCommand) ]
        , PersistentCommandBadge.view persistentCommand
        ]


listingDescription : AppState -> PersistentCommand -> Html Msg
listingDescription appState persistentCommand =
    let
        attempts =
            "Attempts: " ++ String.fromInt persistentCommand.attempts ++ "/" ++ String.fromInt persistentCommand.maxAttempts

        createdByFragment =
            span [ class "fragment" ]
                [ img [ src (User.imageUrlOrGravatar persistentCommand.createdBy), class "user-icon user-icon-small" ] []
                , text <| User.fullName persistentCommand.createdBy
                ]
    in
    span []
        [ linkTo appState (Routes.appsDetail persistentCommand.app.uuid) [ class "fragment" ] [ text persistentCommand.app.name ]
        , createdByFragment
        , span [ class "fragment" ] [ text attempts ]
        ]
