module Wizard.Dev.PersistentCommandsDetail.View exposing (view)

import Html exposing (Html, code, div, h3, pre, span, text)
import Html.Attributes exposing (class)
import Json.Print
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.PersistentCommand as PersistentCommand
import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Shared.Data.User as User
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Html exposing (emptyNode)
import SyntaxHighlight
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.PersistentCommandBadge as PersistentCommandBadge
import Wizard.Common.View.TenantIcon as TenantIcon
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Dev.Common.PersistentCommandActionsDropdown as PersistentCommandActionDropdown
import Wizard.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPersistentCommand appState model) model.persistentCommand


viewPersistentCommand : AppState -> Model -> PersistentCommandDetail -> Html Msg
viewPersistentCommand appState model persistentCommand =
    DetailPage.container
        [ header appState model persistentCommand
        , content appState model persistentCommand
        , sidePanel appState persistentCommand
        ]


header : AppState -> Model -> PersistentCommandDetail -> Html Msg
header appState model persistentCommand =
    let
        title =
            span []
                [ text (PersistentCommand.visibleName persistentCommand)
                , PersistentCommandBadge.view persistentCommand
                ]

        dropdownActions =
            PersistentCommandActionDropdown.dropdown appState
                { dropdownState = model.dropdownState
                , toggleMsg = DropdownMsg
                }
                { retryMsg = always RerunCommand
                , setIgnoredMsg = always SetIgnored
                , viewActionVisible = False
                }
                persistentCommand
    in
    DetailPage.header title [ dropdownActions ]


content : AppState -> Model -> PersistentCommandDetail -> Html msg
content appState model persistentCommand =
    let
        defaultContent =
            pre [] [ code [] [ text persistentCommand.body ] ]

        bodyContent =
            case Json.Print.prettyString { indent = 4, columns = 100 } persistentCommand.body of
                Ok jsonResult ->
                    div []
                        [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                        , SyntaxHighlight.json jsonResult
                            |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                            |> Result.withDefault defaultContent
                        ]

                Err _ ->
                    div []
                        [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                        , SyntaxHighlight.noLang persistentCommand.body
                            |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                            |> Result.withDefault defaultContent
                        ]

        body =
            [ h3 [] [ text "Body" ]
            , bodyContent
            ]

        error =
            case persistentCommand.lastErrorMessage of
                Just lastErrorMessage ->
                    [ h3 [ class "mt-5" ] [ text "Last Error Message" ]
                    , pre [ class "pre-error" ] [ text lastErrorMessage ]
                    ]

                Nothing ->
                    []
    in
    DetailPage.content
        [ FormResult.view appState model.updating
        , div [ DetailPage.contentInnerFullClass ]
            (error ++ body)
        ]


sidePanel : AppState -> PersistentCommandDetail -> Html msg
sidePanel appState persistentCommand =
    let
        sections =
            [ sidePanelPersistentCommandInfo appState persistentCommand
            , sidePanelAppInfo appState persistentCommand
            ]

        lastTraceUuidSection =
            case persistentCommand.lastTraceUuid of
                Just lastTraceUuid ->
                    [ sidePanelLastTraceUuid lastTraceUuid ]

                Nothing ->
                    []

        createdBySection =
            case persistentCommand.createdBy of
                Just createdBy ->
                    [ sidePanelCreatedByInfo createdBy ]

                Nothing ->
                    []
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 (sections ++ lastTraceUuidSection ++ createdBySection) ]


sidePanelPersistentCommandInfo : AppState -> PersistentCommandDetail -> ( String, String, Html msg )
sidePanelPersistentCommandInfo appState persistentCommand =
    let
        info =
            [ ( "Attempts", "attempts", text (String.fromInt persistentCommand.attempts ++ "/" ++ String.fromInt persistentCommand.maxAttempts) )
            , ( "Created at", "created", text (TimeUtils.toReadableDateTime appState.timeZone persistentCommand.createdAt) )
            , ( "Updated at", "created", text (TimeUtils.toReadableDateTime appState.timeZone persistentCommand.updatedAt) )
            ]
    in
    ( "Persistent Command", "persistent-command", DetailPage.sidePanelList 4 8 info )


sidePanelAppInfo : AppState -> PersistentCommandDetail -> ( String, String, Html msg )
sidePanelAppInfo appState persistentCommand =
    let
        tenantUrl =
            persistentCommand.tenant.clientUrl
                |> String.split "://"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""

        tenantView =
            DetailPage.sidePanelItemWithIconWithLink appState
                (Routes.tenantsDetail persistentCommand.tenant.uuid)
                persistentCommand.tenant.name
                (text tenantUrl)
                (TenantIcon.view persistentCommand.tenant)
    in
    ( "Tenant", "tenant", tenantView )


sidePanelLastTraceUuid : Uuid -> ( String, String, Html msg )
sidePanelLastTraceUuid uuid =
    let
        uuidView =
            code [] [ text (Uuid.toString uuid) ]
    in
    ( "Last Trace UUID", "last-trace-uuid", uuidView )


sidePanelCreatedByInfo : UserSuggestion -> ( String, String, Html msg )
sidePanelCreatedByInfo createdBy =
    let
        userView =
            DetailPage.sidePanelItemWithIcon (User.fullName createdBy)
                emptyNode
                (UserIcon.viewSmall createdBy)
    in
    ( "Created by", "created-by", userView )
