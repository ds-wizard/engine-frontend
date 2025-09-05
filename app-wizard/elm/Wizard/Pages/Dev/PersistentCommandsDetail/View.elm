module Wizard.Pages.Dev.PersistentCommandsDetail.View exposing (view)

import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Components.PersistentCommandBadge as PersistentCommandBadge
import Common.Utils.TimeUtils as TimeUtils
import Html exposing (Html, code, div, h3, pre, span, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Json.Print
import SyntaxHighlight
import Uuid exposing (Uuid)
import Wizard.Api.Models.PersistentCommand as PersistentCommand
import Wizard.Api.Models.PersistentCommandDetail exposing (PersistentCommandDetail)
import Wizard.Api.Models.User as User
import Wizard.Api.Models.UserSuggestion exposing (UserSuggestion)
import Wizard.Components.DetailPage as DetailPage
import Wizard.Components.TenantIcon as TenantIcon
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Common.PersistentCommandActionsDropdown as PersistentCommandActionDropdown
import Wizard.Pages.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPersistentCommand appState model) model.persistentCommand


viewPersistentCommand : AppState -> Model -> PersistentCommandDetail -> Html Msg
viewPersistentCommand appState model persistentCommand =
    DetailPage.container
        [ header model persistentCommand
        , content model persistentCommand
        , sidePanel appState persistentCommand
        ]


header : Model -> PersistentCommandDetail -> Html Msg
header model persistentCommand =
    let
        title =
            span []
                [ text (PersistentCommand.visibleName persistentCommand)
                , PersistentCommandBadge.badge persistentCommand
                ]

        dropdownActions =
            PersistentCommandActionDropdown.dropdown
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


content : Model -> PersistentCommandDetail -> Html msg
content model persistentCommand =
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
        [ FormResult.view model.updating
        , div [ DetailPage.contentInnerFullClass ]
            (error ++ body)
        ]


sidePanel : AppState -> PersistentCommandDetail -> Html msg
sidePanel appState persistentCommand =
    let
        sections =
            [ sidePanelPersistentCommandInfo appState persistentCommand
            , sidePanelAppInfo persistentCommand
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


sidePanelAppInfo : PersistentCommandDetail -> ( String, String, Html msg )
sidePanelAppInfo persistentCommand =
    let
        tenantUrl =
            persistentCommand.tenant.clientUrl
                |> String.split "://"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""

        tenantView =
            DetailPage.sidePanelItemWithIconWithLink
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
                Html.nothing
                (UserIcon.viewSmall createdBy)
    in
    ( "Created by", "created-by", userView )
