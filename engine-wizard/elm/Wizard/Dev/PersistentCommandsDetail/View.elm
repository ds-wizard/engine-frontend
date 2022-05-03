module Wizard.Dev.PersistentCommandsDetail.View exposing (view)

import Html exposing (Html, a, code, div, h3, pre, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Print
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.PersistentCommand as PersistentCommand
import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, faSet)
import SyntaxHighlight
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.AppIcon as AppIcon
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.PersistentCommandBadge as PersistentCommandBadge
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewPersistentCommand appState model) model.persistentCommand


viewPersistentCommand : AppState -> Model -> PersistentCommandDetail -> Html Msg
viewPersistentCommand appState model persistentCommand =
    DetailPage.container
        [ header appState persistentCommand
        , content appState model persistentCommand
        , sidePanel appState persistentCommand
        ]


header : AppState -> PersistentCommandDetail -> Html Msg
header appState persistentCommand =
    let
        title =
            span []
                [ text (PersistentCommand.visibleName persistentCommand)
                , PersistentCommandBadge.view persistentCommand
                ]

        rerunAction =
            a
                [ onClick RerunCommand
                , class "link-with-icon"
                , dataCy "persistent-command-detail_rerun-link"
                ]
                [ faSet "persistentCommand.retry" appState
                , text "Rerun"
                ]
    in
    DetailPage.header title [ rerunAction ]


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
        [ FormResult.view appState model.rerunning
        , div [ DetailPage.contentInnerFullClass ]
            (error ++ body)
        ]


sidePanel : AppState -> PersistentCommandDetail -> Html msg
sidePanel appState persistentCommand =
    let
        sections =
            [ sidePanelPersistentCommandInfo appState persistentCommand
            , sidePanelAppInfo appState persistentCommand
            , sidePanelCreatedByInfo persistentCommand
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 sections ]


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
        appUrl =
            persistentCommand.app.clientUrl
                |> String.split "://"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""

        appView =
            DetailPage.sidePanelItemWithIconWithLink appState
                (Routes.appsDetail persistentCommand.app.uuid)
                persistentCommand.app.name
                (text appUrl)
                (AppIcon.view persistentCommand.app)
    in
    ( "App", "app", appView )


sidePanelCreatedByInfo : PersistentCommandDetail -> ( String, String, Html msg )
sidePanelCreatedByInfo persistentCommand =
    let
        userView =
            DetailPage.sidePanelItemWithIcon (User.fullName persistentCommand.createdBy)
                emptyNode
                (UserIcon.viewSmall persistentCommand.createdBy)
    in
    ( "Created by", "created-by", userView )
