module Wizard.Common.Components.AIAssistant exposing
    ( Msg
    , State
    , UpdateConfig
    , ViewConfig
    , init
    , initialState
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, a, button, div, form, h5, input, p, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random exposing (Seed)
import Shared.Api.Request exposing (ServerInfo)
import Shared.Data.ApiError exposing (ApiError)
import Shared.Html exposing (fa, faSet)
import Shared.Markdown as Markdown
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.AIAssistant.Api as Api
import Wizard.Common.Components.AIAssistant.Models.Answer exposing (Answer)
import Wizard.Common.Components.AIAssistant.Models.Conversation exposing (Conversation)
import Wizard.Common.Components.AIAssistant.Models.Message exposing (Message)
import Wizard.Common.View.ActionResultBlock as ActionResultBlock


type State
    = State StateData


type alias StateData =
    { conversation : ActionResult Conversation
    , currentMessage : String
    , pendingMessage : Maybe String
    , answer : ActionResult Answer
    }


initialState : State
initialState =
    State
        { conversation = ActionResult.Unset
        , currentMessage = ""
        , pendingMessage = Nothing
        , answer = ActionResult.Unset
        }


type Msg
    = Init
    | GetLatestConversationCompleted (Result ApiError Conversation)
    | InputMessage String
    | SubmitMessage
    | SubmitSampleMessage String
    | SubmitMessageCompleted (Result ApiError Answer)
    | NewConversation
    | NewConversationCompleted (Result ApiError Answer)


init : Msg
init =
    Init


type alias UpdateConfig =
    { serverInfo : ServerInfo
    , seed : Seed
    }


update : UpdateConfig -> Msg -> State -> ( Seed, State, Cmd Msg )
update cfg msg (State state) =
    let
        startNewConversation =
            let
                ( uuid, newSeed ) =
                    Uuid.randomUuid cfg.seed

                conversation =
                    { uuid = uuid
                    , messages = []
                    }
            in
            ( newSeed
            , State { state | conversation = ActionResult.Success conversation }
            , Api.postQuestion cfg.serverInfo conversation.uuid { question = "" } NewConversationCompleted
            )

        submitMessage message =
            case state.conversation of
                ActionResult.Success conversation ->
                    let
                        question =
                            { question = message
                            }
                    in
                    ( cfg.seed
                    , State
                        { state
                            | currentMessage = ""
                            , pendingMessage = Just message
                            , answer = ActionResult.Loading
                        }
                    , Api.postQuestion cfg.serverInfo conversation.uuid question SubmitMessageCompleted
                    )

                _ ->
                    ( cfg.seed
                    , State state
                    , Cmd.none
                    )
    in
    case msg of
        Init ->
            ( cfg.seed
            , State { state | conversation = ActionResult.Loading }
            , Api.getLatestConversation cfg.serverInfo GetLatestConversationCompleted
            )

        GetLatestConversationCompleted result ->
            case result of
                Ok conversation ->
                    ( cfg.seed
                    , State { state | conversation = ActionResult.Success conversation }
                    , Cmd.none
                    )

                Err _ ->
                    startNewConversation

        InputMessage message ->
            ( cfg.seed
            , State { state | currentMessage = message }
            , Cmd.none
            )

        SubmitMessage ->
            if String.isEmpty state.currentMessage then
                ( cfg.seed
                , State state
                , Cmd.none
                )

            else
                submitMessage state.currentMessage

        SubmitSampleMessage message ->
            submitMessage message

        SubmitMessageCompleted result ->
            case state.conversation of
                ActionResult.Success conversation ->
                    case result of
                        Ok answer ->
                            let
                                message =
                                    { question = Maybe.withDefault "" state.pendingMessage
                                    , answer = answer.answer
                                    }
                            in
                            ( cfg.seed
                            , State
                                { state
                                    | conversation =
                                        ActionResult.Success
                                            { conversation
                                                | messages = conversation.messages ++ [ message ]
                                            }
                                    , pendingMessage = Nothing
                                    , answer = ActionResult.Unset
                                }
                            , Cmd.none
                            )

                        Err _ ->
                            ( cfg.seed
                            , State { state | answer = ActionResult.Error "Unable to get response" }
                            , Cmd.none
                            )

                _ ->
                    ( cfg.seed
                    , State state
                    , Cmd.none
                    )

        NewConversation ->
            startNewConversation

        NewConversationCompleted result ->
            case result of
                Ok _ ->
                    ( cfg.seed
                    , State state
                    , Cmd.none
                    )

                Err _ ->
                    ( cfg.seed
                    , State { state | answer = ActionResult.Error "Unable to get response" }
                    , Cmd.none
                    )


type alias ViewConfig msg =
    { appState : AppState
    , closeMsg : msg
    , wrapMsg : Msg -> msg
    }


view : ViewConfig msg -> State -> Html msg
view cfg (State state) =
    ActionResultBlock.view cfg.appState
        (viewConversation cfg state)
        state.conversation


viewConversation : ViewConfig msg -> StateData -> Conversation -> Html msg
viewConversation cfg state conversation =
    let
        isNewConversation =
            List.isEmpty conversation.messages && state.pendingMessage == Nothing

        content =
            if isNewConversation then
                viewNewConversation cfg

            else
                viewMessages cfg state conversation
    in
    div [ class "ai-assistant" ]
        [ div [ class "header fw-bold" ]
            [ div [ class "px-3 py-3 d-flex justify-content-between" ]
                [ text "AI Assistant"
                , a [ onClick cfg.closeMsg ] [ faSet "_global.close" cfg.appState ]
                ]
            ]
        , content
        , viewForm cfg state
        ]


viewNewConversation : ViewConfig msg -> Html msg
viewNewConversation cfg =
    let
        sampleMessages =
            [ "How to create a data management plan?"
            , "What is a knowledge model?"
            , "What is a document template?"
            ]

        viewSampleMessage message =
            button [ class "btn btn-outline-primary mb-2 text-start", onClick (cfg.wrapMsg (SubmitSampleMessage message)) ]
                [ text message ]
    in
    div [ class "flex-grow-1 px-3 py-3 overflow-auto" ]
        ([ h5 [] [ text "👋 Hello!" ]
         , p [] [ text "I'm the AI Assistant, here to help you understand and get the most out of FAIR Wizard." ]
         , p [] [ text "I don’t have access to your data but can assist you using information from official guides and web resources." ]
         , p [] [ text "Feel free to ask me anything about FAIR Wizard’s features, capabilities, or best practices, or choose a sample question below to get started." ]
         ]
            ++ List.map viewSampleMessage sampleMessages
        )


viewMessages : ViewConfig msg -> StateData -> Conversation -> Html msg
viewMessages cfg state conversation =
    let
        loader =
            case state.answer of
                ActionResult.Loading ->
                    [ div [ class "rounded bg-light px-3 py-1 mt-3 border w-25" ]
                        [ fa "fa-lg fa-ellipsis fa-beat-fade" ]
                    ]

                ActionResult.Error _ ->
                    [ newConversationLink cfg
                    , div [ class "text-danger py-2" ]
                        [ fa "fas fa-exclamation-triangle me-1"
                        , text "Unable to get response"
                        ]
                    ]

                _ ->
                    []

        pendingMessageOrNewConversation =
            case state.pendingMessage of
                Just message ->
                    [ viewQuestion message ]

                Nothing ->
                    [ newConversationLink cfg ]

        messages =
            List.concatMap viewMessage (List.reverse conversation.messages)
    in
    div [ class "messages px-3" ] (loader ++ pendingMessageOrNewConversation ++ messages)


newConversationLink : ViewConfig msg -> Html msg
newConversationLink cfg =
    div [ class "text-center pt-3 pb-2" ]
        [ a
            [ class "fw-bold"
            , onClick (cfg.wrapMsg NewConversation)
            ]
            [ text "New conversation" ]
        ]


viewMessage : Message -> List (Html msg)
viewMessage message =
    [ viewAnswer message.answer
    , viewQuestion message.question
    ]


viewQuestion : String -> Html msg
viewQuestion question =
    div [ class "rounded bg-primary px-2 py-1 mt-3 w-75 m-25" ]
        [ text question ]


viewAnswer : String -> Html msg
viewAnswer answer =
    Markdown.toHtml [ class "rounded bg-light px-2 py-1 mt-3 border" ]
        (Markdown.sanitizeHtml answer)


viewForm : ViewConfig msg -> StateData -> Html msg
viewForm cfg state =
    form
        [ onSubmit (cfg.wrapMsg SubmitMessage)
        , class "px-3 py-3"
        ]
        [ input
            [ value state.currentMessage
            , onInput (cfg.wrapMsg << InputMessage)
            , type_ "text"
            , class "form-control"
            , disabled (state.answer == ActionResult.Loading)
            ]
            []
        , a
            [ onClick (cfg.wrapMsg SubmitMessage)
            , disabled (state.answer == ActionResult.Loading || String.isEmpty state.currentMessage)
            , class "link-primary"
            ]
            [ fa "fas fa-paper-plane" ]
        ]
