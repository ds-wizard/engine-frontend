module Wizard.Common.FormEngine.View exposing (FormRenderer, FormViewConfig, viewForm)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown)
import String exposing (fromInt)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FormEngine.Model exposing (..)
import Wizard.Common.FormEngine.Msgs exposing (Msg(..))
import Wizard.Common.Html exposing (emptyNode, faKeyClass, faSet)
import Wizard.Common.Locale exposing (l, lx)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.FormEngine.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.FormEngine.View"


type QuestionState
    = Default
    | Answered
    | Desirable


type alias FormViewConfig msg question option err =
    { customActions : List (String -> List String -> Html msg)
    , isDesirable : Maybe (question -> Bool)
    , disabled : Bool
    , getExtraQuestionClass : String -> Maybe String
    , renderer : FormRenderer msg question option err
    , appState : AppState
    }


type alias FormRenderer msg question option err =
    { renderQuestionLabel : question -> Html (Msg msg err)
    , renderQuestionDescription : question -> Html (Msg msg err)
    , renderOptionLabel : option -> Html (Msg msg err)
    , renderOptionBadges : option -> Html (Msg msg err)
    , renderOptionAdvice : option -> Html (Msg msg err)
    }


viewForm : FormViewConfig msg question option err -> Form question option -> Html (Msg msg err)
viewForm config form =
    div [ class "form-engine-form", classList [ ( "form-engine-form-disabled", config.disabled ) ] ]
        (List.indexedMap (viewFormElement form config [] []) form.elements)


stateValueToString : FormElementState -> String
stateValueToString =
    .value >> Maybe.map getStringReply >> Maybe.withDefault ""


identifierToChar : Int -> String
identifierToChar =
    (+) 97 >> Char.fromCode >> String.fromChar


viewFormElement : Form question option -> FormViewConfig msg question option err -> List String -> List String -> Int -> FormElement question option -> Html (Msg msg err)
viewFormElement form config path humanIdentifiers order formItem =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ String.fromInt <| order + 1 ]

        extraClass uuid =
            Maybe.withDefault "" <| config.getExtraQuestionClass uuid

        dataPath pathAttribute =
            attribute "data-path" <| String.join "." pathAttribute

        prefix formItemName =
            "question-" ++ formItemName
    in
    case formItem of
        StringFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", disabled config.disabled, type_ "text", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        TextFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (stateValueToString state /= "") newHumanIdentifiers
                , textarea [ class "form-control", disabled config.disabled, value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        NumberFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", disabled config.disabled, type_ "number", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        ChoiceFormElement descriptor options state ->
            div [ class <| "form-group form-group-choices " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (state.value /= Nothing) newHumanIdentifiers
                , config.renderer.renderQuestionDescription descriptor.question
                , div [] (List.indexedMap (viewChoice config (path ++ [ descriptor.name ]) descriptor state) options)
                , viewClearAnswer config.appState (state.value /= Nothing && not config.disabled) (path ++ [ descriptor.name ])
                , viewAdvice config state.value options
                , viewFollowUps form config (path ++ [ descriptor.name ]) newHumanIdentifiers state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (List.length items > 0) newHumanIdentifiers
                , config.renderer.renderQuestionDescription descriptor.question
                , div [] (List.indexedMap (viewGroupItem form config (path ++ [ descriptor.name ]) newHumanIdentifiers) items)
                , if not config.disabled then
                    button [ class "btn btn-outline-secondary link-with-icon", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ]
                        [ faSet "_global.add" config.appState
                        , lx_ "groupElement.add" config.appState
                        ]

                  else if List.length items == 0 then
                    i [] [ lx_ "groupElement.noAnswers" config.appState ]

                  else
                    emptyNode
                ]

        TypeHintFormElement descriptor typeHintConfig state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id <| prefix descriptor.name, dataPath (path ++ [ descriptor.name ]) ]
                [ viewLabel config descriptor path (stateValueToString state /= "") newHumanIdentifiers
                , input
                    [ class "form-control"
                    , type_ "text"
                    , disabled config.disabled
                    , value (stateValueToString state)
                    , onInput (InputTypehint (path ++ [ descriptor.name ]) descriptor.name << IntegrationReply << PlainValue)
                    , onFocus <| ShowTypeHints (path ++ [ descriptor.name ]) descriptor.name (stateValueToString state)
                    , onBlur HideTypeHints
                    ]
                    []
                , viewTypeHints config.appState form.typeHints path descriptor
                , viewIntegrationReplyExtra typeHintConfig state
                , config.renderer.renderQuestionDescription descriptor.question
                ]


viewIntegrationReplyExtra : TypeHintConfig -> FormElementState -> Html (Msg msg err)
viewIntegrationReplyExtra config state =
    case state.value of
        Just (IntegrationReply (IntegrationValue id name)) ->
            let
                url =
                    String.replace "${id}" id config.url

                logo =
                    if String.isEmpty config.logo then
                        emptyNode

                    else
                        img [ src config.logo ] []
            in
            p [ class "integration-extra" ]
                [ logo
                , a [ href url, target "_blank" ] [ text url ]
                ]

        _ ->
            emptyNode


viewTypeHint : List String -> String -> TypeHint -> Html (Msg msg err)
viewTypeHint path descriptorId typeHint =
    li []
        [ a [ onMouseDown <| InputTypehint path descriptorId <| IntegrationReply <| IntegrationValue typeHint.id typeHint.name ]
            [ text typeHint.name
            ]
        ]


viewTypeHints : AppState -> Maybe TypeHints -> List String -> FormItemDescriptor a -> Html (Msg msg err)
viewTypeHints appState typeHints path descriptor =
    let
        currentPath =
            path ++ [ descriptor.name ]

        visible =
            typeHints |> Maybe.map (.path >> (==) currentPath) |> Maybe.withDefault False

        hintsResult =
            typeHints
                |> Maybe.map .hints
                |> Maybe.withDefault Unset
    in
    if visible then
        let
            content =
                case hintsResult of
                    Success hints ->
                        ul [] (List.map (viewTypeHint currentPath descriptor.name) hints)

                    Loading ->
                        div [ class "loading" ]
                            [ faSet "_global.spinner" appState
                            , lx_ "typeHints.loading" appState
                            ]

                    Error err ->
                        div [ class "error" ]
                            [ faSet "_global.error" appState
                            , text err
                            ]

                    Unset ->
                        emptyNode
        in
        div [ class "typehints" ] [ content ]

    else
        emptyNode


viewLabel : FormViewConfig msg question option err -> FormItemDescriptor question -> List String -> Bool -> List String -> Html (Msg msg err)
viewLabel config descriptor path answered humanIdentifiers =
    let
        questionState =
            let
                desirable =
                    config.isDesirable
                        |> Maybe.map (\isDesirable -> isDesirable descriptor.question)
                        |> Maybe.withDefault False
            in
            case ( answered, desirable ) of
                ( True, _ ) ->
                    Answered

                ( _, True ) ->
                    Desirable

                _ ->
                    Default
    in
    label []
        [ span []
            [ span
                [ class "badge badge-secondary badge-human-identifier"
                , classList
                    [ ( "badge-secondary", questionState == Default )
                    , ( "badge-success", questionState == Answered )
                    , ( "badge-danger", questionState == Desirable )
                    ]
                ]
                [ text <| String.join "." humanIdentifiers ]
            , span
                [ classList
                    [ ( "text-success", questionState == Answered )
                    , ( "text-danger", questionState == Desirable )
                    ]
                ]
                [ config.renderer.renderQuestionLabel descriptor.question ]
            ]
        , viewCustomActions descriptor.name path config
        ]


viewCustomActions : String -> List String -> FormViewConfig msg question option err -> Html (Msg msg err)
viewCustomActions questionId path config =
    span [ class "custom-actions" ]
        (List.map (\f -> Html.map (CustomQuestionMsg questionId) <| f questionId path) config.customActions)


viewClearAnswer : AppState -> Bool -> List String -> Html (Msg msg err)
viewClearAnswer appState answered path =
    if answered then
        a [ class "clear-answer", onClick <| Clear path ]
            [ faSet "questionnaire.clearAnswer" appState
            , lx_ "optionsElement.clearAnswer" appState
            ]

    else
        emptyNode


viewGroupItem : Form question option -> FormViewConfig msg question option err -> List String -> List String -> Int -> ItemElement question option -> Html (Msg msg err)
viewGroupItem form config path humanIdentifiers index itemElement =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ identifierToChar index ]

        deleteButton =
            if not config.disabled then
                button [ class "btn btn-outline-danger btn-item-delete", onClick (GroupItemRemove path index) ]
                    [ faSet "_global.delete" config.appState ]

            else
                emptyNode
    in
    div [ class "item" ]
        [ div [ class "card bg-light  mb-5" ]
            [ div [ class "card-body" ] <|
                List.indexedMap (viewFormElement form config (path ++ [ fromInt index ]) newHumanIdentifiers) itemElement
            ]
        , deleteButton
        ]


viewChoice : FormViewConfig msg question option err -> List String -> FormItemDescriptor question -> FormElementState -> Int -> OptionElement question option -> Html (Msg msg err)
viewChoice config path parentDescriptor parentState order optionElement =
    let
        radioName =
            String.join "." (path ++ [ parentDescriptor.name ])

        humanIndentifier =
            identifierToChar order ++ ". "

        viewOption option value extra =
            div [ class "radio", classList [ ( "radio-selected", Just value == parentState.value ) ] ]
                [ label []
                    [ input [ type_ "radio", disabled config.disabled, name radioName, onClick (Input path value), checked (Just value == parentState.value) ] []
                    , text humanIndentifier
                    , config.renderer.renderOptionLabel option
                    , extra
                    , config.renderer.renderOptionBadges option
                    ]
                ]
    in
    case optionElement of
        SimpleOptionElement { option, name } ->
            viewOption option (AnswerReply name) emptyNode

        DetailedOptionElement { option, name } _ ->
            viewOption option
                (AnswerReply name)
                (i
                    [ class <| "expand-icon " ++ faKeyClass "questionnaire.followUpsIndication" config.appState
                    , title <| l_ "optionsElement.followUpTitle" config.appState
                    ]
                    []
                )


viewAdvice : FormViewConfig msg question option err -> Maybe ReplyValue -> List (OptionElement question option) -> Html (Msg msg err)
viewAdvice config value options =
    let
        getDescriptor option =
            case option of
                SimpleOptionElement descriptor ->
                    descriptor

                DetailedOptionElement descriptor _ ->
                    descriptor

        isSelected descriptor =
            case ( value, descriptor ) of
                ( Just v, { name } ) ->
                    name == getAnswerUuid v

                _ ->
                    False

        selectedDetailedOption =
            List.map getDescriptor options
                |> List.filter isSelected
                |> List.head
    in
    case selectedDetailedOption of
        Just descriptor ->
            config.renderer.renderOptionAdvice descriptor.option

        _ ->
            emptyNode


viewFollowUps : Form question option -> FormViewConfig msg question option err -> List String -> List String -> Maybe ReplyValue -> List (OptionElement question option) -> Html (Msg msg err)
viewFollowUps form config path humanIdentifiers value options =
    let
        isSelected ( _, option ) =
            case ( value, option ) of
                ( Just v, DetailedOptionElement { name } _ ) ->
                    name == getAnswerUuid v

                _ ->
                    False

        selectedDetailedOption =
            options
                |> List.indexedMap (\i o -> ( i, o ))
                |> List.filter isSelected
                |> List.head
    in
    case selectedDetailedOption of
        Just ( index, DetailedOptionElement descriptor items ) ->
            div [ class "followups-group" ]
                (List.indexedMap
                    (viewFormElement
                        form
                        config
                        (path ++ [ descriptor.name ])
                        (humanIdentifiers ++ [ identifierToChar index ])
                    )
                    items
                )

        _ ->
            emptyNode
