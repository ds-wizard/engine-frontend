module FormEngine.View exposing (FormRenderer, FormViewConfig, viewForm)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown)
import String exposing (fromInt)


type QuestionState
    = Default
    | Answered
    | Desirable


type alias FormViewConfig msg question option err =
    { customActions : List ( String, msg )
    , isDesirable : Maybe (question -> Bool)
    , disabled : Bool
    , getExtraQuestionClass : String -> Maybe String
    , renderer : FormRenderer msg question option err
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
        (List.indexedMap (viewFormElement form config [] [] False) form.elements)


stateValueToString : FormElementState -> String
stateValueToString =
    .value >> Maybe.map getStringReply >> Maybe.withDefault ""


identifierToChar : Int -> String
identifierToChar =
    (+) 97 >> Char.fromCode >> String.fromChar


viewFormElement : Form question option -> FormViewConfig msg question option err -> List String -> List String -> Bool -> Int -> FormElement question option -> Html (Msg msg err)
viewFormElement form config path humanIdentifiers ignoreFirstHumanIdentifier order formItem =
    let
        newHumanIdentifiers =
            case ( ignoreFirstHumanIdentifier, order ) of
                ( True, 0 ) ->
                    humanIdentifiers

                ( True, _ ) ->
                    humanIdentifiers ++ [ String.fromInt order ]

                ( False, _ ) ->
                    humanIdentifiers ++ [ String.fromInt <| order + 1 ]

        extraClass uuid =
            Maybe.withDefault "" <| config.getExtraQuestionClass uuid
    in
    case formItem of
        StringFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", disabled config.disabled, type_ "text", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        TextFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , textarea [ class "form-control", disabled config.disabled, value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        NumberFormElement descriptor state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", disabled config.disabled, type_ "number", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , config.renderer.renderQuestionDescription descriptor.question
                ]

        ChoiceFormElement descriptor options state ->
            div [ class <| "form-group form-group-choices " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (state.value /= Nothing) newHumanIdentifiers
                , config.renderer.renderQuestionDescription descriptor.question
                , div [] (List.indexedMap (viewChoice config (path ++ [ descriptor.name ]) descriptor state) options)
                , viewClearAnswer (state.value /= Nothing && not config.disabled) (path ++ [ descriptor.name ])
                , viewAdvice config state.value options
                , viewFollowUps form config (path ++ [ descriptor.name ]) newHumanIdentifiers state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (List.length items > 0) newHumanIdentifiers
                , config.renderer.renderQuestionDescription descriptor.question
                , div [] (List.indexedMap (viewGroupItem form config (path ++ [ descriptor.name ]) newHumanIdentifiers) items)
                , if not config.disabled then
                    button [ class "btn btn-outline-secondary link-with-icon", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [], text "Add" ]

                  else
                    text ""
                ]

        TypeHintFormElement descriptor typeHintConfig state ->
            div [ class <| "form-group " ++ extraClass descriptor.name, id descriptor.name ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
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
                , viewTypeHints form.typeHints path descriptor
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
                        text ""

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


viewTypeHints : Maybe TypeHints -> List String -> FormItemDescriptor a -> Html (Msg msg err)
viewTypeHints typeHints path descriptor =
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
                            [ i [ class "fa fa-spinner fa-spin" ] []
                            , text "Loading"
                            ]

                    Error err ->
                        div [ class "error" ]
                            [ i [ class "fa fa-exclamation-triangle" ] []
                            , text err
                            ]

                    Unset ->
                        text ""
        in
        div [ class "typehints" ] [ content ]

    else
        text ""


viewLabel : FormViewConfig msg question option err -> FormItemDescriptor question -> Bool -> List String -> Html (Msg msg err)
viewLabel config descriptor answered humanIdentifiers =
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
        , viewCustomActions descriptor.name config
        ]


viewCustomActions : String -> FormViewConfig msg question option err -> Html (Msg msg err)
viewCustomActions questionId config =
    -- temporary fix since item name will be removed in the future versions
    if questionId /= "itemName" then
        span [ class "custom-actions" ]
            (List.map (viewCustomAction questionId) config.customActions)

    else
        text ""


viewCustomAction : String -> ( String, msg ) -> Html (Msg msg err)
viewCustomAction questionId ( icon, msg ) =
    a [ onClick <| CustomQuestionMsg questionId msg ]
        [ i [ class <| "fa " ++ icon ] [] ]


viewClearAnswer : Bool -> List String -> Html (Msg msg err)
viewClearAnswer answered path =
    if answered then
        a [ class "clear-answer", onClick <| Clear path ]
            [ i [ class "fa fa-undo" ] []
            , text "Clear answer"
            ]

    else
        text ""


viewGroupItem : Form question option -> FormViewConfig msg question option err -> List String -> List String -> Int -> ItemElement question option -> Html (Msg msg err)
viewGroupItem form config path humanIdentifiers index itemElement =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ identifierToChar index ]

        deleteButton =
            if not config.disabled then
                button [ class "btn btn-outline-danger btn-item-delete", onClick (GroupItemRemove path index) ]
                    [ i [ class "fa fa-trash-o" ] [] ]

            else
                text ""

        ignoreFirstIdentifier =
            itemElement
                |> List.head
                |> Maybe.map
                    (\f ->
                        case f of
                            StringFormElement descriptor _ ->
                                descriptor.name == "itemName"

                            _ ->
                                False
                    )
                |> Maybe.withDefault False
    in
    div [ class "item" ]
        [ div [ class "card bg-light  mb-5" ]
            [ div [ class "card-body" ] <|
                List.indexedMap (viewFormElement form config (path ++ [ fromInt index ]) newHumanIdentifiers ignoreFirstIdentifier) itemElement
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
            viewOption option (AnswerReply name) (text "")

        DetailedOptionElement { option, name } _ ->
            viewOption option (AnswerReply name) (i [ class "expand-icon fa fa-list-ul", title "This option leads to some follow up questions" ] [])


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
            text ""


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
                        False
                    )
                    items
                )

        _ ->
            text ""
