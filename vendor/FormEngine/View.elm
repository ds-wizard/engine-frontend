module FormEngine.View exposing (FormViewConfig, viewForm)

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


type alias FormViewConfig msg a err =
    { customActions : List ( String, msg )
    , viewExtraData : Maybe (a -> Html (Msg msg err))
    , isDesirable : Maybe (a -> Bool)
    }


viewForm : FormViewConfig msg a err -> Form a -> Html (Msg msg err)
viewForm config form =
    div [ class "form-engine-form" ]
        (List.indexedMap (viewFormElement form config [] [] False) form.elements)


stateValueToString : FormElementState -> String
stateValueToString =
    .value >> Maybe.map getStringReply >> Maybe.withDefault ""


identifierToChar : Int -> String
identifierToChar =
    (+) 97 >> Char.fromCode >> String.fromChar


viewFormElement : Form a -> FormViewConfig msg a err -> List String -> List String -> Bool -> Int -> FormElement a -> Html (Msg msg err)
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
    in
    case formItem of
        StringFormElement descriptor state ->
            div [ class "form-group" ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", type_ "text", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
                ]

        TextFormElement descriptor state ->
            div [ class "form-group" ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , textarea [ class "form-control", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
                ]

        NumberFormElement descriptor state ->
            div [ class "form-group" ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , input [ class "form-control", type_ "number", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
                ]

        ChoiceFormElement descriptor options state ->
            div [ class "form-group form-group-choices" ]
                [ viewLabel config descriptor (state.value /= Nothing) newHumanIdentifiers
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
                , div [] (List.indexedMap (viewChoice (path ++ [ descriptor.name ]) descriptor state) options)
                , viewClearAnswer (state.value /= Nothing) (path ++ [ descriptor.name ])
                , viewAdvice state.value options
                , viewFollowUps form config (path ++ [ descriptor.name ]) newHumanIdentifiers state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class "form-group" ]
                [ viewLabel config descriptor (List.length items > 0) newHumanIdentifiers
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
                , div [] (List.indexedMap (viewGroupItem form config (path ++ [ descriptor.name ]) newHumanIdentifiers) items)
                , button [ class "btn btn-outline-secondary link-with-icon", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [], text "Add" ]
                ]

        TypeHintFormElement descriptor typeHintConfig state ->
            div [ class "form-group" ]
                [ viewLabel config descriptor (stateValueToString state /= "") newHumanIdentifiers
                , input
                    [ class "form-control"
                    , type_ "text"
                    , value (stateValueToString state)
                    , onInput (InputTypehint (path ++ [ descriptor.name ]) descriptor.name << IntegrationReply << PlainValue)
                    , onFocus <| ShowTypeHints (path ++ [ descriptor.name ]) descriptor.name (stateValueToString state)
                    , onBlur HideTypeHints
                    ]
                    []
                , viewTypeHints form.typeHints path descriptor
                , viewIntegrationReplyExtra typeHintConfig state
                , viewDescription descriptor.text
                , viewExtraData config descriptor.extraData
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


viewLabel : FormViewConfig msg a err -> FormItemDescriptor a -> Bool -> List String -> Html (Msg msg err)
viewLabel config descriptor answered humanIdentifiers =
    let
        questionState =
            let
                desirable =
                    config.isDesirable
                        |> Maybe.andThen (\isDesirable -> Maybe.map isDesirable descriptor.extraData)
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
                [ text descriptor.label ]
            ]
        , viewCustomActions descriptor.name config
        ]


viewDescription : Maybe String -> Html (Msg msg err)
viewDescription descriptionText =
    descriptionText
        |> Maybe.map (\t -> p [ class "form-text text-muted" ] [ text t ])
        |> Maybe.withDefault (text "")


viewCustomActions : String -> FormViewConfig msg a err -> Html (Msg msg err)
viewCustomActions questionId config =
    span [ class "custom-actions" ]
        (List.map (viewCustomAction questionId) config.customActions)


viewCustomAction : String -> ( String, msg ) -> Html (Msg msg err)
viewCustomAction questionId ( icon, msg ) =
    a [ onClick <| CustomQuestionMsg questionId msg ]
        [ i [ class <| "fa " ++ icon ] [] ]


viewExtraData : FormViewConfig msg a err -> Maybe a -> Html (Msg msg err)
viewExtraData config extraData =
    case ( config.viewExtraData, extraData ) of
        ( Just view, Just data ) ->
            view data

        _ ->
            text ""


viewClearAnswer : Bool -> List String -> Html (Msg msg err)
viewClearAnswer answered path =
    if answered then
        a [ class "clear-answer", onClick <| Clear path ]
            [ i [ class "fa fa-undo" ] []
            , text "Clear answer"
            ]

    else
        text ""


viewGroupItem : Form a -> FormViewConfig msg a err -> List String -> List String -> Int -> ItemElement a -> Html (Msg msg err)
viewGroupItem form config path humanIdentifiers index itemElement =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ identifierToChar index ]

        deleteButton =
            button [ class "btn btn-outline-danger btn-item-delete", onClick (GroupItemRemove path index) ]
                [ i [ class "fa fa-trash-o" ] [] ]
    in
    div [ class "item" ]
        [ div [ class "card bg-light  mb-5" ]
            [ div [ class "card-body" ] <|
                List.indexedMap (viewFormElement form config (path ++ [ fromInt index ]) newHumanIdentifiers True) itemElement
            ]
        , deleteButton
        ]


viewChoice : List String -> FormItemDescriptor a -> FormElementState -> Int -> OptionElement a -> Html (Msg msg err)
viewChoice path parentDescriptor parentState order optionElement =
    let
        radioName =
            String.join "." (path ++ [ parentDescriptor.name ])

        humanIndentifier =
            identifierToChar order

        viewBadge ( cssClass, title ) =
            span [ class <| "badge " ++ cssClass ] [ text title ]

        viewBadges mbBadges =
            case mbBadges of
                Just badges ->
                    div [ class "badges" ] (List.map viewBadge badges)

                Nothing ->
                    text ""

        viewOption title value extra badges =
            div [ class "radio", classList [ ( "radio-selected", Just value == parentState.value ) ] ]
                [ label []
                    [ input [ type_ "radio", name radioName, onClick (Input path value), checked (Just value == parentState.value) ] []
                    , text <| humanIndentifier ++ ". " ++ title
                    , extra
                    , viewBadges badges
                    ]
                ]
    in
    case optionElement of
        SimpleOptionElement { name, label, badges } ->
            viewOption label (AnswerReply name) (text "") badges

        DetailedOptionElement { name, label, badges } _ ->
            viewOption label (AnswerReply name) (i [ class "expand-icon fa fa-list-ul", title "This option leads to some follow up questions" ] []) badges


viewAdvice : Maybe ReplyValue -> List (OptionElement a) -> Html (Msg msg err)
viewAdvice value options =
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
            adviceElement descriptor.text

        _ ->
            text ""


adviceElement : Maybe String -> Html (Msg msg err)
adviceElement maybeAdvice =
    case maybeAdvice of
        Just advice ->
            div [ class "alert alert-info" ] [ text advice ]

        _ ->
            text ""


viewFollowUps : Form a -> FormViewConfig msg a err -> List String -> List String -> Maybe ReplyValue -> List (OptionElement a) -> Html (Msg msg err)
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
