module FormEngine.View exposing (FormViewConfig, viewForm)

import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)


type alias FormViewConfig msg a =
    { customActions : List ( String, msg )
    , viewExtraData : Maybe (a -> Html (Msg msg))
    }


viewForm : FormViewConfig msg a -> Form a -> Html (Msg msg)
viewForm config form =
    div [ class "form-engine-form" ]
        (List.map (viewFormElement config []) form.elements)


stateValueToString : FormElementState -> String
stateValueToString =
    .value >> Maybe.map getStringReply >> Maybe.withDefault ""


viewFormElement : FormViewConfig msg a -> List String -> FormElement a -> Html (Msg msg)
viewFormElement config path formItem =
    case formItem of
        StringFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , input [ class "form-control", type_ "text", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        TextFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , textarea [ class "form-control", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        NumberFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , input [ class "form-control", type_ "number", value (stateValueToString state), onInput (Input (path ++ [ descriptor.name ]) << StringReply) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        ChoiceFormElement descriptor options state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                , div [] (List.map (viewChoice (path ++ [ descriptor.name ]) descriptor state) options)
                , viewClearAnswer (state.value /= Nothing) (path ++ [ descriptor.name ])
                , viewAdvice state.value options
                , viewFollowUps config (path ++ [ descriptor.name ]) state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                , div [] (List.indexedMap (viewGroupItem config (path ++ [ descriptor.name ])) items)
                , button [ class "btn btn-secondary link-with-icon", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [], text "Add" ]
                ]


viewCustomActions : String -> FormViewConfig msg a -> Html (Msg msg)
viewCustomActions questionId config =
    span [ class "custom-actions" ]
        (List.map (viewCustomAction questionId) config.customActions)


viewCustomAction : String -> ( String, msg ) -> Html (Msg msg)
viewCustomAction questionId ( icon, msg ) =
    a [ onClick <| CustomQuestionMsg questionId msg ]
        [ i [ class <| "fa " ++ icon ] [] ]


viewExtraData : FormViewConfig msg a -> Maybe a -> Html (Msg msg)
viewExtraData config extraData =
    case ( config.viewExtraData, extraData ) of
        ( Just view, Just data ) ->
            view data

        _ ->
            text ""


viewClearAnswer : Bool -> List String -> Html (Msg msg)
viewClearAnswer answered path =
    if answered then
        a [ class "clear-answer", onClick <| Clear path ]
            [ i [ class "fa fa-undo" ] []
            , text "Clear answer"
            ]

    else
        text ""


viewGroupItem : FormViewConfig msg a -> List String -> Int -> ItemElement a -> Html (Msg msg)
viewGroupItem config path index itemElement =
    let
        deleteButton =
            button [ class "btn btn-outline-danger btn-item-delete", onClick (GroupItemRemove path index) ]
                [ i [ class "fa fa-trash-o" ] [] ]
    in
    div [ class "card bg-light item mb-5" ]
        [ div [ class "card-body" ] <|
            [ deleteButton ]
                ++ List.map (viewFormElement config (path ++ [ fromInt index ])) itemElement
        ]


viewChoice : List String -> FormItemDescriptor a -> FormElementState -> OptionElement a -> Html (Msg msg)
viewChoice path parentDescriptor parentState optionElement =
    let
        radioName =
            String.join "." (path ++ [ parentDescriptor.name ])

        viewOption title value extra =
            div [ class "radio" ]
                [ label []
                    [ input [ type_ "radio", name radioName, onClick (Input path value), checked (Just value == parentState.value) ] []
                    , text title
                    , extra
                    ]
                ]
    in
    case optionElement of
        SimpleOptionElement { name, label } ->
            viewOption label (AnswerReply name) (text "")

        DetailedOptionElement { name, label } _ ->
            viewOption label (AnswerReply name) (i [ class "expand-icon fa fa-list-ul", title "This option leads to some follow up questions" ] [])


viewAdvice : Maybe ReplyValue -> List (OptionElement a) -> Html (Msg msg)
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


adviceElement : Maybe String -> Html (Msg msg)
adviceElement maybeAdvice =
    case maybeAdvice of
        Just advice ->
            div [ class "alert alert-info" ] [ text advice ]

        _ ->
            text ""


viewFollowUps : FormViewConfig msg a -> List String -> Maybe ReplyValue -> List (OptionElement a) -> Html (Msg msg)
viewFollowUps config path value options =
    let
        isSelected option =
            case ( value, option ) of
                ( Just v, DetailedOptionElement { name } _ ) ->
                    name == getAnswerUuid v

                _ ->
                    False

        selectedDetailedOption =
            List.filter isSelected options |> List.head
    in
    case selectedDetailedOption of
        Just (DetailedOptionElement descriptor items) ->
            div [ class "followups-group" ]
                (List.map (viewFormElement config (path ++ [ descriptor.name ])) items)

        _ ->
            text ""
