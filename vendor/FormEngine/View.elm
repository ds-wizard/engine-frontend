module FormEngine.View exposing (FormViewConfig, viewForm)

import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias FormViewConfig msg a =
    { customActions : List ( String, msg )
    , viewExtraData : Maybe (a -> Html (Msg msg))
    }


viewForm : FormViewConfig msg a -> Form a -> Html (Msg msg)
viewForm config form =
    div [ class "form-engine-form" ]
        (List.map (viewFormElement config []) form.elements)


viewFormElement : FormViewConfig msg a -> List String -> FormElement a -> Html (Msg msg)
viewFormElement config path formItem =
    case formItem of
        StringFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , input [ class "form-control", type_ "text", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        TextFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , textarea [ class "form-control", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        NumberFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , input [ class "form-control", type_ "number", value (state.value |> Maybe.map toString |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                ]

        ChoiceFormElement descriptor options state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                , div [] (List.map (viewChoice (path ++ [ descriptor.name ]) descriptor state) options)
                , viewAdvice state.value options
                , viewFollowUps config (path ++ [ descriptor.name ]) state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name config ]
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , viewExtraData config descriptor.extraData
                , div [] (List.indexedMap (viewGroupItem config (path ++ [ descriptor.name ]) (List.length items)) items)
                , button [ class "btn btn-secondary", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [] ]
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


viewGroupItem : FormViewConfig msg a -> List String -> Int -> Int -> ItemElement a -> Html (Msg msg)
viewGroupItem config path numberOfItems index itemElement =
    let
        deleteButton =
            if numberOfItems == 1 then
                text ""
            else
                button [ class "btn btn-outline-danger btn-item-delete", onClick (GroupItemRemove path index) ]
                    [ i [ class "fa fa-trash-o" ] [] ]
    in
    div [ class "card bg-light item mb-5" ]
        [ div [ class "card-body" ] <|
            [ deleteButton ]
                ++ List.map (viewFormElement config (path ++ [ toString index ])) itemElement
        ]


viewChoice : List String -> FormItemDescriptor a -> FormElementState String -> OptionElement a -> Html (Msg msg)
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
            viewOption label name (text "")

        DetailedOptionElement { name, label } _ ->
            viewOption label name (i [ class "expand-icon fa fa-list-ul", title "This option leads to some follow up questions" ] [])


viewAdvice : Maybe String -> List (OptionElement a) -> Html (Msg msg)
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
                    name == v

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


viewFollowUps : FormViewConfig msg a -> List String -> Maybe String -> List (OptionElement a) -> Html (Msg msg)
viewFollowUps config path value options =
    let
        isSelected option =
            case ( value, option ) of
                ( Just v, DetailedOptionElement { name } _ ) ->
                    name == v

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
