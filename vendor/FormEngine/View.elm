module FormEngine.View exposing (viewForm)

import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


viewForm : List ( String, a ) -> Form -> Html (Msg a)
viewForm customActions form =
    div [ class "form-engine-form" ]
        (List.map (viewFormElement customActions []) form.elements)


viewFormElement : List ( String, a ) -> List String -> FormElement -> Html (Msg a)
viewFormElement customActions path formItem =
    case formItem of
        StringFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name customActions ]
                , input [ class "form-control", type_ "text", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                ]

        TextFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name customActions ]
                , textarea [ class "form-control", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                ]

        NumberFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name customActions ]
                , input [ class "form-control", type_ "number", value (state.value |> Maybe.map toString |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                ]

        ChoiceFormElement descriptor options state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name customActions ]
                , p [ class "form-text text-muted" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , div [] (List.map (viewChoice (path ++ [ descriptor.name ]) descriptor state) options)
                , viewAdvice state.value options
                , viewFollowUps customActions (path ++ [ descriptor.name ]) state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class "form-group" ]
                [ label [] [ text descriptor.label, viewCustomActions descriptor.name customActions ]
                , div [] (List.indexedMap (viewGroupItem customActions (path ++ [ descriptor.name ]) (List.length items)) items)
                , button [ class "btn btn-secondary", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [] ]
                ]


viewCustomActions : String -> List ( String, a ) -> Html (Msg a)
viewCustomActions questionId customActions =
    span [ class "feedback" ]
        (List.map (viewCustomAction questionId) customActions)


viewCustomAction : String -> ( String, a ) -> Html (Msg a)
viewCustomAction questionId ( icon, msg ) =
    a [ onClick <| CustomQuestionMsg questionId msg ]
        [ i [ class <| "fa " ++ icon ] [] ]


viewGroupItem : List ( String, a ) -> List String -> Int -> Int -> ItemElement -> Html (Msg a)
viewGroupItem customActions path numberOfItems index itemElement =
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
                ++ List.map (viewFormElement customActions (path ++ [ toString index ])) itemElement
        ]


viewChoice : List String -> FormItemDescriptor -> FormElementState String -> OptionElement -> Html (Msg a)
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


viewAdvice : Maybe String -> List OptionElement -> Html (Msg a)
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


adviceElement : Maybe String -> Html (Msg a)
adviceElement maybeAdvice =
    case maybeAdvice of
        Just advice ->
            div [ class "alert alert-info" ] [ text advice ]

        _ ->
            text ""


viewFollowUps : List ( String, a ) -> List String -> Maybe String -> List OptionElement -> Html (Msg a)
viewFollowUps customActions path value options =
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
                (List.map (viewFormElement customActions (path ++ [ descriptor.name ])) items)

        _ ->
            text ""
