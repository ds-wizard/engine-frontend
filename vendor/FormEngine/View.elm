module FormEngine.View exposing (viewForm)

import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


viewForm : Form -> Html Msg
viewForm form =
    div [ class "form-engine-form" ]
        (List.map (viewFormElement []) form.elements)


viewFormElement : List String -> FormElement -> Html Msg
viewFormElement path formItem =
    case formItem of
        StringFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [ class "control-label" ] [ text descriptor.label ]
                , input [ class "form-control", type_ "text", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "help-block" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                ]

        TextFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [ class "control-label" ] [ text descriptor.label ]
                , textarea [ class "form-control", value (state.value |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                , p [ class "help-block" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                ]

        NumberFormElement descriptor state ->
            div [ class "form-group" ]
                [ label [ class "control-label" ] [ text descriptor.label ]
                , input [ class "form-control", type_ "number", value (state.value |> Maybe.map toString |> Maybe.withDefault ""), onInput (Input (path ++ [ descriptor.name ])) ] []
                ]

        ChoiceFormElement descriptor options state ->
            div [ class "form-group" ]
                [ label [ class "control-label" ] [ text descriptor.label ]
                , p [ class "help-block" ] [ text (descriptor.text |> Maybe.withDefault "") ]
                , div [] (List.map (viewChoice (path ++ [ descriptor.name ]) descriptor state) options)
                , viewAdvice state.value options
                , viewFollowUps (path ++ [ descriptor.name ]) state.value options
                ]

        GroupFormElement descriptor _ items state ->
            div [ class "form-group" ]
                [ label [ class "control-label" ] [ text descriptor.label ]
                , div [] (List.indexedMap (viewGroupItem (path ++ [ descriptor.name ]) (List.length items)) items)
                , button [ class "btn btn-default", onClick (GroupItemAdd (path ++ [ descriptor.name ])) ] [ i [ class "fa fa-plus" ] [] ]
                ]


viewGroupItem : List String -> Int -> Int -> ItemElement -> Html Msg
viewGroupItem path numberOfItems index itemElement =
    let
        deleteButton =
            if numberOfItems == 1 then
                text ""
            else
                button [ class "btn btn-default btn-item-delete", onClick (GroupItemRemove path index) ]
                    [ i [ class "fa fa-trash-o" ] [] ]
    in
    div [ class "well well-item" ] <|
        [ deleteButton ]
            ++ List.map (viewFormElement (path ++ [ toString index ])) itemElement


viewChoice : List String -> FormItemDescriptor -> FormElementState String -> OptionElement -> Html Msg
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


viewAdvice : Maybe String -> List OptionElement -> Html Msg
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


adviceElement : Maybe String -> Html Msg
adviceElement maybeAdvice =
    case maybeAdvice of
        Just advice ->
            div [ class "alert alert-info" ] [ text advice ]

        _ ->
            text ""


viewFollowUps : List String -> Maybe String -> List OptionElement -> Html Msg
viewFollowUps path value options =
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
                (List.map (viewFormElement (path ++ [ descriptor.name ])) items)

        _ ->
            text ""
