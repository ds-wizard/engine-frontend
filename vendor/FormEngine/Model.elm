module FormEngine.Model
    exposing
        ( Form
        , FormElement(..)
        , FormElementState
        , FormItem(..)
        , FormItemDescriptor
        , FormTree
        , FormValues
        , ItemElement
        , Option(..)
        , OptionDescriptor
        , OptionElement(..)
        , createForm
        , createItemElement
        , getDescriptor
        , getOptionDescriptor
        )

import Dict exposing (Dict)


{- Types definitions -}


type alias FormItemDescriptor =
    { name : String
    , label : String
    , text : Maybe String
    }


type alias OptionDescriptor =
    { name : String
    , label : String
    , text : Maybe String
    }


type Option
    = SimpleOption OptionDescriptor
    | DetailedOption OptionDescriptor (List FormItem)


type FormItem
    = StringFormItem FormItemDescriptor
    | NumberFormItem FormItemDescriptor
    | TextFormItem FormItemDescriptor
    | ChoiceFormItem FormItemDescriptor (List Option)
    | GroupFormItem FormItemDescriptor (List FormItem)


type alias FormTree =
    { items : List FormItem
    }


type alias FormElementState value =
    { value : Maybe value
    , valid : Bool
    }


type OptionElement
    = SimpleOptionElement OptionDescriptor
    | DetailedOptionElement OptionDescriptor (List FormElement)


type alias ItemElement =
    List FormElement


type FormElement
    = StringFormElement FormItemDescriptor (FormElementState String)
    | NumberFormElement FormItemDescriptor (FormElementState Int)
    | TextFormElement FormItemDescriptor (FormElementState String)
    | ChoiceFormElement FormItemDescriptor (List OptionElement) (FormElementState String)
    | GroupFormElement FormItemDescriptor (List FormItem) (List ItemElement) (FormElementState Int)


type alias Form =
    { elements : List FormElement
    }


type alias FormValues =
    { values : Dict String String
    }



{- Type helpers -}


getOptionDescriptor : OptionElement -> OptionDescriptor
getOptionDescriptor option =
    case option of
        SimpleOptionElement descriptor ->
            descriptor

        DetailedOptionElement descriptor _ ->
            descriptor


getDescriptor : FormElement -> FormItemDescriptor
getDescriptor element =
    case element of
        StringFormElement descriptor _ ->
            descriptor

        NumberFormElement descriptor _ ->
            descriptor

        TextFormElement descriptor _ ->
            descriptor

        ChoiceFormElement descriptor _ _ ->
            descriptor

        GroupFormElement descriptor _ _ _ ->
            descriptor



{- Form creation -}


createForm : FormTree -> FormValues -> Form
createForm formTree formValues =
    { elements = List.map createFormElement formTree.items |> List.map (setInitialValue formValues []) }


createFormElement : FormItem -> FormElement
createFormElement item =
    case item of
        StringFormItem descriptor ->
            StringFormElement descriptor emptyFormElementState

        NumberFormItem descriptor ->
            NumberFormElement descriptor emptyFormElementState

        TextFormItem descriptor ->
            TextFormElement descriptor emptyFormElementState

        ChoiceFormItem descriptor options ->
            ChoiceFormElement descriptor (List.map createOptionElement options) emptyFormElementState

        GroupFormItem descriptor items ->
            GroupFormElement descriptor items [ createItemElement items ] emptyFormElementState


emptyFormElementState : FormElementState a
emptyFormElementState =
    { value = Nothing, valid = True }


createOptionElement : Option -> OptionElement
createOptionElement option =
    case option of
        SimpleOption descriptor ->
            SimpleOptionElement descriptor

        DetailedOption descriptor items ->
            DetailedOptionElement descriptor (List.map createFormElement items)


createItemElement : List FormItem -> ItemElement
createItemElement formItems =
    List.map createFormElement formItems


setInitialValue : FormValues -> List String -> FormElement -> FormElement
setInitialValue formValues path element =
    case element of
        StringFormElement descriptor state ->
            StringFormElement descriptor { state | value = getInitialValue formValues path descriptor.name }

        NumberFormElement descriptor state ->
            NumberFormElement descriptor { state | value = initialValueToInt <| getInitialValue formValues path descriptor.name }

        TextFormElement descriptor state ->
            TextFormElement descriptor { state | value = getInitialValue formValues path descriptor.name }

        ChoiceFormElement descriptor options state ->
            let
                newOptions =
                    List.map (setInitialValuesOption formValues (path ++ [ descriptor.name ])) options
            in
            ChoiceFormElement descriptor newOptions { state | value = getInitialValue formValues path descriptor.name }

        GroupFormElement descriptor items itemElements state ->
            let
                numberOfItems =
                    getInitialValue formValues path descriptor.name
                        |> initialValueToInt
                        |> Maybe.withDefault 1

                itemElements =
                    List.repeat numberOfItems (createItemElement items)
                        |> List.indexedMap (setInitialValuesItems formValues (path ++ [ descriptor.name ]))
            in
            GroupFormElement descriptor items itemElements state


getInitialValue : FormValues -> List String -> String -> Maybe String
getInitialValue formValues path current =
    let
        key =
            String.join "." (path ++ [ current ])
    in
    Dict.get key formValues.values


initialValueToInt : Maybe String -> Maybe Int
initialValueToInt =
    Maybe.map (String.toInt >> Result.withDefault 0)


setInitialValuesOption : FormValues -> List String -> OptionElement -> OptionElement
setInitialValuesOption formValues path option =
    case option of
        DetailedOptionElement descriptor items ->
            DetailedOptionElement descriptor (List.map (setInitialValue formValues (path ++ [ descriptor.name ])) items)

        _ ->
            option


setInitialValuesItems : FormValues -> List String -> Int -> ItemElement -> ItemElement
setInitialValuesItems formValues path index itemElement =
    List.map (setInitialValue formValues (path ++ [ toString index ])) itemElement
