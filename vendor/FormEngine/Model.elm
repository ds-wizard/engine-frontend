module FormEngine.Model exposing
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
    , decodeFormValues
    , encodeFormValues
    , getDescriptor
    , getFormValues
    , getOptionDescriptor
    )

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (..)
import List.Extra as List
import String exposing (fromFloat, fromInt)



{- Types definitions -}


type alias FormItemDescriptor a =
    { name : String
    , label : String
    , text : Maybe String
    , extraData : Maybe a
    }


type alias OptionDescriptor =
    { name : String
    , label : String
    , text : Maybe String
    }


type Option a
    = SimpleOption OptionDescriptor
    | DetailedOption OptionDescriptor (List (FormItem a))


type FormItem a
    = StringFormItem (FormItemDescriptor a)
    | NumberFormItem (FormItemDescriptor a)
    | TextFormItem (FormItemDescriptor a)
    | ChoiceFormItem (FormItemDescriptor a) (List (Option a))
    | GroupFormItem (FormItemDescriptor a) (List (FormItem a))


type alias FormTree a =
    { items : List (FormItem a)
    }


type alias FormElementState value =
    { value : Maybe value
    , valid : Bool
    }


type OptionElement a
    = SimpleOptionElement OptionDescriptor
    | DetailedOptionElement OptionDescriptor (List (FormElement a))


type alias ItemElement a =
    List (FormElement a)


type FormElement a
    = StringFormElement (FormItemDescriptor a) (FormElementState String)
    | NumberFormElement (FormItemDescriptor a) (FormElementState Int)
    | TextFormElement (FormItemDescriptor a) (FormElementState String)
    | ChoiceFormElement (FormItemDescriptor a) (List (OptionElement a)) (FormElementState String)
    | GroupFormElement (FormItemDescriptor a) (List (FormItem a)) (List (ItemElement a)) (FormElementState Int)


type alias Form a =
    { elements : List (FormElement a)
    }


type alias FormValues =
    List FormValue


type alias FormValue =
    { path : String
    , value : String
    }



{- Decoders and encoders -}


decodeFormValues : Decoder FormValues
decodeFormValues =
    Decode.list decodeFormValue


decodeFormValue : Decoder FormValue
decodeFormValue =
    Decode.succeed FormValue
        |> required "path" Decode.string
        |> required "value" Decode.string


encodeFormValues : FormValues -> Encode.Value
encodeFormValues formValues =
    Encode.list encodeFormValue formValues


encodeFormValue : FormValue -> Encode.Value
encodeFormValue formValue =
    Encode.object
        [ ( "path", Encode.string formValue.path )
        , ( "value", Encode.string formValue.value )
        ]



{- Type helpers -}


getOptionDescriptor : OptionElement a -> OptionDescriptor
getOptionDescriptor option =
    case option of
        SimpleOptionElement descriptor ->
            descriptor

        DetailedOptionElement descriptor _ ->
            descriptor


getDescriptor : FormElement a -> FormItemDescriptor a
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


createForm : FormTree a -> FormValues -> List String -> Form a
createForm formTree formValues defaultPath =
    { elements = List.map createFormElement formTree.items |> List.map (setInitialValue formValues defaultPath) }


createFormElement : FormItem a -> FormElement a
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


createOptionElement : Option a -> OptionElement a
createOptionElement option =
    case option of
        SimpleOption descriptor ->
            SimpleOptionElement descriptor

        DetailedOption descriptor items ->
            DetailedOptionElement descriptor (List.map createFormElement items)


createItemElement : List (FormItem a) -> ItemElement a
createItemElement formItems =
    List.map createFormElement formItems


setInitialValue : FormValues -> List String -> FormElement a -> FormElement a
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
                        |> Maybe.withDefault 0

                newItemElements =
                    List.repeat numberOfItems (createItemElement items)
                        |> List.indexedMap (setInitialValuesItems formValues (path ++ [ descriptor.name ]))

                newState =
                    { state | value = Just numberOfItems }
            in
            GroupFormElement descriptor items newItemElements newState


getInitialValue : FormValues -> List String -> String -> Maybe String
getInitialValue formValues path current =
    let
        key =
            String.join "." (path ++ [ current ])
    in
    List.find (.path >> (==) key) formValues
        |> Maybe.map .value


initialValueToInt : Maybe String -> Maybe Int
initialValueToInt =
    Maybe.map (String.toInt >> Maybe.withDefault 0)


setInitialValuesOption : FormValues -> List String -> OptionElement a -> OptionElement a
setInitialValuesOption formValues path option =
    case option of
        DetailedOptionElement descriptor items ->
            DetailedOptionElement descriptor (List.map (setInitialValue formValues (path ++ [ descriptor.name ])) items)

        _ ->
            option


setInitialValuesItems : FormValues -> List String -> Int -> ItemElement a -> ItemElement a
setInitialValuesItems formValues path index itemElement =
    List.map (setInitialValue formValues (path ++ [ fromInt index ])) itemElement



{- getting form values -}


getFormValues : List String -> Form a -> FormValues
getFormValues defaultPath form =
    List.foldl (getFieldValue defaultPath) [] form.elements


getFieldValue : List String -> FormElement a -> FormValues -> FormValues
getFieldValue path element values =
    case element of
        StringFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state.value

        NumberFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) (Maybe.map fromInt state.value)

        TextFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state.value

        ChoiceFormElement descriptor options state ->
            let
                newValues =
                    applyFieldValue values (pathToKey path descriptor.name) state.value
            in
            List.foldl (getOptionValues (path ++ [ descriptor.name ])) newValues options

        GroupFormElement descriptor items itemElements state ->
            let
                newValues =
                    applyFieldValue values (pathToKey path descriptor.name) (Maybe.map fromInt state.value)
            in
            List.indexedFoldl (getItemValues (path ++ [ descriptor.name ])) newValues itemElements


getOptionValues : List String -> OptionElement a -> FormValues -> FormValues
getOptionValues path option values =
    case option of
        DetailedOptionElement descriptor items ->
            List.foldl (getFieldValue (path ++ [ descriptor.name ])) values items

        _ ->
            values


getItemValues : List String -> Int -> ItemElement a -> FormValues -> FormValues
getItemValues path index item values =
    List.foldl (getFieldValue (path ++ [ fromInt index ])) values item


pathToKey : List String -> String -> String
pathToKey path current =
    String.join "." (path ++ [ current ])


applyFieldValue : FormValues -> String -> Maybe String -> FormValues
applyFieldValue values key stringValue =
    case stringValue of
        Just value ->
            values ++ [ { path = key, value = value } ]

        Nothing ->
            values ++ [ { path = key, value = "" } ]
