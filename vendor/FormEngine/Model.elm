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
        , decodeFormValues
        , encodeFormValues
        , getDescriptor
        , getFormValues
        , getOptionDescriptor
        )

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import List.Extra as List


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
    decode FormValue
        |> required "path" Decode.string
        |> required "value" Decode.string


encodeFormValues : FormValues -> Encode.Value
encodeFormValues formValues =
    Encode.list <| List.map encodeFormValue formValues


encodeFormValue : FormValue -> Encode.Value
encodeFormValue formValue =
    Encode.object
        [ ( "path", Encode.string formValue.path )
        , ( "value", Encode.string formValue.value )
        ]



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


createForm : FormTree -> FormValues -> List String -> Form
createForm formTree formValues defaultPath =
    { elements = List.map createFormElement formTree.items |> List.map (setInitialValue formValues defaultPath) }


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

                newState =
                    { state | value = Just numberOfItems }
            in
            GroupFormElement descriptor items itemElements newState


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



{- getting form values -}


getFormValues : List String -> Form -> FormValues
getFormValues defaultPath form =
    List.foldl (getFieldValue defaultPath) [] form.elements


getFieldValue : List String -> FormElement -> FormValues -> FormValues
getFieldValue path element values =
    case element of
        StringFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state

        NumberFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state

        TextFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state

        ChoiceFormElement descriptor options state ->
            let
                newValues =
                    applyFieldValue values (pathToKey path descriptor.name) state
            in
            List.foldl (getOptionValues (path ++ [ descriptor.name ])) newValues options

        GroupFormElement descriptor items itemElements state ->
            let
                newValues =
                    applyFieldValue values (pathToKey path descriptor.name) state
            in
            List.indexedFoldl (getItemValues (path ++ [ descriptor.name ])) newValues itemElements


getOptionValues : List String -> OptionElement -> FormValues -> FormValues
getOptionValues path option values =
    case option of
        DetailedOptionElement descriptor items ->
            List.foldl (getFieldValue (path ++ [ descriptor.name ])) values items

        _ ->
            values


getItemValues : List String -> Int -> ItemElement -> FormValues -> FormValues
getItemValues path index item values =
    List.foldl (getFieldValue (path ++ [ toString index ])) values item


pathToKey : List String -> String -> String
pathToKey path current =
    String.join "." (path ++ [ current ])


applyFieldValue : FormValues -> String -> FormElementState a -> FormValues
applyFieldValue values key state =
    case state.value of
        Just value ->
            values ++ [ { path = key, value = valueToString value } ]

        _ ->
            values


valueToString : a -> String
valueToString value =
    let
        str =
            toString value
    in
    if String.left 1 str == "\"" then
        String.dropRight 1 (String.dropLeft 1 str)
    else
        str
