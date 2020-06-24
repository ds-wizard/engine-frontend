module Wizard.Common.FormEngine.Model exposing
    ( Form
    , FormElement(..)
    , FormElementState
    , FormItem(..)
    , FormItemDescriptor
    , FormTree
    , ItemElement
    , Option(..)
    , OptionDescriptor
    , OptionElement(..)
    , TypeHint
    , TypeHintConfig
    , TypeHints
    , createForm
    , createItemElement
    , getDescriptor
    , getFormValues
    , getOptionDescriptor
    , setTypeHintsResult
    )

import ActionResult exposing (ActionResult)
import Debounce exposing (Debounce)
import List.Extra as List
import Shared.Data.QuestionnaireDetail.FormValue exposing (FormValue)
import Shared.Data.QuestionnaireDetail.FormValue.ReplyValue as ReplyValue exposing (ReplyValue(..))
import String exposing (fromInt)


type alias FormItemDescriptor question =
    { name : String
    , question : question
    }


type alias OptionDescriptor option =
    { name : String
    , option : option
    }


type Option question option
    = SimpleOption (OptionDescriptor option)
    | DetailedOption (OptionDescriptor option) (List (FormItem question option))


type alias TypeHintConfig =
    { logo : String
    , url : String
    }


type FormItem question option
    = StringFormItem (FormItemDescriptor question)
    | NumberFormItem (FormItemDescriptor question)
    | TextFormItem (FormItemDescriptor question)
    | TypeHintFormItem (FormItemDescriptor question) TypeHintConfig
    | ChoiceFormItem (FormItemDescriptor question) (List (Option question option))
    | GroupFormItem (FormItemDescriptor question) (List (FormItem question option))


type alias FormTree question option =
    { items : List (FormItem question option)
    }


type alias FormElementState =
    { value : Maybe ReplyValue
    , valid : Bool
    }


type OptionElement question option
    = SimpleOptionElement (OptionDescriptor option)
    | DetailedOptionElement (OptionDescriptor option) (List (FormElement question option))


type alias ItemElement question option =
    List (FormElement question option)


type FormElement question option
    = StringFormElement (FormItemDescriptor question) FormElementState
    | NumberFormElement (FormItemDescriptor question) FormElementState
    | TextFormElement (FormItemDescriptor question) FormElementState
    | TypeHintFormElement (FormItemDescriptor question) TypeHintConfig FormElementState
    | ChoiceFormElement (FormItemDescriptor question) (List (OptionElement question option)) FormElementState
    | GroupFormElement (FormItemDescriptor question) (List (FormItem question option)) (List (ItemElement question option)) FormElementState


type alias TypeHint =
    { id : String
    , name : String
    }


type alias TypeHints =
    { path : List String
    , hints : ActionResult (List TypeHint)
    }


type alias Form question option =
    { elements : List (FormElement question option)
    , typeHints : Maybe TypeHints
    , debounce : Debounce ( String, String )
    }


getOptionDescriptor : OptionElement question option -> OptionDescriptor option
getOptionDescriptor option =
    case option of
        SimpleOptionElement descriptor ->
            descriptor

        DetailedOptionElement descriptor _ ->
            descriptor


getDescriptor : FormElement question option -> FormItemDescriptor question
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

        TypeHintFormElement descriptor _ _ ->
            descriptor


setTypeHintsResult : ActionResult (List TypeHint) -> Form question option -> Form question option
setTypeHintsResult typeHintsResult form =
    let
        set result typeHints =
            { typeHints | hints = result }
    in
    { form | typeHints = Maybe.map (set typeHintsResult) form.typeHints }



{- Form creation -}


createForm : FormTree question option -> List FormValue -> List String -> Form question option
createForm formTree formValues defaultPath =
    { elements = List.map createFormElement formTree.items |> List.map (setInitialValue formValues defaultPath)
    , typeHints = Nothing
    , debounce = Debounce.init
    }


createFormElement : FormItem question option -> FormElement question option
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
            GroupFormElement descriptor items [] emptyFormElementState

        TypeHintFormItem descriptor typeHintConfig ->
            TypeHintFormElement descriptor typeHintConfig emptyFormElementState


emptyFormElementState : FormElementState
emptyFormElementState =
    { value = Nothing, valid = True }


createOptionElement : Option question option -> OptionElement question option
createOptionElement option =
    case option of
        SimpleOption descriptor ->
            SimpleOptionElement descriptor

        DetailedOption descriptor items ->
            DetailedOptionElement descriptor (List.map createFormElement items)


createItemElement : List (FormItem question option) -> ItemElement question option
createItemElement formItems =
    List.map createFormElement formItems


setInitialValue : List FormValue -> List String -> FormElement question option -> FormElement question option
setInitialValue formValues path element =
    case element of
        StringFormElement descriptor state ->
            StringFormElement descriptor { state | value = getInitialValue formValues path descriptor.name }

        NumberFormElement descriptor state ->
            NumberFormElement descriptor { state | value = getInitialValue formValues path descriptor.name }

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
                        |> Maybe.map ReplyValue.getItemListCount
                        |> Maybe.withDefault 0

                newItemElements =
                    List.repeat numberOfItems (createItemElement items)
                        |> List.indexedMap (setInitialValuesItems formValues (path ++ [ descriptor.name ]))

                newState =
                    { state | value = Just <| ItemListReply numberOfItems }
            in
            GroupFormElement descriptor items newItemElements newState

        TypeHintFormElement descriptor typeHintConfig state ->
            TypeHintFormElement descriptor typeHintConfig { state | value = getInitialValue formValues path descriptor.name }


getInitialValue : List FormValue -> List String -> String -> Maybe ReplyValue
getInitialValue formValues path current =
    let
        key =
            String.join "." (path ++ [ current ])
    in
    List.find (.path >> (==) key) formValues
        |> Maybe.map .value


setInitialValuesOption : List FormValue -> List String -> OptionElement question option -> OptionElement question option
setInitialValuesOption formValues path option =
    case option of
        DetailedOptionElement descriptor items ->
            DetailedOptionElement descriptor (List.map (setInitialValue formValues (path ++ [ descriptor.name ])) items)

        _ ->
            option


setInitialValuesItems : List FormValue -> List String -> Int -> ItemElement question option -> ItemElement question option
setInitialValuesItems formValues path index itemElement =
    List.map (setInitialValue formValues (path ++ [ fromInt index ])) itemElement



{- getting form values -}


getFormValues : List String -> Form question option -> List FormValue
getFormValues defaultPath form =
    List.foldl (getFieldValue defaultPath) [] form.elements


getFieldValue : List String -> FormElement question option -> List FormValue -> List FormValue
getFieldValue path element values =
    case element of
        StringFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state.value

        NumberFormElement descriptor state ->
            applyFieldValue values (pathToKey path descriptor.name) state.value

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
                    applyFieldValue values (pathToKey path descriptor.name) state.value
            in
            List.indexedFoldl (getItemValues (path ++ [ descriptor.name ])) newValues itemElements

        TypeHintFormElement descriptor _ state ->
            applyFieldValue values (pathToKey path descriptor.name) state.value


getOptionValues : List String -> OptionElement question option -> List FormValue -> List FormValue
getOptionValues path option values =
    case option of
        DetailedOptionElement descriptor items ->
            List.foldl (getFieldValue (path ++ [ descriptor.name ])) values items

        _ ->
            values


getItemValues : List String -> Int -> ItemElement question option -> List FormValue -> List FormValue
getItemValues path index item values =
    List.foldl (getFieldValue (path ++ [ fromInt index ])) values item


pathToKey : List String -> String -> String
pathToKey path current =
    String.join "." (path ++ [ current ])


applyFieldValue : List FormValue -> String -> Maybe ReplyValue -> List FormValue
applyFieldValue values key replyValue =
    case replyValue of
        Just value ->
            values ++ [ { path = key, value = value } ]

        Nothing ->
            values ++ [ { path = key, value = EmptyReply } ]
