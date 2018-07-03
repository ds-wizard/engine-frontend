module FormEngine.Update exposing (updateForm)

import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))


updateForm : Msg msg -> Form a -> Form a
updateForm msg form =
    case msg of
        Input path value ->
            { elements = List.map (updateElement (updateElementValue value) path) form.elements }

        GroupItemAdd path ->
            { elements = List.map (updateElement updateGroupItemAdd path) form.elements }

        GroupItemRemove path index ->
            { elements = List.map (updateElement (updateGroupItemRemove index) path) form.elements }

        _ ->
            form


updateElement : (FormElement a -> FormElement a) -> List String -> FormElement a -> FormElement a
updateElement updateFunction path element =
    case path of
        head :: [] ->
            if (getDescriptor element).name /= head then
                element
            else
                updateFunction element

        head :: tail ->
            if (getDescriptor element).name /= head then
                element
            else
                case element of
                    ChoiceFormElement descriptor options state ->
                        ChoiceFormElement descriptor (List.map (updateOption updateFunction tail) options) state

                    GroupFormElement descriptor items elementItems state ->
                        GroupFormElement descriptor items (List.indexedMap (updateItem updateFunction tail) elementItems) state

                    _ ->
                        element

        _ ->
            element


updateOption : (FormElement a -> FormElement a) -> List String -> OptionElement a -> OptionElement a
updateOption updateFunction path option =
    case path of
        head :: tail ->
            if (getOptionDescriptor option).name /= head then
                option
            else
                case option of
                    SimpleOptionElement _ ->
                        option

                    DetailedOptionElement descriptor items ->
                        DetailedOptionElement descriptor (List.map (updateElement updateFunction tail) items)

        _ ->
            option


updateItem : (FormElement a -> FormElement a) -> List String -> Int -> ItemElement a -> ItemElement a
updateItem updateFunction path index item =
    case path of
        head :: tail ->
            if toString index /= head then
                item
            else
                List.map (updateElement updateFunction tail) item

        _ ->
            item


updateElementValue : String -> FormElement a -> FormElement a
updateElementValue value element =
    case element of
        StringFormElement descriptor state ->
            StringFormElement descriptor { state | value = Just value }

        NumberFormElement descriptor state ->
            NumberFormElement descriptor { state | value = Just <| Result.withDefault 0 (String.toInt value) }

        TextFormElement descriptor state ->
            TextFormElement descriptor { state | value = Just value }

        ChoiceFormElement descriptor options state ->
            ChoiceFormElement descriptor options { state | value = Just value }

        _ ->
            element


updateGroupItemAdd : FormElement a -> FormElement a
updateGroupItemAdd element =
    case element of
        GroupFormElement descriptor items elementItems state ->
            let
                newElementItems =
                    elementItems ++ [ createItemElement items ]
            in
            GroupFormElement descriptor items newElementItems { state | value = Just <| List.length newElementItems }

        _ ->
            element


updateGroupItemRemove : Int -> FormElement a -> FormElement a
updateGroupItemRemove index element =
    case element of
        GroupFormElement descriptor items elementItems state ->
            let
                newElementItems =
                    removeFromList index elementItems
            in
            GroupFormElement descriptor items newElementItems { state | value = Just <| List.length newElementItems }

        _ ->
            element


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs
