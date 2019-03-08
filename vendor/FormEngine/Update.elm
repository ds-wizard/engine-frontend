module FormEngine.Update exposing (updateForm)

import ActionResult exposing (ActionResult(..))
import Debounce
import FormEngine.Model exposing (..)
import FormEngine.Msgs exposing (Msg(..))
import String exposing (fromInt)


debounceConfig : Debounce.Config (Msg msg err)
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }


updateForm : Msg msg err -> Form question option -> (String -> String -> (Result err (List TypeHint) -> Msg msg err) -> Cmd (Msg msg err)) -> ( Form question option, Cmd (Msg msg err) )
updateForm msg form loadTypeHints =
    case msg of
        Input path value ->
            ( { form | elements = List.map (updateElement (updateElementValue value) path) form.elements }
            , Cmd.none
            )

        InputTypehint path questionUuid value ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig ( questionUuid, getStringReply value ) form.debounce
            in
            ( { form
                | elements = List.map (updateElement (updateElementValue value) path) form.elements
                , typeHints =
                    Just
                        { path = path
                        , hints = Loading
                        }
                , debounce = debounce
              }
            , cmd
            )

        DebounceMsg debounceMsg ->
            let
                load ( questionUuid, value ) =
                    loadTypeHints questionUuid value TypeHintsLoaded

                ( debounce, cmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast load) debounceMsg form.debounce
            in
            ( { form | debounce = debounce }
            , cmd
            )

        Clear path ->
            ( { form | elements = List.map (updateElement clearElementValue path) form.elements }
            , Cmd.none
            )

        GroupItemAdd path ->
            ( { form | elements = List.map (updateElement updateGroupItemAdd path) form.elements }
            , Cmd.none
            )

        GroupItemRemove path index ->
            ( { form | elements = List.map (updateElement (updateGroupItemRemove index) path) form.elements }
            , Cmd.none
            )

        ShowTypeHints path questionUuid value ->
            ( { form
                | typeHints =
                    Just
                        { path = path
                        , hints = Loading
                        }
              }
            , loadTypeHints questionUuid value TypeHintsLoaded
            )

        HideTypeHints ->
            ( { form | typeHints = Nothing }
            , Cmd.none
            )

        TypeHintsLoaded result ->
            let
                actionResult =
                    case result of
                        Ok typeHints ->
                            Success typeHints

                        Err _ ->
                            Error "Unable to get type hints"
            in
            ( setTypeHintsResult actionResult form, Cmd.none )

        _ ->
            ( form, Cmd.none )


updateElement : (FormElement question option -> FormElement question option) -> List String -> FormElement question option -> FormElement question option
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


updateOption : (FormElement question option -> FormElement question option) -> List String -> OptionElement question option -> OptionElement question option
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


updateItem : (FormElement question option -> FormElement question option) -> List String -> Int -> ItemElement question option -> ItemElement question option
updateItem updateFunction path index item =
    case path of
        head :: tail ->
            if fromInt index /= head then
                item

            else
                List.map (updateElement updateFunction tail) item

        _ ->
            item


updateElementValue : ReplyValue -> FormElement question option -> FormElement question option
updateElementValue value element =
    case element of
        StringFormElement descriptor state ->
            StringFormElement descriptor { state | value = Just value }

        NumberFormElement descriptor state ->
            NumberFormElement descriptor { state | value = Just value }

        TextFormElement descriptor state ->
            TextFormElement descriptor { state | value = Just value }

        TypeHintFormElement descriptor typeHintConfig state ->
            TypeHintFormElement descriptor typeHintConfig { state | value = Just value }

        ChoiceFormElement descriptor options state ->
            ChoiceFormElement descriptor options { state | value = Just value }

        GroupFormElement _ _ _ _ ->
            element


clearElementValue : FormElement question option -> FormElement question option
clearElementValue element =
    case element of
        StringFormElement descriptor state ->
            StringFormElement descriptor { state | value = Nothing }

        NumberFormElement descriptor state ->
            NumberFormElement descriptor { state | value = Nothing }

        TextFormElement descriptor state ->
            TextFormElement descriptor { state | value = Nothing }

        TypeHintFormElement descriptor typeHintConfig state ->
            TypeHintFormElement descriptor typeHintConfig { state | value = Nothing }

        ChoiceFormElement descriptor options state ->
            ChoiceFormElement descriptor options { state | value = Nothing }

        GroupFormElement _ _ _ _ ->
            element


updateGroupItemAdd : FormElement question option -> FormElement question option
updateGroupItemAdd element =
    case element of
        GroupFormElement descriptor items elementItems state ->
            let
                newElementItems =
                    elementItems ++ [ createItemElement items ]
            in
            GroupFormElement descriptor items newElementItems { state | value = Just <| ItemListReply <| List.length newElementItems }

        _ ->
            element


updateGroupItemRemove : Int -> FormElement question option -> FormElement question option
updateGroupItemRemove index element =
    case element of
        GroupFormElement descriptor items elementItems state ->
            let
                newElementItems =
                    removeFromList index elementItems
            in
            GroupFormElement descriptor items newElementItems { state | value = Just <| ItemListReply <| List.length newElementItems }

        _ ->
            element


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs
