module Common.Pages.KMEditor.Editor.Widgets exposing
    ( Config
    , annotations
    , checkboxes
    , children
    , color
    , fileSize
    , markdown
    , select
    , selectWithGroups
    , selectWithWarning
    , string
    , textarea
    , toggle
    )

{-| What the entity forms are built from: one vocabulary, so a form reads as a
list of fields rather than a list of `Ui.Field` calls with their own wrapping.

Most of it is `Ui.Field` renamed. Two are not:

  - **`children`** — an entity's ordered list of children, each a place to go,
    with Move up / Move down, a drag handle and a way to add another
    (`Ui.ReorderableList`).
  - **`annotations`** — the key/value pairs any entity can carry, with the
    caret landing in the key of the row just added.

-}

import Common.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.ColorPalette as ColorPalette
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Field as Field
import Ui.ListField as ListField
import Ui.ReorderableList as ReorderableList


type alias Config a =
    { a | locale : Gettext.Locale }



-- FIELDS


string : { id : String, label : String, value : String, onInput : String -> msg } -> Html msg
string =
    Field.toHtml << Field.input


textarea : { id : String, label : String, value : String, onInput : String -> msg } -> Html msg
textarea =
    Field.toHtml << Field.textarea


markdown : { id : String, label : String, value : String, onInput : String -> msg } -> Html msg
markdown config =
    Field.toHtml
        (Field.markdown
            { id = config.id, label = config.label, value = config.value, onChange = config.onInput }
        )


select :
    { id : String, label : String, value : String, options : List ( String, String ), onInput : String -> msg }
    -> Html msg
select =
    Field.toHtml << Field.select


{-| A choice that throws something away — a question's type, where changing it
takes its answers, its item questions or its choices with it. The warning is
only there while there is something to lose.
-}
selectWithWarning :
    Maybe String
    -> { id : String, label : String, value : String, options : List ( String, String ), onInput : String -> msg }
    -> Html msg
selectWithWarning warning config =
    Field.toHtml (Field.withWarning warning (Field.select config))


{-| A color, with a palette beside it.
-}
color : { id : String, label : String, value : String, onInput : String -> msg } -> Html msg
color config =
    Field.toHtml (Field.withSwatches ColorPalette.all (Field.color config))


{-| A number of bytes, with what that is in words beside it and the ceiling the
tenant imposes said underneath — the same shape the rest of the app uses for a
size (`Common.Components.Form.Field.fileSize`).
-}
fileSize :
    Config a
    -> { id : String, label : String, value : String, maximum : Int, onInput : String -> msg }
    -> Html msg
fileSize config cfg =
    let
        readable =
            case String.toInt cfg.value of
                Just bytes ->
                    if bytes > 0 then
                        Field.withSuffix ("≈ " ++ ByteUnits.toReadable bytes)

                    else
                        identity

                Nothing ->
                    identity
    in
    Field.toHtml
        (Field.input { id = cfg.id, label = cfg.label, value = cfg.value, onInput = cfg.onInput }
            |> Field.withInputMode "numeric"
            |> readable
            |> Field.withHint
                (String.replace "%s"
                    (ByteUnits.toReadable cfg.maximum)
                    (gettext "Uploaded files cannot be larger than %s, but you can set a smaller limit." config.locale)
                )
        )


{-| A select whose options are grouped — a list question picked out of the
knowledge model, under the chapter it is in.
-}
selectWithGroups :
    { id : String
    , label : String
    , value : String
    , loose : List ( String, String )
    , groups : List ( String, List ( String, String ) )
    , onInput : String -> msg
    }
    -> Field.Field msg
selectWithGroups =
    Field.selectWithGroups


{-| One thing on or off, as a switch: it takes effect as it is pressed rather
than when something else is saved.
-}
toggle : { id : String, label : String, checked : Bool, onCheck : Bool -> msg } -> Html msg
toggle =
    Field.toHtml << Field.toggle


{-| A set chosen from a list — a question's tags, each in its own color.
-}
checkboxes :
    { id : String
    , label : String
    , values : List String
    , options : List { value : String, label : String, color : Maybe String }
    , onToggle : String -> msg
    }
    -> Html msg
checkboxes config =
    let
        choice option =
            case option.color of
                Just value ->
                    Field.withChoiceColor value (Field.choice { value = option.value, label = option.label })

                Nothing ->
                    Field.choice { value = option.value, label = option.label }
    in
    Field.toHtml
        (Field.checkboxGroup
            { id = config.id
            , label = config.label
            , values = config.values
            , options = List.map choice config.options
            , onToggle = config.onToggle
            }
        )



-- AN ENTITY'S CHILDREN


{-| The ordered list of things inside an entity: a chapter's questions, a
question's answers, a resource collection's pages.

Each row is a link to that child — the tree and this are the two ways to reach
it — and the order is the knowledge model's, so moving a row is an edit.

-}
children :
    Config a
    ->
        { id : String
        , label : String
        , items : List { uuid : String, name : String, link : Html msg }
        , onReorder : List String -> msg
        , addLabel : String
        , onAdd : msg
        , announcement : Maybe String
        , drag : Maybe ReorderableList.Dragging
        , onDrag : Maybe ReorderableList.Dragging -> msg
        }
    -> Html msg
children config cfg =
    let
        uuids =
            List.map .uuid cfg.items

        move from to =
            cfg.onReorder (moveInList from to uuids)

        list =
            ReorderableList.toHtmlWithWords (words config)
                { id = cfg.id
                , label = cfg.label
                , items =
                    List.map
                        (\child -> ReorderableList.item { key = child.uuid, label = child.name, content = [ child.link ] })
                        cfg.items
                , onMove = move
                , announcement = cfg.announcement
                , add = Just { label = cfg.addLabel, onAdd = cfg.onAdd }
                , drag = Just { state = cfg.drag, onChange = cfg.onDrag }
                }
    in
    Field.toHtml (Field.readOnlyHtml { id = cfg.id ++ "-label", label = cfg.label, value = [ list ] })


words : Config a -> ReorderableList.Words
words config =
    { moveUp = \label -> String.replace "%s" label (gettext "Move %s up" config.locale)
    , moveDown = \label -> String.replace "%s" label (gettext "Move %s down" config.locale)
    , atStart = gettext "It is already first" config.locale
    , atEnd = gettext "It is already last" config.locale
    }


{-| The list with the item at `from` put at `to`, which is what a move up or
down means to an ordered list of uuids.
-}
moveInList : Int -> Int -> List String -> List String
moveInList from to list =
    case List.drop from (List.take (from + 1) list) of
        [ moved ] ->
            let
                without =
                    List.take from list ++ List.drop (from + 1) list
            in
            List.take to without ++ [ moved ] ++ List.drop to without

        _ ->
            list



-- ANNOTATIONS


{-| The key/value pairs any entity can carry. A row is a key and a value; adding
one puts the caret in its key, which `Ui.ListField` arranges.

The value is a textarea: an annotation carries whatever a document template
wants of it, which is as often a sentence or a fragment of markup as it is a
word, and a single line hides all but the start of it.

-}
annotations :
    Config a
    ->
        { id : String
        , value : List Annotation
        , onChange : List Annotation -> msg
        }
    -> Html msg
annotations config cfg =
    let
        row index annotation =
            ListField.row
                { label =
                    if String.isEmpty annotation.key then
                        String.replace "%s" (String.fromInt (index + 1)) (gettext "Annotation %s" config.locale)

                    else
                        annotation.key
                , content =
                    [ Field.toHtml
                        (Field.input
                            { id = cfg.id ++ "-key-" ++ String.fromInt index
                            , label = gettext "Key" config.locale
                            , value = annotation.key
                            , onInput = \value -> cfg.onChange (setAt index { annotation | key = value } cfg.value)
                            }
                        )
                    , Field.toHtml
                        (Field.textarea
                            { id = cfg.id ++ "-value-" ++ String.fromInt index
                            , label = gettext "Value" config.locale
                            , value = annotation.value
                            , onInput = \value -> cfg.onChange (setAt index { annotation | value = value } cfg.value)
                            }
                        )
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (cfg.id ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = cfg.id
        , label = gettext "Annotations" config.locale
        , rows = List.indexedMap row cfg.value
        , addLabel = gettext "Add annotation" config.locale
        , onAdd = cfg.onChange (cfg.value ++ [ { key = "", value = "" } ])
        , onRemove = Just (\index -> cfg.onChange (removeAt index cfg.value))
        , hint = Nothing
        , error = Nothing
        }


setAt : Int -> b -> List b -> List b
setAt index value list =
    List.indexedMap
        (\i item ->
            if i == index then
                value

            else
                item
        )
        list


removeAt : Int -> List b -> List b
removeAt index list =
    List.take index list ++ List.drop (index + 1) list
