module Common.Components.Questionnaire.Item exposing
    ( encodeUsages
    , toggleId
    , usagesMenuId
    , viewEmpty
    , viewFooter
    , viewHeader
    , viewItemsEnd
    )

{-| The nodes of a list question's items: each item's header and footer, the notice
standing in for an item with nothing to show, and the list's end with its "Add".

Like the questions, every function is the body of a lazy call with stored or
primitive arguments. Each node is a `listitem` of the questionnaire's list, one
level under the list question: an item's header is named "Item 2: its title", its
footer and its empty notice by that same heading — the footer is where an item
ends, often far below a header that has scrolled away — and the list's end by the
list question it adds to.

-}

import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Components.Questionnaire.ContentMsg exposing (Msg(..))
import Common.Components.Questionnaire.Flags as Flags exposing (Flags)
import Common.Components.Questionnaire.Nest as Nest
import Common.Components.Questionnaire.Nodes as Nodes exposing (ItemEmptyNodeData, ItemFooterNodeData, ItemHeaderNodeData, ItemsEndNodeData)
import Common.Components.Questionnaire.Path as Path
import Common.Components.Questionnaire.Question as Question
import Common.Components.Questionnaire.ReplyString as ReplyString
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, div, em, span, strong, text)
import Html.Attributes exposing (attribute, class, classList, id, type_)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Menu as Menu
import Ui.Text as Text
import Ui.Tooltip as Tooltip


{-| `itemTitle` is what the item's first question says, or empty; `replyString` is
the list's reply, for the item's place in it; `usages` the item select questions
the item is selected in (`encodeUsages`), or empty, and `usagesMenu` their menu.
-}
viewHeader : Gettext.Locale -> ItemHeaderNodeData -> Flags -> String -> String -> String -> Menu.Model -> Html Msg
viewHeader locale data flags itemTitle replyString usages usagesMenu =
    let
        itemName =
            String.format (gettext "Item %s" locale) [ String.fromInt (data.itemIndex + 1) ]

        titleId =
            Nest.titleId data.itemPath

        -- Collapsed, the header shows the item's title, as the wizard did; open,
        -- the questions below say what it is, and the heading is only read out.
        heading =
            if data.isCollapsed then
                if String.isEmpty itemTitle then
                    [ em [] [ text itemName ] ]

                else
                    [ Text.visuallyHiddenInline (itemName ++ ": "), strong [] [ text itemTitle ] ]

            else if String.isEmpty itemTitle then
                [ Text.visuallyHiddenInline itemName ]

            else
                [ Text.visuallyHiddenInline (itemName ++ ": " ++ itemTitle) ]

        ( toggleIcon, toggleMsg ) =
            if data.isCollapsed then
                ( Icon.solid "fa-chevron-right", ExpandPaths [ data.itemPath ] )

            else
                ( Icon.solid "fa-chevron-down", CollapsePaths [ data.itemPath ] )

        -- Where the item is selected: the number of
        -- item select questions, opening a menu that goes to each of them.
        usagesButton =
            case decodeUsages usages of
                [] ->
                    text ""

                usageList ->
                    let
                        count =
                            List.length usageList
                    in
                    Menu.toHtml
                        { id = usagesMenuId data.itemUuid
                        , trigger =
                            Menu.custom
                                { label = String.format (ngettext ( "Used in %s item select question", "Used in %s item select questions" ) count locale) [ String.fromInt count ]
                                , class = "ui-button ui-button--quiet ui-button--sm"
                                , content = [ Icon.toHtml (Icon.solid "fa-arrow-right-arrow-left"), text (String.fromInt count) ]
                                }
                                |> Menu.withTriggerTooltipAt Tooltip.End
                                |> Menu.withTriggerTestId "item-usages"
                        , alignment = Menu.End
                        , items =
                            Menu.header (gettext "Used in item select questions" locale)
                                :: List.map (\( path, label ) -> Menu.item label (ScrollToPath path)) usageList
                        , toMsg = ItemUsagesMenuMsg data.itemUuid
                        }
                        usagesMenu

        buttons =
            if Flags.isReadOnly flags then
                []

            else
                let
                    itemUuids =
                        Maybe.unwrap [] (ReplyValue.getItemUuids << .value) (ReplyString.toReply replyString)

                    itemEvent =
                        { path = data.parentQuestionPath, itemUuids = itemUuids, itemUuid = data.itemUuid }
                in
                [ if data.itemIndex == 0 then
                    text ""

                  else
                    Button.toHtml
                        (Button.iconOnly (Icon.solid "fa-arrow-up") (gettext "Move up" locale) (MoveItemUp itemEvent)
                            |> Button.quiet
                            |> Button.small
                            |> Button.withTooltipAt Tooltip.End
                            |> Button.withTestId "item-move-up"
                        )
                , if data.itemIndex == List.length itemUuids - 1 then
                    text ""

                  else
                    Button.toHtml
                        (Button.iconOnly (Icon.solid "fa-arrow-down") (gettext "Move down" locale) (MoveItemDown itemEvent)
                            |> Button.quiet
                            |> Button.small
                            |> Button.withTooltipAt Tooltip.End
                            |> Button.withTestId "item-move-down"
                        )
                , Button.toHtml
                    (Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" locale) (RemoveItemOpen itemEvent)
                        |> Button.quiet
                        |> Button.small
                        |> Button.withTooltipAt Tooltip.End
                        |> Button.withTestId "item-delete"
                    )
                ]
    in
    Nest.listItem { level = Nodes.depth data.nestingType + 1, labelledBy = titleId, attributes = [] }
        data.nestingType
        [ div
            [ class "c-questionnaire-item"
            , classList [ ( "is-collapsed", data.isCollapsed ) ]
            , attribute "data-path" data.itemPath
            ]
            [ Html.button
                [ type_ "button"
                , id (toggleId data.itemPath)
                , class "c-questionnaire-item__toggle"
                , attribute "aria-expanded" (boolString (not data.isCollapsed))
                , onClick toggleMsg
                ]
                [ Icon.toHtml toggleIcon
                , span [ class "c-questionnaire-item__title", id titleId ] heading
                ]
            , div [ class "c-questionnaire-item__actions" ] (usagesButton :: buttons)
            ]
        ]


{-| The id of the button that folds and unfolds the item at `itemPath`: what takes
the focus when the item is added. Not its title — open and untitled, the title
holds only visually hidden text, and its focus ring is a small square beside the
chevron.
-}
toggleId : String -> String
toggleId itemPath =
    Path.toFieldId itemPath ++ "-toggle"


usagesMenuId : String -> String
usagesMenuId itemUuid =
    "item-usages-" ++ itemUuid


{-| The item select questions an item is selected in, their paths and titles, as
the string `viewHeader` compares.
-}
encodeUsages : List ( String, String ) -> String
encodeUsages usages =
    E.encode 0 (E.list (\( path, label ) -> E.list E.string [ path, label ]) usages)


decodeUsages : String -> List ( String, String )
decodeUsages usages =
    D.decodeString (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.string))) usages
        |> Result.withDefault []


viewEmpty : Gettext.Locale -> ItemEmptyNodeData -> Html Msg
viewEmpty locale data =
    let
        message =
            if data.hiddenByViewOptions then
                gettext "All questions in this item are hidden by the current view settings." locale

            else
                gettext "This item contains no questions." locale
    in
    Nest.listItem { level = Nodes.depth data.nestingType, labelledBy = Nest.titleId data.itemPath, attributes = [] }
        data.nestingType
        [ div [ class "c-questionnaire-item-empty" ] [ Alert.toHtml (Alert.info message) ] ]


viewFooter : Gettext.Locale -> ItemFooterNodeData -> Html Msg
viewFooter locale data =
    Nest.listItem { level = Nodes.depth data.nestingType + 1, labelledBy = Nest.titleId data.itemPath, attributes = [] }
        data.nestingType
        [ div [ class "c-questionnaire-item-footer" ]
            [ div [ class "c-questionnaire-item-footer__bar" ]
                [ Button.toHtml
                    (Button.ghost (gettext "Collapse" locale) (CollapsePaths [ data.itemPath ])
                        |> Button.withIcon (Icon.solid "fa-chevron-up")
                        |> Button.quiet
                        |> Button.small
                    )
                ]
            ]
        ]


viewItemsEnd : Gettext.Locale -> ItemsEndNodeData -> Flags -> String -> Html Msg
viewItemsEnd locale data flags replyString =
    let
        mbReply =
            ReplyString.toReply replyString
    in
    Nest.listItem { level = Nodes.depth data.nestingType + 1, labelledBy = Nest.titleId data.questionPath, attributes = [] }
        data.nestingType
        -- Expand and collapse all on one line, Add on the next: the one is about
        -- the items there are, the other makes another.
        [ div [ class "c-questionnaire-items-end" ]
            [ Question.viewItemsCollapse { locale = locale, mbReply = mbReply, questionPath = data.questionPath }
            , if Flags.isReadOnly flags then
                text ""

              else
                Button.toHtml
                    (Button.secondary (gettext "Add" locale) (AddItem data.questionPath (Maybe.unwrap [] (ReplyValue.getItemUuids << .value) mbReply))
                        |> Button.withIcon (Icon.solid "fa-plus")
                        |> Button.withTestId "item-add"
                    )
            ]
        ]


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"
