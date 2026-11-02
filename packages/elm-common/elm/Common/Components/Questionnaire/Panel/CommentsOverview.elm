module Common.Components.Questionnaire.Panel.CommentsOverview exposing
    ( Msg(..)
    , view
    )

{-| Where the comments are: every question that has any, under its chapter, with
how many are still open — and, when resolved ones are shown, how many of those
there are too.

It is `Panel.Group` with counts at the end of each row, and choosing a question
opens its comments.

The counts are `Ui.Badge`s with descriptions, so "3" is heard as "3 unresolved
comments" rather than as a number by itself.

-}

import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.ProjectQuestionnaire exposing (QuestionCommentInfo)
import Common.Components.Questionnaire.Panel.Group as Group
import Gettext exposing (Locale, gettext, ngettext)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Lazy as Lazy
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Badge as Badge
import Ui.Field as Field
import Ui.Icon as Icon


type Msg
    = Open String
    | ViewResolved Bool


view : Locale -> Bool -> List QuestionCommentInfo -> Html Msg
view locale viewResolved comments =
    Lazy.lazy3 viewLazy locale viewResolved comments


viewLazy : Locale -> Bool -> List QuestionCommentInfo -> Html Msg
viewLazy locale viewResolved comments =
    let
        shown =
            List.filter
                (\comment ->
                    comment.unresolvedComments
                        > 0
                        || (viewResolved && comment.resolvedComments > 0)
                )
                comments

        anyResolved =
            List.any (\comment -> comment.resolvedComments > 0) comments

        counts comment =
            Maybe.values
                [ if comment.unresolvedComments > 0 then
                    Just
                        (Badge.toHtml
                            (Badge.neutral (String.fromInt comment.unresolvedComments)
                                |> Badge.withDescription (plural "%s unresolved comment" "%s unresolved comments" comment.unresolvedComments)
                            )
                        )

                  else
                    Nothing
                , if viewResolved && comment.resolvedComments > 0 then
                    Just
                        (Badge.toHtml
                            (Badge.success (String.fromInt comment.resolvedComments)
                                |> Badge.withDescription (plural "%s resolved comment" "%s resolved comments" comment.resolvedComments)
                            )
                        )

                  else
                    Nothing
                ]

        plural singular pluralForm count =
            String.format (ngettext ( singular, pluralForm ) count locale) [ String.fromInt count ]

        list =
            Html.map (\(Group.Selected path) -> Open path)
                (Group.view
                    { locale = locale
                    , icon = Icon.regular "fa-comment"
                    , empty =
                        { title = gettext "No comments" locale
                        , description = gettext "Comments left on a question are listed here, with the questions they are on." locale
                        }
                    , items =
                        List.map
                            (\comment ->
                                { chapter = comment.chapter
                                , row = { path = comment.path, title = Question.getTitle comment.question, end = counts comment }
                                }
                            )
                            shown
                    }
                )

        rows =
            list
    in
    if anyResolved then
        Html.div [ Attributes.class "c-questionnaire-panel-filter" ]
            [ Field.toHtml
                (Field.toggle
                    { id = "questionnaire-comments-overview-resolved"
                    , label = gettext "Show resolved" locale
                    , checked = viewResolved
                    , onCheck = ViewResolved
                    }
                )
            , rows
            ]

    else
        rows
