module Common.Components.Questionnaire.Path exposing
    ( chapterUuid
    , isCollapsed
    , toFieldId
    , toString
    )

{-| Paths into a questionnaire: `chapter.question`, `chapter.question.answer.followUp`,
`chapter.list.item.question`, and so on — the uuids from the chapter down, joined
with dots. They are the keys of the replies, the labels and the comment counts,
and of the collapsed set, so every part of the questionnaire speaks them.
-}

import Set exposing (Set)


toString : List String -> String
toString =
    String.join "."


{-| An identifier usable as an HTML `id` or `name`. A path starts with a uuid and
contains dots, so it is prefixed and everything that is not safe in an
identifier becomes an underscore.
-}
toFieldId : String -> String
toFieldId path =
    let
        toSafeChar char =
            if Char.isAlphaNum char || char == '-' || char == '_' then
                char

            else
                '_'
    in
    "value_" ++ String.map toSafeChar path


isCollapsed : String -> { a | collapsedPaths : Set String } -> Bool
isCollapsed path model =
    Set.member path model.collapsedPaths


{-| The chapter a path is in — its first segment.
-}
chapterUuid : String -> String
chapterUuid path =
    String.split "." path
        |> List.head
        |> Maybe.withDefault ""
