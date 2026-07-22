module Wizard.Api.Models.KnowledgeModel.Chapter exposing
    ( Chapter
    , addQuestionUuid
    , decoder
    , equalContent
    , localize
    , removeQuestionUuid
    )

import Gettext exposing (Locale, gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Chapter =
    { uuid : String
    , title : String
    , text : Maybe String
    , questionUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder Chapter
decoder =
    D.succeed Chapter
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "questionUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


addQuestionUuid : String -> Chapter -> Chapter
addQuestionUuid questionUuid chapter =
    { chapter | questionUuids = chapter.questionUuids ++ [ questionUuid ] }


removeQuestionUuid : String -> Chapter -> Chapter
removeQuestionUuid questionUuid chapter =
    { chapter | questionUuids = List.filter ((/=) questionUuid) chapter.questionUuids }


equalContent : Chapter -> Chapter -> Bool
equalContent chapter1 chapter2 =
    (chapter1.title == chapter2.title)
        && (chapter1.text == chapter2.text)


localize : Locale -> Chapter -> Chapter
localize locale chapter =
    { chapter
        | title = gettext chapter.title locale
        , text = Maybe.map (\text -> gettext text locale) chapter.text
    }
