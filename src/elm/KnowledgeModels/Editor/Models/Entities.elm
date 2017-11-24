module KnowledgeModels.Editor.Models.Entities exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , chapters : List Chapter
    }


type alias Chapter =
    { uuid : String
    , title : String
    , text : String
    , questions : List Question
    }


type alias Question =
    { uuid : String
    , type_ : String
    , title : String
    , text : String
    , answers : List Answer
    , references : List Reference
    , experts : List Expert
    }


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , following : Followings
    }


type Followings
    = Followings (List Question)


type alias Reference =
    { uuid : String
    , chapter : String
    }


type alias Expert =
    { uuid : String
    , name : String
    , email : String
    }


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "chapters" (Decode.list chapterDecoder)


chapterDecoder : Decoder Chapter
chapterDecoder =
    decode Chapter
        |> required "uuid" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> required "questions" (Decode.list questionDecoder)


questionDecoder : Decoder Question
questionDecoder =
    decode Question
        |> required "uuid" Decode.string
        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "text" Decode.string
        |> required "answers" (Decode.lazy (\_ -> Decode.list answerDecoder))
        |> required "references" (Decode.list referenceDecoder)
        |> required "experts" (Decode.list expertDecoder)


answerDecoder : Decoder Answer
answerDecoder =
    decode Answer
        |> required "uuid" Decode.string
        |> required "label" Decode.string
        |> required "advice" (Decode.nullable Decode.string)
        |> required "following" followingsDecoder


followingsDecoder : Decoder Followings
followingsDecoder =
    Decode.map Followings (Decode.lazy (\_ -> Decode.list questionDecoder))


referenceDecoder : Decoder Reference
referenceDecoder =
    decode Reference
        |> required "uuid" Decode.string
        |> required "chapter" Decode.string


expertDecoder : Decoder Expert
expertDecoder =
    decode Expert
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "email" Decode.string
