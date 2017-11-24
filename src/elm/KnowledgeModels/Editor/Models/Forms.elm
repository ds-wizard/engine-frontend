module KnowledgeModels.Editor.Models.Forms exposing (..)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)


type alias KnowledgeModelForm =
    { name : String }


type alias ChapterForm =
    { title : String
    , text : String
    }


type alias QuestionForm =
    { title : String
    , text : String
    }


type alias AnswerForm =
    { label : String
    , advice : Maybe String
    }


type alias ReferenceForm =
    { chapter : String }


type alias ExpertForm =
    { name : String
    , email : String
    }


initForm : Validation () a -> List ( String, Field.Field ) -> Form () a
initForm validation initials =
    Form.initial initials validation


knowledgeModelFormValidation : Validation () KnowledgeModelForm
knowledgeModelFormValidation =
    Validate.map KnowledgeModelForm
        (Validate.field "name" Validate.string)


knowledgeModelFormInitials : KnowledgeModel -> List ( String, Field.Field )
knowledgeModelFormInitials knowledgeModel =
    [ ( "name", Field.string knowledgeModel.name ) ]


updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | name = knowledgeModelForm.name }


initChapterForm : Chapter -> Form () ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


chapterFormValidation : Validation () ChapterForm
chapterFormValidation =
    Validate.map2 ChapterForm
        (Validate.field "title" Validate.string)
        (Validate.field "text" Validate.string)


chapterFormInitials : Chapter -> List ( String, Field.Field )
chapterFormInitials chapter =
    [ ( "title", Field.string chapter.title )
    , ( "text", Field.string chapter.text )
    ]


updateChapterWithForm : Chapter -> ChapterForm -> Chapter
updateChapterWithForm chapter chapterForm =
    { chapter | title = chapterForm.title, text = chapterForm.text }


initQuestionForm : Question -> Form () QuestionForm
initQuestionForm =
    questionFormInitials >> initForm questionFormValidation


questionFormValidation : Validation () QuestionForm
questionFormValidation =
    Validate.map2 QuestionForm
        (Validate.field "title" Validate.string)
        (Validate.field "text" Validate.string)


questionFormInitials : Question -> List ( String, Field.Field )
questionFormInitials question =
    [ ( "title", Field.string question.title )
    , ( "text", Field.string question.text )
    ]


updateQuestionWithForm : Question -> QuestionForm -> Question
updateQuestionWithForm question questionForm =
    { question | title = questionForm.title, text = questionForm.text }


answerFormValidation : Validation () AnswerForm
answerFormValidation =
    Validate.map2 AnswerForm
        (Validate.field "label" Validate.string)
        (Validate.field "advice" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


answerFormInitials : Answer -> List ( String, Field.Field )
answerFormInitials answer =
    let
        advice =
            case answer.advice of
                Just a ->
                    a

                Nothing ->
                    ""
    in
    [ ( "label", Field.string answer.label )
    , ( "advice", Field.string advice )
    ]


referenceFormValidation : Validation () ReferenceForm
referenceFormValidation =
    Validate.map ReferenceForm
        (Validate.field "chapter" Validate.string)


referenceFormInitials : Reference -> List ( String, Field.Field )
referenceFormInitials reference =
    [ ( "chapter", Field.string reference.chapter ) ]


expertFormValidation : Validation () ExpertForm
expertFormValidation =
    Validate.map2 ExpertForm
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)


expertFormInitials : Expert -> List ( String, Field.Field )
expertFormInitials expert =
    [ ( "name", Field.string expert.name )
    , ( "email", Field.string expert.email )
    ]
