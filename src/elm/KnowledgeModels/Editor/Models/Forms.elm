module KnowledgeModels.Editor.Models.Forms exposing (..)

{-|


# Forms

@docs KnowledgeModelForm, ChapterForm, QuestionForm, AnswerForm, ReferenceForm, ExpertForm, initForm


# KnowledgeModelForm helpers

@docs knowledgeModelFormValidation, knowledgeModelFormInitials, updateKnowledgeModelWithForm


# ChapterForm helpers

@docs initChapterForm, chapterFormValidation, chapterFormInitials, updateChapterWithForm


# QuestionForm helpers

@docs initQuestionForm, questionFormValidation, questionFormInitials, updateQuestionWithForm


# AnswerForm helpers

@docs initAnswerForm, answerFormValidation, answerFormInitials, updateAnswerWithForm


# ReferenceForm helpers

@docs initReferenceForm, referenceFormValidation, referenceFormInitials, updateReferenceWithForm


# ExpertForm helpers

@docs initExpertForm, expertFormValidation, expertFormInitials, updateExpertWithForm

-}

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)


{-| -}
type alias KnowledgeModelForm =
    { name : String }


{-| -}
type alias ChapterForm =
    { title : String
    , text : String
    }


{-| -}
type alias QuestionForm =
    { title : String
    , shortUuid : Maybe String
    , text : String
    }


{-| -}
type alias AnswerForm =
    { label : String
    , advice : Maybe String
    }


{-| -}
type alias ReferenceForm =
    { chapter : String }


{-| -}
type alias ExpertForm =
    { name : String
    , email : String
    }


{-| -}
initForm : Validation CustomFormError a -> List ( String, Field.Field ) -> Form CustomFormError a
initForm validation initials =
    Form.initial initials validation


{-| -}
knowledgeModelFormValidation : Validation CustomFormError KnowledgeModelForm
knowledgeModelFormValidation =
    Validate.map KnowledgeModelForm
        (Validate.field "name" Validate.string)


{-| -}
knowledgeModelFormInitials : KnowledgeModel -> List ( String, Field.Field )
knowledgeModelFormInitials knowledgeModel =
    [ ( "name", Field.string knowledgeModel.name ) ]


{-| -}
updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | name = knowledgeModelForm.name }


{-| -}
initChapterForm : Chapter -> Form CustomFormError ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


{-| -}
chapterFormValidation : Validation CustomFormError ChapterForm
chapterFormValidation =
    Validate.map2 ChapterForm
        (Validate.field "title" Validate.string)
        (Validate.field "text" Validate.string)


{-| -}
chapterFormInitials : Chapter -> List ( String, Field.Field )
chapterFormInitials chapter =
    [ ( "title", Field.string chapter.title )
    , ( "text", Field.string chapter.text )
    ]


{-| -}
updateChapterWithForm : Chapter -> ChapterForm -> Chapter
updateChapterWithForm chapter chapterForm =
    { chapter | title = chapterForm.title, text = chapterForm.text }


{-| -}
initQuestionForm : Question -> Form CustomFormError QuestionForm
initQuestionForm =
    questionFormInitials >> initForm questionFormValidation


{-| -}
questionFormValidation : Validation CustomFormError QuestionForm
questionFormValidation =
    Validate.map3 QuestionForm
        (Validate.field "title" Validate.string)
        (Validate.field "shortUuid" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
        (Validate.field "text" Validate.string)


{-| -}
questionFormInitials : Question -> List ( String, Field.Field )
questionFormInitials question =
    [ ( "title", Field.string question.title )
    , ( "shortUuid", Field.string (question.shortUuid |> Maybe.withDefault "") )
    , ( "text", Field.string question.text )
    ]


{-| -}
updateQuestionWithForm : Question -> QuestionForm -> Question
updateQuestionWithForm question questionForm =
    { question | title = questionForm.title, text = questionForm.text, shortUuid = questionForm.shortUuid }


{-| -}
initAnswerForm : Answer -> Form CustomFormError AnswerForm
initAnswerForm =
    answerFormInitials >> initForm answerFormValidation


{-| -}
answerFormValidation : Validation CustomFormError AnswerForm
answerFormValidation =
    Validate.map2 AnswerForm
        (Validate.field "label" Validate.string)
        (Validate.field "advice" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


{-| -}
answerFormInitials : Answer -> List ( String, Field.Field )
answerFormInitials answer =
    [ ( "label", Field.string answer.label )
    , ( "advice", Field.string (answer.advice |> Maybe.withDefault "") )
    ]


{-| -}
updateAnswerWithForm : Answer -> AnswerForm -> Answer
updateAnswerWithForm answer answerForm =
    { answer | label = answerForm.label, advice = answerForm.advice }


{-| -}
initReferenceForm : Reference -> Form CustomFormError ReferenceForm
initReferenceForm =
    referenceFormInitials >> initForm referenceFormValidation


{-| -}
referenceFormValidation : Validation CustomFormError ReferenceForm
referenceFormValidation =
    Validate.map ReferenceForm
        (Validate.field "chapter" Validate.string)


{-| -}
referenceFormInitials : Reference -> List ( String, Field.Field )
referenceFormInitials reference =
    [ ( "chapter", Field.string reference.chapter ) ]


{-| -}
updateReferenceWithForm : Reference -> ReferenceForm -> Reference
updateReferenceWithForm reference referenceForm =
    { reference | chapter = referenceForm.chapter }


{-| -}
initExpertForm : Expert -> Form CustomFormError ExpertForm
initExpertForm =
    expertFormInitials >> initForm expertFormValidation


{-| -}
expertFormValidation : Validation CustomFormError ExpertForm
expertFormValidation =
    Validate.map2 ExpertForm
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)


{-| -}
expertFormInitials : Expert -> List ( String, Field.Field )
expertFormInitials expert =
    [ ( "name", Field.string expert.name )
    , ( "email", Field.string expert.email )
    ]


{-| -}
updateExpertWithForm : Expert -> ExpertForm -> Expert
updateExpertWithForm expert expertForm =
    { expert | name = expertForm.name, email = expertForm.email }
