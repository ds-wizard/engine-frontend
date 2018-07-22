module KMEditor.Editor.Models.Forms exposing (..)

import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Error as Error exposing (ErrorValue(InvalidString))
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import Set


type alias KnowledgeModelForm =
    { name : String }


type alias ChapterForm =
    { title : String
    , text : String
    }


type alias QuestionForm =
    { title : String
    , type_ : String
    , text : String
    , itemName : String
    }


type alias AnswerForm =
    { label : String
    , advice : Maybe String
    }


type ReferenceFormType
    = ResourcePageReferenceFormType String
    | URLReferenceFormType String String
    | CrossReferenceFormType String String


type alias ReferenceForm =
    { reference : ReferenceFormType
    }


type alias ExpertForm =
    { name : String
    , email : String
    }



{- Common utils -}


initForm : Validation CustomFormError a -> List ( String, Field.Field ) -> Form CustomFormError a
initForm validation initials =
    Form.initial initials validation


formChanged : Form CustomFormError a -> Bool
formChanged form =
    Set.size (Form.getChangedFields form) > 0



{- Knowledge Model -}


initKnowledgeModelFrom : KnowledgeModel -> Form CustomFormError KnowledgeModelForm
initKnowledgeModelFrom =
    knowledgeModelFormInitials >> initForm knowledgeModelFormValidation


knowledgeModelFormValidation : Validation CustomFormError KnowledgeModelForm
knowledgeModelFormValidation =
    Validate.map KnowledgeModelForm
        (Validate.field "name" Validate.string)


knowledgeModelFormInitials : KnowledgeModel -> List ( String, Field.Field )
knowledgeModelFormInitials knowledgeModel =
    [ ( "name", Field.string knowledgeModel.name ) ]


updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | name = knowledgeModelForm.name }



{- Chapter -}


initChapterForm : Chapter -> Form CustomFormError ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


chapterFormValidation : Validation CustomFormError ChapterForm
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



{- Question -}


initQuestionForm : Question -> Form CustomFormError QuestionForm
initQuestionForm =
    questionFormInitials >> initForm questionFormValidation


questionFormValidation : Validation CustomFormError QuestionForm
questionFormValidation =
    Validate.map4 QuestionForm
        (Validate.field "title" Validate.string)
        (Validate.field "type_" Validate.string)
        (Validate.field "text" Validate.string)
        (Validate.field "itemName" (Validate.oneOf [ Validate.emptyString, Validate.string ]))


questionFormInitials : Question -> List ( String, Field.Field )
questionFormInitials question =
    [ ( "title", Field.string question.title )
    , ( "type_", Field.string question.type_ )
    , ( "text", Field.string question.text )
    , ( "itemName", Field.string (question.answerItemTemplate |> Maybe.map .title |> Maybe.withDefault "") )
    ]


updateQuestionWithForm : Question -> QuestionForm -> Question
updateQuestionWithForm question questionForm =
    let
        answerItemTemplate =
            if questionForm.type_ == "list" then
                Just
                    { title = questionForm.itemName
                    , questions = AnswerItemTemplateQuestions []
                    }
            else
                Nothing
    in
    { question
        | title = questionForm.title
        , text = questionForm.text
        , type_ = questionForm.type_
        , answerItemTemplate = answerItemTemplate
    }


questionTypeOptions : List ( String, String )
questionTypeOptions =
    [ ( "options", "Options" )
    , ( "list", "List of items" )
    , ( "string", "String" )
    , ( "number", "Number" )
    , ( "date", "Date" )
    , ( "text", "Long text" )
    ]



{- Answer -}


initAnswerForm : Answer -> Form CustomFormError AnswerForm
initAnswerForm =
    answerFormInitials >> initForm answerFormValidation


answerFormValidation : Validation CustomFormError AnswerForm
answerFormValidation =
    Validate.map2 AnswerForm
        (Validate.field "label" Validate.string)
        (Validate.field "advice" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


answerFormInitials : Answer -> List ( String, Field.Field )
answerFormInitials answer =
    [ ( "label", Field.string answer.label )
    , ( "advice", Field.string (answer.advice |> Maybe.withDefault "") )
    ]


updateAnswerWithForm : Answer -> AnswerForm -> Answer
updateAnswerWithForm answer answerForm =
    { answer | label = answerForm.label, advice = answerForm.advice }



{- Reference -}


initReferenceForm : Reference -> Form CustomFormError ReferenceForm
initReferenceForm =
    referenceFormInitials >> initForm referenceFormValidation


referenceFormValidation : Validation CustomFormError ReferenceForm
referenceFormValidation =
    Validate.succeed ReferenceForm
        |> Validate.andMap (Validate.field "referenceType" Validate.string |> Validate.andThen validateReference)


validateReference : String -> Validation CustomFormError ReferenceFormType
validateReference referenceType =
    case referenceType of
        "ResourcePageReference" ->
            Validate.succeed ResourcePageReferenceFormType
                |> Validate.andMap (Validate.field "shortUuid" Validate.string)

        "URLReference" ->
            Validate.succeed URLReferenceFormType
                |> Validate.andMap (Validate.field "url" Validate.string)
                |> Validate.andMap (Validate.field "anchor" Validate.string)

        "CrossReference" ->
            Validate.succeed CrossReferenceFormType
                |> Validate.andMap (Validate.field "targetUuid" Validate.string)
                |> Validate.andMap (Validate.field "description" Validate.string)

        _ ->
            Validate.fail <| Error.value InvalidString


referenceFormInitials : Reference -> List ( String, Field.Field )
referenceFormInitials reference =
    case reference of
        ResourcePageReference data ->
            [ ( "referenceType", Field.string "ResourcePageReference" )
            , ( "shortUuid", Field.string data.shortUuid )
            ]

        URLReference data ->
            [ ( "referenceType", Field.string "URLReference" )
            , ( "url", Field.string data.url )
            , ( "anchor", Field.string data.anchor )
            ]

        CrossReference data ->
            [ ( "referenceType", Field.string "CrossReference" )
            , ( "targetUuid", Field.string data.targetUuid )
            , ( "description", Field.string data.description )
            ]


updateReferenceWithForm : Reference -> ReferenceForm -> Reference
updateReferenceWithForm reference referenceForm =
    case referenceForm.reference of
        ResourcePageReferenceFormType shortUuid ->
            ResourcePageReference
                { uuid = getReferenceUuid reference
                , shortUuid = shortUuid
                }

        URLReferenceFormType url anchor ->
            URLReference
                { uuid = getReferenceUuid reference
                , url = url
                , anchor = anchor
                }

        CrossReferenceFormType targetUuid description ->
            CrossReference
                { uuid = getReferenceUuid reference
                , targetUuid = targetUuid
                , description = description
                }


referenceTypeOptions : List ( String, String )
referenceTypeOptions =
    [ ( "ResourcePageReference", "Resource Page" )
    , ( "URLReference", "URL" )
    , ( "CrossReference", "Cross Reference" )
    ]



{- Expert -}


initExpertForm : Expert -> Form CustomFormError ExpertForm
initExpertForm =
    expertFormInitials >> initForm expertFormValidation


expertFormValidation : Validation CustomFormError ExpertForm
expertFormValidation =
    Validate.map2 ExpertForm
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)


expertFormInitials : Expert -> List ( String, Field.Field )
expertFormInitials expert =
    [ ( "name", Field.string expert.name )
    , ( "email", Field.string expert.email )
    ]


updateExpertWithForm : Expert -> ExpertForm -> Expert
updateExpertWithForm expert expertForm =
    { expert | name = expertForm.name, email = expertForm.email }
