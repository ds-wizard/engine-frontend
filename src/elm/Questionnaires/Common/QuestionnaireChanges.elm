module Questionnaires.Common.QuestionnaireChanges exposing
    ( QuestionnaireChanges
    , empty
    , foldMap
    , merge
    )

import Questionnaires.Common.AnswerChange exposing (AnswerChange)
import Questionnaires.Common.QuestionChange exposing (QuestionChange)


type alias QuestionnaireChanges =
    { questions : List QuestionChange
    , answers : List AnswerChange
    }


merge : QuestionnaireChanges -> QuestionnaireChanges -> QuestionnaireChanges
merge a b =
    { questions = a.questions ++ b.questions
    , answers = a.answers ++ b.answers
    }


foldMap : (a -> QuestionnaireChanges) -> List a -> QuestionnaireChanges
foldMap f list =
    List.foldr merge empty (List.map f list)


empty : QuestionnaireChanges
empty =
    { questions = [], answers = [] }
