module Wizard.Projects.Common.QuestionnaireChanges exposing
    ( QuestionnaireChanges
    , empty
    , foldMap
    , merge
    )

import Wizard.Projects.Common.AnswerChange exposing (AnswerChange)
import Wizard.Projects.Common.ChoiceChange exposing (ChoiceChange)
import Wizard.Projects.Common.QuestionChange exposing (QuestionChange)


type alias QuestionnaireChanges =
    { questions : List QuestionChange
    , answers : List AnswerChange
    , choices : List ChoiceChange
    }


merge : QuestionnaireChanges -> QuestionnaireChanges -> QuestionnaireChanges
merge a b =
    { questions = a.questions ++ b.questions
    , answers = a.answers ++ b.answers
    , choices = a.choices ++ b.choices
    }


foldMap : (a -> QuestionnaireChanges) -> List a -> QuestionnaireChanges
foldMap f list =
    List.foldr merge empty (List.map f list)


empty : QuestionnaireChanges
empty =
    { questions = [], answers = [], choices = [] }
