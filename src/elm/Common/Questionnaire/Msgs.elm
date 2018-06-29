module Common.Questionnaire.Msgs exposing (..)

import FormEngine.Msgs
import KMEditor.Common.Models.Entities exposing (Chapter)


type Msg
    = FormMsg FormEngine.Msgs.Msg
    | SetActiveChapter Chapter
