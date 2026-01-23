module Wizard.Components.Questionnaire2.QuestionnaireUpdateReturnData exposing
    ( QuestionnaireUpdateReturnData
    , fromModel
    , fromModelCmd
    )

import Random exposing (Seed)
import Wizard.Api.Models.ProjectDetail.ProjectEvent exposing (ProjectEvent)
import Wizard.Data.AppState exposing (AppState)


type alias QuestionnaireUpdateReturnData m msg =
    { seed : Seed
    , model : m
    , cmd : Cmd msg
    , event : Maybe ProjectEvent
    }


fromModel : AppState -> m -> QuestionnaireUpdateReturnData m msg
fromModel appState model =
    { seed = appState.seed
    , model = model
    , cmd = Cmd.none
    , event = Nothing
    }


fromModelCmd : AppState -> m -> Cmd msg -> QuestionnaireUpdateReturnData m msg
fromModelCmd appState model cmd =
    { seed = appState.seed
    , model = model
    , cmd = cmd
    , event = Nothing
    }
