module Wizard.Projects.Common.ChoiceChange exposing
    ( ChoiceAddData
    , ChoiceChange(..)
    , ChoiceChangeData
    , getChoiceUuid
    )

import Shared.Data.KnowledgeModel.Choice exposing (Choice)


type ChoiceChange
    = ChoiceAdd ChoiceAddData
    | ChoiceChange ChoiceChangeData


type alias ChoiceAddData =
    { choice : Choice }


type alias ChoiceChangeData =
    { choice : Choice
    , originalChoice : Choice
    }


getChoiceUuid : ChoiceChange -> String
getChoiceUuid change =
    case change of
        ChoiceAdd data ->
            data.choice.uuid

        ChoiceChange data ->
            data.choice.uuid
