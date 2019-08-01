module Questionnaires.Msgs exposing (Msg(..))

import Questionnaires.Create.Msgs
import Questionnaires.CreateMigration.Msgs
import Questionnaires.Detail.Msgs
import Questionnaires.Edit.Msgs
import Questionnaires.Index.Msgs
import Questionnaires.Migration.Msgs


type Msg
    = CreateMsg Questionnaires.Create.Msgs.Msg
    | CreateMigrationMsg Questionnaires.CreateMigration.Msgs.Msg
    | DetailMsg Questionnaires.Detail.Msgs.Msg
    | EditMsg Questionnaires.Edit.Msgs.Msg
    | IndexMsg Questionnaires.Index.Msgs.Msg
    | MigrationMsg Questionnaires.Migration.Msgs.Msg
