module Wizard.Questionnaires.Msgs exposing (Msg(..))

import Wizard.Questionnaires.Create.Msgs
import Wizard.Questionnaires.CreateMigration.Msgs
import Wizard.Questionnaires.Detail.Msgs
import Wizard.Questionnaires.Edit.Msgs
import Wizard.Questionnaires.Index.Msgs
import Wizard.Questionnaires.Migration.Msgs


type Msg
    = CreateMsg Wizard.Questionnaires.Create.Msgs.Msg
    | CreateMigrationMsg Wizard.Questionnaires.CreateMigration.Msgs.Msg
    | DetailMsg Wizard.Questionnaires.Detail.Msgs.Msg
    | EditMsg Wizard.Questionnaires.Edit.Msgs.Msg
    | IndexMsg Wizard.Questionnaires.Index.Msgs.Msg
    | MigrationMsg Wizard.Questionnaires.Migration.Msgs.Msg
