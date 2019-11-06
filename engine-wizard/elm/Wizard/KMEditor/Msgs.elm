module Wizard.KMEditor.Msgs exposing (Msg(..))

import Wizard.KMEditor.Create.Msgs
import Wizard.KMEditor.Editor.Msgs
import Wizard.KMEditor.Index.Msgs
import Wizard.KMEditor.Migration.Msgs
import Wizard.KMEditor.Publish.Msgs


type Msg
    = CreateMsg Wizard.KMEditor.Create.Msgs.Msg
    | EditorMsg Wizard.KMEditor.Editor.Msgs.Msg
    | IndexMsg Wizard.KMEditor.Index.Msgs.Msg
    | MigrationMsg Wizard.KMEditor.Migration.Msgs.Msg
    | PublishMsg Wizard.KMEditor.Publish.Msgs.Msg
