module Wizard.Pages.KMEditor.Msgs exposing (Msg(..))

import Wizard.Pages.KMEditor.Create.Msgs
import Wizard.Pages.KMEditor.Editor.Msgs
import Wizard.Pages.KMEditor.Index.Msgs
import Wizard.Pages.KMEditor.Migration.Msgs
import Wizard.Pages.KMEditor.Publish.Msgs


type Msg
    = CreateMsg Wizard.Pages.KMEditor.Create.Msgs.Msg
    | EditorMsg Wizard.Pages.KMEditor.Editor.Msgs.Msg
    | IndexMsg Wizard.Pages.KMEditor.Index.Msgs.Msg
    | MigrationMsg Wizard.Pages.KMEditor.Migration.Msgs.Msg
    | PublishMsg Wizard.Pages.KMEditor.Publish.Msgs.Msg
