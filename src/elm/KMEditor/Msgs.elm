module KMEditor.Msgs exposing (..)

import KMEditor.Create.Msgs
import KMEditor.Editor.Msgs
import KMEditor.Editor2.Msgs
import KMEditor.Index.Msgs
import KMEditor.Migration.Msgs
import KMEditor.Publish.Msgs


type Msg
    = CreateMsg KMEditor.Create.Msgs.Msg
    | EditorMsg KMEditor.Editor.Msgs.Msg
    | Editor2Msg KMEditor.Editor2.Msgs.Msg
    | IndexMsg KMEditor.Index.Msgs.Msg
    | MigrationMsg KMEditor.Migration.Msgs.Msg
    | PublishMsg KMEditor.Publish.Msgs.Msg
