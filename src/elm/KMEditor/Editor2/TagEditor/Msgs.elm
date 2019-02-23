module KMEditor.Editor2.TagEditor.Msgs exposing (Msg(..))


type Msg
    = Highlight String
    | CancelHighlight
    | AddTag String String
    | RemoveTag String String
