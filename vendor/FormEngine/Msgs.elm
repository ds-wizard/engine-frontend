module FormEngine.Msgs exposing (Msg(..))

import FormEngine.Model exposing (ReplyValue)


type Msg a
    = Input (List String) ReplyValue
    | InputTypehint (List String) String ReplyValue
    | Clear (List String)
    | GroupItemAdd (List String)
    | GroupItemRemove (List String) Int
    | CustomQuestionMsg String a
    | ShowTypeHints (List String) String
    | HideTypeHints
