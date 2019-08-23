module Common.FormEngine.Msgs exposing (Msg(..))

import Common.FormEngine.Model exposing (ReplyValue, TypeHint)
import Debounce


type Msg a err
    = Input (List String) ReplyValue
    | InputTypehint (List String) String ReplyValue
    | Clear (List String)
    | GroupItemAdd (List String)
    | GroupItemRemove (List String) Int
    | CustomQuestionMsg String a
    | ShowTypeHints (List String) String String
    | HideTypeHints
    | DebounceMsg Debounce.Msg
    | TypeHintsLoaded (Result err (List TypeHint))
