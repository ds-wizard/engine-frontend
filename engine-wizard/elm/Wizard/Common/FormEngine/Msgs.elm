module Wizard.Common.FormEngine.Msgs exposing (Msg(..))

import Debounce
import Shared.Data.QuestionnaireDetail.FormValue.ReplyValue exposing (ReplyValue)
import Wizard.Common.FormEngine.Model exposing (TypeHint)


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
