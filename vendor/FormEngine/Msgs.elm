module FormEngine.Msgs exposing (..)


type Msg a
    = Input (List String) String
    | GroupItemAdd (List String)
    | GroupItemRemove (List String) Int
    | CustomQuestionMsg String a
