module FormEngine.Msgs exposing (..)


type Msg a
    = Input (List String) String
    | Clear (List String)
    | GroupItemAdd (List String)
    | GroupItemRemove (List String) Int
    | CustomQuestionMsg String a
