module FormEngine.Msgs exposing (..)


type Msg
    = Input (List String) String
    | GroupItemAdd (List String)
    | GroupItemRemove (List String) Int
