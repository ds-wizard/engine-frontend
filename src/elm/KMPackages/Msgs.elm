module KMPackages.Msgs exposing (Msg(..))

import KMPackages.Detail.Msgs
import KMPackages.Import.Msgs
import KMPackages.Index.Msgs


type Msg
    = DetailMsg KMPackages.Detail.Msgs.Msg
    | ImportMsg KMPackages.Import.Msgs.Msg
    | IndexMsg KMPackages.Index.Msgs.Msg
