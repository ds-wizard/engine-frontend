module Shared.Data.AdminOperationSection exposing
    ( AdminOperationSection
    , decoder
    , getOperation
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Shared.Data.AdminOperation as AdminOperation exposing (AdminOperation)


type alias AdminOperationSection =
    { name : String
    , description : Maybe String
    , operations : List AdminOperation
    }


decoder : Decoder AdminOperationSection
decoder =
    D.succeed AdminOperationSection
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "operations" (D.list AdminOperation.decoder)


getOperation : String -> AdminOperationSection -> Maybe AdminOperation
getOperation operationName section =
    List.find (.name >> (==) operationName) section.operations
