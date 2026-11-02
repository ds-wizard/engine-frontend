module Common.Api.Models.DevOperationSection exposing
    ( DevOperationSection
    , decoder
    , getOperation
    )

import Common.Api.Models.DevOperation as AdminOperation exposing (DevOperation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List


type alias DevOperationSection =
    { name : String
    , description : Maybe String
    , operations : List DevOperation
    }


decoder : Decoder DevOperationSection
decoder =
    D.succeed DevOperationSection
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "operations" (D.list AdminOperation.decoder)


getOperation : String -> DevOperationSection -> Maybe DevOperation
getOperation operationName section =
    List.find (.name >> (==) operationName) section.operations
