module KMEditor.Common.KnowledgeModel.Reference.ReferenceType exposing
    ( ReferenceType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type ReferenceType
    = ResourcePageReferenceType
    | URLReferenceType
    | CrossReferenceType


decoder : Decoder ReferenceType
decoder =
    D.field "referenceType" D.string
        |> D.andThen
            (\str ->
                case str of
                    "ResourcePageReference" ->
                        D.succeed ResourcePageReferenceType

                    "URLReference" ->
                        D.succeed URLReferenceType

                    "CrossReference" ->
                        D.succeed CrossReferenceType

                    valueType ->
                        D.fail <| "Unknown reference type: " ++ valueType
            )
