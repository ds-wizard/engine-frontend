module Common.Api.Models.Locale.LocaleUploadResult exposing
    ( LocaleUploadResult(..)
    , PreviewData
    , ReplaceWarning
    , decoder
    )

{-| What the server says to a PO sent to it, uploaded or pulled from the
registry: without `confirmed`, what it read from the header and what it warns
about, nothing stored; with it, the stored translation.
-}

import Common.Api.Models.Locale as Locale exposing (Locale)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type LocaleUploadResult
    = Preview PreviewData
    | Stored { replaced : Bool, locale : Locale }


type alias PreviewData =
    { name : String
    , code : String
    , potVersion : Maybe String
    , warnings : List ReplaceWarning
    }


{-| The translation of the same language that would be replaced; an official
one is then no longer updated by us.
-}
type alias ReplaceWarning =
    { uuid : Uuid
    , name : String
    , code : String
    , official : Bool
    }


decoder : Decoder LocaleUploadResult
decoder =
    D.field "type" D.string
        |> D.andThen
            (\resultType ->
                case resultType of
                    "Preview" ->
                        D.map Preview previewDecoder

                    "Stored" ->
                        D.map2 (\replaced locale -> Stored { replaced = replaced, locale = locale })
                            (D.field "replaced" D.bool)
                            (D.field "locale" Locale.decoder)

                    _ ->
                        D.fail ("Unknown locale upload result: " ++ resultType)
            )


previewDecoder : Decoder PreviewData
previewDecoder =
    D.succeed PreviewData
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.optional "potVersion" (D.nullable D.string) Nothing
        |> D.required "warnings" (D.list warningDecoder)


warningDecoder : Decoder ReplaceWarning
warningDecoder =
    D.succeed ReplaceWarning
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "code" D.string
        |> D.required "official" D.bool
