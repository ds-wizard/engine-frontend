module Wizard.Api.Models.DocumentTemplateSuggestion exposing
    ( DocumentTemplateSuggestion
    , decoder
    , languageOptions
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatSimple as DocumentTemplateFormatSimple exposing (DocumentTemplateFormatSimple)
import Wizard.Api.Models.DocumentTemplateLocale as DocumentTemplateLocale exposing (DocumentTemplateLocale)


type alias DocumentTemplateSuggestion =
    { uuid : Uuid
    , name : String
    , description : String
    , organizationId : String
    , templateId : String
    , version : Version
    , formats : List DocumentTemplateFormatSimple
    , language : String
    , locales : List DocumentTemplateLocale
    }


decoder : Decoder DocumentTemplateSuggestion
decoder =
    D.succeed DocumentTemplateSuggestion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "organizationId" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "formats" (D.list DocumentTemplateFormatSimple.decoder)
        |> D.required "language" D.string
        |> D.required "locales" (D.list DocumentTemplateLocale.decoder)


{-| Languages the document template can be used with -- its default language plus
the code of every locale imported for it.
-}
languageOptions : DocumentTemplateSuggestion -> List ( String, String )
languageOptions documentTemplate =
    ( documentTemplate.language, documentTemplate.language )
        :: List.map (\locale -> ( locale.code, locale.code )) (List.sortBy .code documentTemplate.locales)
