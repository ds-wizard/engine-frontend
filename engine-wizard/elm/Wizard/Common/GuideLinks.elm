module Wizard.Common.GuideLinks exposing
    ( GuideLinks
    , decoder
    , default
    , documentTemplates
    , integrationQuestionSecrets
    , markdownCheatsheet
    , merge
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)


type GuideLinks
    = GuideLinks (Dict String String)


decoder : Decoder GuideLinks
decoder =
    D.dict D.string
        |> D.map GuideLinks


default : GuideLinks
default =
    GuideLinks <|
        Dict.fromList
            [ ( "documentTemplates", "https://guide.ds-wizard.org/en/latest/more/development/document-templates/index.html" )
            , ( "integrationQuestionSecrets", "https://guide.ds-wizard.org/en/latest/more/development/integration-questions/integration-api.html#secrets-and-other-properties" )
            , ( "markdownCheatsheet", "https://guide.ds-wizard.org/en/latest/more/miscellaneous/markdown-cheatsheet.html" )
            ]


merge : GuideLinks -> GuideLinks -> GuideLinks
merge (GuideLinks guideLinksA) (GuideLinks guideLinksB) =
    GuideLinks <|
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a _ -> Dict.insert key a)
            (\key b -> Dict.insert key b)
            guideLinksA
            guideLinksB
            Dict.empty


get : String -> GuideLinks -> String
get key (GuideLinks guideLinks) =
    Dict.get key guideLinks
        |> Maybe.withDefault ""


documentTemplates : GuideLinks -> String
documentTemplates =
    get "documentTemplates"


integrationQuestionSecrets : GuideLinks -> String
integrationQuestionSecrets =
    get "integrationQuestionSecrets"


markdownCheatsheet : GuideLinks -> String
markdownCheatsheet =
    get "markdownCheatsheet"
