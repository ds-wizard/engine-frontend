module Wizard.Api.Models.KnowledgeModelPackageDeletionImpact exposing (KnowledgeModelPackageDeletionImpact, KnowledgeModelPackageDeletionImpactEditor, KnowledgeModelPackageDeletionImpactPackage, KnowledgeModelPackageDeletionImpactProject, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelPackageDeletionImpact =
    { uuid : Uuid
    , name : String
    , version : Version
    , projects : List KnowledgeModelPackageDeletionImpactProject
    , packages : List KnowledgeModelPackageDeletionImpactPackage
    , editors : List KnowledgeModelPackageDeletionImpactEditor
    }


decoder : Decoder KnowledgeModelPackageDeletionImpact
decoder =
    D.succeed KnowledgeModelPackageDeletionImpact
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
        |> D.required "projects" (D.list knowledgeModelPackageDeletionImpactProjectDecoder)
        |> D.required "packages" (D.list knowledgeModelPackageDeletionImpactPackageDecoder)
        |> D.required "editors" (D.list knowledgeModelPackageDeletionImpactEditorDecoder)


type alias KnowledgeModelPackageDeletionImpactProject =
    { uuid : Uuid
    , name : String
    }


knowledgeModelPackageDeletionImpactProjectDecoder : Decoder KnowledgeModelPackageDeletionImpactProject
knowledgeModelPackageDeletionImpactProjectDecoder =
    D.succeed KnowledgeModelPackageDeletionImpactProject
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string


type alias KnowledgeModelPackageDeletionImpactPackage =
    { uuid : Uuid
    , name : String
    , version : Version
    }


knowledgeModelPackageDeletionImpactPackageDecoder : Decoder KnowledgeModelPackageDeletionImpactPackage
knowledgeModelPackageDeletionImpactPackageDecoder =
    D.succeed KnowledgeModelPackageDeletionImpactPackage
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "version" Version.decoder


type alias KnowledgeModelPackageDeletionImpactEditor =
    { uuid : Uuid
    , name : String
    }


knowledgeModelPackageDeletionImpactEditorDecoder : Decoder KnowledgeModelPackageDeletionImpactEditor
knowledgeModelPackageDeletionImpactEditorDecoder =
    D.succeed KnowledgeModelPackageDeletionImpactEditor
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
