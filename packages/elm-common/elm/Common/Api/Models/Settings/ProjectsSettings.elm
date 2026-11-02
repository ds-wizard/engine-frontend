module Common.Api.Models.Settings.ProjectsSettings exposing
    ( ProjectTagging
    , ProjectsSettings
    , decoder
    , encode
    , section
    )

import Common.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Common.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)
import Common.Api.Models.Settings.ProjectsSettings.ProjectSharingSettings as ProjectSharingSettings exposing (ProjectSharingSettings)
import Common.Api.Models.Settings.ProjectsSettings.ProjectVisibilitySettings as ProjectVisibilitySettings exposing (ProjectVisibilitySettings)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ProjectsSettings =
    { projectVisibility : ProjectVisibilitySettings
    , projectSharing : ProjectSharingSettings
    , projectCreation : ProjectCreation
    , summaryReport : SimpleFeatureConfig
    , projectTagging : ProjectTagging
    }


type alias ProjectTagging =
    { enabled : Bool
    , tags : List String
    }


section : { name : String, decoder : Decoder ProjectsSettings, encode : ProjectsSettings -> E.Value }
section =
    { name = "projects", decoder = decoder, encode = encode }


decoder : Decoder ProjectsSettings
decoder =
    D.succeed ProjectsSettings
        |> D.required "projectVisibility" ProjectVisibilitySettings.decoder
        |> D.required "projectSharing" ProjectSharingSettings.decoder
        |> D.required "projectCreation" ProjectCreation.decoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder
        |> D.required "projectTagging" projectTaggingDecoder


projectTaggingDecoder : Decoder ProjectTagging
projectTaggingDecoder =
    D.succeed ProjectTagging
        |> D.required "enabled" D.bool
        |> D.required "tags" (D.list D.string)


encode : ProjectsSettings -> E.Value
encode config =
    E.object
        [ ( "projectVisibility", ProjectVisibilitySettings.encode config.projectVisibility )
        , ( "projectSharing", ProjectSharingSettings.encode config.projectSharing )
        , ( "projectCreation", ProjectCreation.encode config.projectCreation )
        , ( "summaryReport", SimpleFeatureConfig.encode config.summaryReport )
        , ( "projectTagging", encodeProjectTagging config.projectTagging )
        ]


encodeProjectTagging : ProjectTagging -> E.Value
encodeProjectTagging projectTagging =
    E.object
        [ ( "enabled", E.bool projectTagging.enabled )
        , ( "tags", E.list E.string projectTagging.tags )
        ]
