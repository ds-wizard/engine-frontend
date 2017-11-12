module KnowledgeModels.Models exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import Regex exposing (regex)
import Utils exposing (validateRegex)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , artifactId : String
    , parentPackageId : Maybe String
    }


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "artifactId" Decode.string
        |> required "parentPackageId" (Decode.nullable Decode.string)


knowledgeModelListDecoder : Decoder (List KnowledgeModel)
knowledgeModelListDecoder =
    Decode.list knowledgeModelDecoder


type alias KnowledgeModelCreateForm =
    { name : String
    , artifactId : String
    , parentPackageId : Maybe String
    }


initKnowledgeModelCreateForm : Form () KnowledgeModelCreateForm
initKnowledgeModelCreateForm =
    Form.initial [] knowledgeModelCreateFormValidation


knowledgeModelCreateFormValidation : Validation () KnowledgeModelCreateForm
knowledgeModelCreateFormValidation =
    Validate.map3 KnowledgeModelCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "artifactId" (validateRegex "^[a-zA-Z0-9_-]+$"))
        (Validate.field "parentPackageId" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


encodeKnowledgeModelForm : String -> KnowledgeModelCreateForm -> Encode.Value
encodeKnowledgeModelForm uuid form =
    let
        parentPackage =
            case form.parentPackageId of
                Just parentPackageId ->
                    Encode.string parentPackageId

                Nothing ->
                    Encode.null
    in
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "name", Encode.string form.name )
        , ( "artifactId", Encode.string form.artifactId )
        , ( "parentPackageId", parentPackage )
        ]
