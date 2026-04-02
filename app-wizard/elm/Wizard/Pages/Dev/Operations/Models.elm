module Wizard.Pages.Dev.Operations.Models exposing
    ( Model
    , fieldPath
    , getSection
    , getTypeHintInputModel
    , initialModel
    , operationPath
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Api.Models.DevOperationSection exposing (DevOperationSection)
import Common.Api.Models.TenantSuggestion exposing (TenantSuggestion)
import Common.Components.TypeHintInput as TypeHintInput
import Dict exposing (Dict)
import List.Extra as List


type alias Model =
    { adminOperationSections : ActionResult (List DevOperationSection)
    , openedSection : Maybe String
    , fieldValues : Dict String String
    , typeHintInputModels : Dict String (TypeHintInput.Model TenantSuggestion)
    , operationResults : Dict String (ActionResult DevOperationExecutionResult)
    }


initialModel : Model
initialModel =
    { adminOperationSections = Loading
    , openedSection = Nothing
    , fieldValues = Dict.empty
    , typeHintInputModels = Dict.empty
    , operationResults = Dict.empty
    }


fieldPath : String -> String -> String -> String
fieldPath sectionName operationName parameterName =
    sectionName ++ "__" ++ operationName ++ "__" ++ parameterName


operationPath : String -> String -> String
operationPath sectionName operationName =
    sectionName ++ "__" ++ operationName


getSection : String -> Model -> Maybe DevOperationSection
getSection sectionName model =
    ActionResult.unwrap Nothing (List.find (.name >> (==) sectionName)) model.adminOperationSections


getTypeHintInputModel : String -> Model -> TypeHintInput.Model TenantSuggestion
getTypeHintInputModel path model =
    Maybe.withDefault (TypeHintInput.init path) (Dict.get path model.typeHintInputModels)
