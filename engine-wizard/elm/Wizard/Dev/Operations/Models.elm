module Wizard.Dev.Operations.Models exposing
    ( Model
    , fieldPath
    , getSection
    , initialModel
    , operationPath
    )

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import List.Extra as List
import Shared.Data.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Shared.Data.DevOperationSection exposing (DevOperationSection)


type alias Model =
    { adminOperationSections : ActionResult (List DevOperationSection)
    , openedSection : Maybe String
    , fieldValues : Dict String String
    , operationResults : Dict String (ActionResult DevOperationExecutionResult)
    }


initialModel : Model
initialModel =
    { adminOperationSections = Loading
    , openedSection = Nothing
    , fieldValues = Dict.empty
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
