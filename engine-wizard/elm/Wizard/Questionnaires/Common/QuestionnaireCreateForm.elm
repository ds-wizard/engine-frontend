module Wizard.Questionnaires.Common.QuestionnaireCreateForm exposing (QuestionnaireCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)


type alias QuestionnaireCreateForm =
    { name : String
    , packageId : String
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    }


init : AppState -> Maybe String -> Form FormError QuestionnaireCreateForm
init appState selectedPackage =
    let
        initialPackageId =
            case selectedPackage of
                Just packageId ->
                    [ ( "packageId", Field.string packageId ) ]

                _ ->
                    []

        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues appState.config.questionnaire.questionnaireVisibility.defaultValue

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues appState.config.questionnaire.questionnaireSharing.defaultValue

        initials =
            initialPackageId
                ++ [ ( "visibilityEnabled", Field.bool visibilityEnabled )
                   , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
                   , ( "sharingEnabled", Field.bool sharingEnabled )
                   , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
                   ]
    in
    Form.initial initials validation


validation : Validation FormError QuestionnaireCreateForm
validation =
    Validate.map6 QuestionnaireCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "packageId" Validate.string)
        (Validate.field "visibilityEnabled" Validate.bool)
        (Validate.field "visibilityPermission" QuestionnairePermission.validation)
        (Validate.field "sharingEnabled" Validate.bool)
        (Validate.field "sharingPermission" QuestionnairePermission.validation)


encode : List String -> QuestionnaireCreateForm -> E.Value
encode tagUuids form =
    E.object
        [ ( "name", E.string form.name )
        , ( "packageId", E.string form.packageId )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "tagUuids", E.list E.string tagUuids )
        , ( "templateId", E.maybe E.string Nothing )
        ]
