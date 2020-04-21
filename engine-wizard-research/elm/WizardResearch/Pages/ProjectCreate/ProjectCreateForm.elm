module WizardResearch.Pages.ProjectCreate.ProjectCreateForm exposing
    ( ProjectCreateForm
    , encode
    , init
    , selectRecommendedOrFirstTemplate
    , selectRecommendedPackage
    , validation
    )

-- MODEL

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Questionnaire.QuestionnaireAccessibility as QuestionnaireAccessibility
import Shared.Data.Template as Template exposing (Template)
import Shared.Form.FormError exposing (FormError)
import Uuid


type alias ProjectCreateForm =
    { name : String
    , templateUuid : String
    , packageId : String
    , tagUuids : List String
    }


validation : List Tag -> Validation FormError ProjectCreateForm
validation tags =
    let
        validateTag tag =
            V.field tag.uuid
                (V.bool
                    |> V.andThen
                        (\checked ->
                            if checked then
                                V.succeed (Just tag.uuid)

                            else
                                V.succeed Nothing
                        )
                )

        validateTagUuids =
            V.sequence (List.map validateTag tags)
                |> V.map (List.filterMap identity)
    in
    V.succeed ProjectCreateForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "templateUuid" V.string)
        |> V.andMap (V.field "packageId" V.string)
        |> V.andMap (V.field "tagUuids" validateTagUuids)


init : Form FormError ProjectCreateForm
init =
    Form.initial [] (validation [])


encode : ProjectCreateForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "templateUuid", E.string form.templateUuid )
        , ( "packageId", E.string form.packageId )
        , ( "tagUuids", E.list E.string form.tagUuids )
        , ( "accessibility", QuestionnaireAccessibility.encode QuestionnaireAccessibility.PrivateQuestionnaire )
        ]


selectRecommendedOrFirstTemplate : List Tag -> List Template -> Maybe String -> Form FormError ProjectCreateForm -> Form FormError ProjectCreateForm
selectRecommendedOrFirstTemplate tags templates mbRecommendedTemplateUuid form =
    let
        mbTemplateUuid =
            case mbRecommendedTemplateUuid of
                Just uuid ->
                    Just uuid

                Nothing ->
                    Maybe.map (Uuid.toString << .uuid) (List.head templates)
    in
    case mbTemplateUuid of
        Just templateUuid ->
            let
                msg =
                    Form.Input "templateUuid" Form.Text (Field.String templateUuid)
            in
            Form.update (validation tags) msg form

        Nothing ->
            form


selectRecommendedPackage : List Tag -> List Template -> Form FormError ProjectCreateForm -> Form FormError ProjectCreateForm
selectRecommendedPackage tags templates form =
    let
        mbTemplate =
            (Form.getFieldAsString "templateUuid" form).value
                |> Maybe.andThen (Template.findByUuid templates)
    in
    case mbTemplate of
        Just template ->
            let
                msg =
                    Form.Input "packageId" Form.Text (Field.String template.recommendedPackageId)
            in
            Form.update (validation tags) msg form

        _ ->
            form
