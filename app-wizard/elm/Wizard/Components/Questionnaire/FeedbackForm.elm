module Wizard.Components.Questionnaire.FeedbackForm exposing
    ( FeedbackForm
    , encode
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias FeedbackForm =
    { title : String
    , content : String
    }


initEmpty : Form.Form FormError FeedbackForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError FeedbackForm
validation =
    Validate.map2 FeedbackForm
        (Validate.field "title" Validate.string)
        (Validate.field "content" Validate.string)


encode : String -> String -> FeedbackForm -> E.Value
encode questionUuid knowledgeModelPackageId form =
    E.object
        [ ( "questionUuid", E.string questionUuid )
        , ( "knowledgeModelPackageId", E.string knowledgeModelPackageId )
        , ( "title", E.string form.title )
        , ( "content", E.string form.content )
        ]
