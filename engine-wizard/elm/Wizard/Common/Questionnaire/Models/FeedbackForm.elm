module Wizard.Common.Questionnaire.Models.FeedbackForm exposing
    ( FeedbackForm
    , encode
    , initEmpty
    , validation
    )

import Form
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Wizard.Common.Form exposing (CustomFormError)


type alias FeedbackForm =
    { title : String
    , content : String
    }


initEmpty : Form.Form CustomFormError FeedbackForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError FeedbackForm
validation =
    Validate.map2 FeedbackForm
        (Validate.field "title" Validate.string)
        (Validate.field "content" Validate.string)


encode : String -> String -> FeedbackForm -> E.Value
encode questionUuid packageId form =
    E.object
        [ ( "questionUuid", E.string questionUuid )
        , ( "packageId", E.string packageId )
        , ( "title", E.string form.title )
        , ( "content", E.string form.content )
        ]
