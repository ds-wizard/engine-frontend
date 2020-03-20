module Wizard.Common.Form.Field exposing (maybeString)

import Form.Field as Field


maybeString : Maybe String -> Field.Field
maybeString =
    Field.string << Maybe.withDefault ""
