module Shared.Elemental.Foundations.Transition exposing (default, slower)

import Css.Transitions exposing (easeInOut)


default fn =
    fn 125 0 easeInOut


slower fn =
    fn 250 0 easeInOut
