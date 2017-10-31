module Requests exposing (..)


apiUrl : String -> String
apiUrl url =
    apiRoot ++ url


apiRoot : String
apiRoot =
    "http://localhost:3000"
