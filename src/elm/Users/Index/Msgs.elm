module Users.Index.Msgs exposing (Msg(..))

import Jwt
import Users.Common.Models exposing (User)


type Msg
    = GetUsersCompleted (Result Jwt.JwtError (List User))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result Jwt.JwtError String)
