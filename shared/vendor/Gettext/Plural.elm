module Gettext.Plural exposing
    ( PluralAst
    , defaultPluralAst
    , eval
    , fromString
    )

import Char exposing (isDigit)
import List.Extra as List


type PluralAst
    = Ternary PluralAst PluralAst PluralAst
    | OR PluralAst PluralAst
    | AND PluralAst PluralAst
    | LT PluralAst PluralAst
    | LTE PluralAst PluralAst
    | GT PluralAst PluralAst
    | GTE PluralAst PluralAst
    | NEQ PluralAst PluralAst
    | EQ PluralAst PluralAst
    | MOD PluralAst PluralAst
    | VAR
    | NUM Int


defaultPluralAst : PluralAst
defaultPluralAst =
    NEQ VAR (NUM 1)


fromString : String -> Result String PluralAst
fromString str =
    String.toList str
        |> tokenize
        |> parse


eval : PluralAst -> Int -> Int
eval ast num =
    case ast of
        VAR ->
            num

        NUM x ->
            x

        Ternary condition onTrue onFalse ->
            case eval condition num of
                0 ->
                    eval onFalse num

                _ ->
                    eval onTrue num

        OR left right ->
            bool2num ((eval left num /= 0) || (eval right num /= 0))

        AND left right ->
            bool2num ((eval left num /= 0) && (eval right num /= 0))

        LT left right ->
            bool2num (eval left num < eval right num)

        LTE left right ->
            bool2num (eval left num <= eval right num)

        GT left right ->
            bool2num (eval left num > eval right num)

        GTE left right ->
            bool2num (eval left num >= eval right num)

        EQ left right ->
            bool2num (eval left num == eval right num)

        NEQ left right ->
            bool2num (eval left num /= eval right num)

        MOD left right ->
            modBy (eval left num) (eval right num)


bool2num : Bool -> Int
bool2num val =
    -- C-like (0 = False, >=1 = True)
    case val of
        True ->
            1

        False ->
            0


bracketsOk : Int -> List Token -> Bool
bracketsOk openedBrackets tokens =
    case ( openedBrackets, tokens ) of
        ( 0, [] ) ->
            True

        ( _, [] ) ->
            False

        ( 0, ClB :: _ ) ->
            False

        ( n, OpB :: rest ) ->
            bracketsOk (n + 1) rest

        ( n, ClB :: rest ) ->
            bracketsOk (n - 1) rest

        ( n, _ :: rest ) ->
            bracketsOk n rest


unbracket : List Token -> List Token
unbracket tokens =
    case tokens of
        [] ->
            []

        OpB :: rest ->
            case List.reverse rest of
                ClB :: restReversed ->
                    let
                        inside =
                            List.reverse restReversed
                    in
                    if bracketsOk 0 inside then
                        unbracket inside

                    else
                        tokens

                _ ->
                    tokens

        _ ->
            tokens


priority : Token -> Int
priority token =
    case token of
        Q ->
            9

        And ->
            8

        Or ->
            7

        Lt ->
            6

        Lte ->
            6

        Gt ->
            6

        Gte ->
            6

        Eq ->
            6

        Neq ->
            6

        Mod ->
            5

        _ ->
            -1


selectPriority : ( Int, Token ) -> ( Int, Token ) -> ( Int, Token )
selectPriority ( prevPos, prevToken ) ( currPos, currToken ) =
    if prevToken == currToken then
        -- left-associative case (non-associative should error now?, nothing right-associative?)
        ( prevPos, prevToken )

    else
        let
            prevPriority =
                priority prevToken

            currPriority =
                priority currToken
        in
        if currPriority > prevPriority then
            ( currPos, currToken )

        else
            ( prevPos, prevToken )



-- new has lower priority


nextOp : List Token -> Int -> Int -> ( Int, Token ) -> ( Int, Token )
nextOp tokens actPos openedBrackets ( pos, tok ) =
    let
        nextPos =
            actPos + 1
    in
    case tokens of
        [] ->
            ( pos, tok )

        OpB :: rest ->
            nextOp rest nextPos (openedBrackets + 1) ( pos, tok )

        ClB :: rest ->
            nextOp rest nextPos (openedBrackets - 1) ( pos, tok )

        actToken :: rest ->
            if openedBrackets > 0 then
                nextOp rest nextPos openedBrackets ( pos, tok )
                -- in brackets, must skip for later processing

            else
                nextOp rest nextPos (openedBrackets - 1) (selectPriority ( pos, tok ) ( actPos, actToken ))


nextOpx : List Token -> ( Int, Token )
nextOpx tokens =
    case tokens of
        [] ->
            ( -1, Col )

        OpB :: rest ->
            nextOp rest 1 1 ( 0, OpB )

        first :: rest ->
            nextOp rest 1 0 ( 0, first )


findTernaryColon : List Token -> Int -> Int -> Int
findTernaryColon tokens actPos openedBrackets =
    case ( tokens, openedBrackets ) of
        ( [], _ ) ->
            actPos

        ( Col :: _, 0 ) ->
            actPos + 1

        ( OpB :: rest, _ ) ->
            findTernaryColon rest (actPos + 1) (openedBrackets + 1)

        ( ClB :: rest, _ ) ->
            findTernaryColon rest (actPos + 1) (openedBrackets - 1)

        ( _ :: rest, _ ) ->
            findTernaryColon rest (actPos + 1) openedBrackets


parseTernaryRight : List Token -> Result String ( PluralAst, PluralAst )
parseTernaryRight tokens =
    let
        colonPos =
            findTernaryColon tokens -1 0
    in
    if colonPos < 0 then
        Err "Colon for ternary expected but not found"

    else
        let
            right1 =
                parse (List.take colonPos tokens)

            right2 =
                parse (List.drop (colonPos + 1) tokens)
        in
        case ( right1, right2 ) of
            ( Err err, _ ) ->
                Err err

            ( _, Err err ) ->
                Err err

            ( Ok r1, Ok r2 ) ->
                Ok ( r1, r2 )


parse : List Token -> Result String PluralAst
parse tokens =
    -- recursive algorithm
    let
        xtokens =
            unbracket tokens
    in
    case xtokens of
        [] ->
            Err "Invalid expression (reached end when not expected)"

        [ Var ] ->
            Ok VAR

        [ Num x ] ->
            Ok (NUM x)

        _ ->
            case nextOpx xtokens of
                ( pos, Q ) ->
                    let
                        leftOperand =
                            parse (List.take pos xtokens)

                        rightOperands =
                            parseTernaryRight (List.drop (pos + 1) xtokens)
                    in
                    case ( leftOperand, rightOperands ) of
                        ( Err _, _ ) ->
                            -- propagate left error
                            leftOperand

                        ( _, Err err ) ->
                            -- propagate right error
                            Err err

                        ( Ok left, Ok ( right1, right2 ) ) ->
                            Ok (Ternary left right1 right2)

                ( pos, binaryOp ) ->
                    let
                        leftOperand =
                            parse (List.take pos xtokens)

                        rightOperand =
                            parse (List.drop (pos + 1) xtokens)
                    in
                    case ( leftOperand, rightOperand ) of
                        ( Err _, _ ) ->
                            -- propagate left error
                            leftOperand

                        ( _, Err _ ) ->
                            -- propagate right error
                            rightOperand

                        ( Ok left, Ok right ) ->
                            case binaryOp of
                                Or ->
                                    Ok (OR left right)

                                And ->
                                    Ok (AND left right)

                                Lt ->
                                    Ok (LT left right)

                                Lte ->
                                    Ok (LTE left right)

                                Gt ->
                                    Ok (GT left right)

                                Gte ->
                                    Ok (GTE left right)

                                Neq ->
                                    Ok (NEQ left right)

                                Eq ->
                                    Ok (EQ left right)

                                Mod ->
                                    Ok (MOD left right)

                                _ ->
                                    -- Invalid, e.g. Col where it should not be
                                    Err "Invalid expression"


type Token
    = Var
    | Num Int
    | Or
    | And
    | Lt
    | Lte
    | Gt
    | Gte
    | Eq
    | Neq
    | Mod
    | Q
    | Col
    | OpB
    | ClB


tokenize : List Char -> List Token
tokenize source =
    case source of
        'n' :: rest ->
            Var :: tokenize rest

        '|' :: '|' :: rest ->
            Or :: tokenize rest

        '&' :: '&' :: rest ->
            And :: tokenize rest

        '<' :: '=' :: rest ->
            Lte :: tokenize rest

        '<' :: rest ->
            Lt :: tokenize rest

        '>' :: '=' :: rest ->
            Gte :: tokenize rest

        '>' :: rest ->
            Gt :: tokenize rest

        '=' :: '=' :: rest ->
            Eq :: tokenize rest

        '!' :: '=' :: rest ->
            Neq :: tokenize rest

        '%' :: rest ->
            Mod :: tokenize rest

        '?' :: rest ->
            Q :: tokenize rest

        ':' :: rest ->
            Col :: tokenize rest

        '(' :: rest ->
            OpB :: tokenize rest

        ')' :: rest ->
            ClB :: tokenize rest

        a :: rest ->
            if isDigit a then
                let
                    num =
                        List.takeWhile isDigit source

                    afterNum =
                        List.dropWhile isDigit source
                in
                Num (Maybe.withDefault 0 (String.toInt (String.fromList num))) :: tokenize afterNum

            else
                tokenize rest

        _ ->
            []
