module Fluent.Parser exposing (..)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , chompIf
        , chompWhile
        , end
        , float
        , getChompedString
        , int
        , loop
        , map
        , oneOf
        , problem
        , spaces
        , succeed
        , symbol
        , token
        , variable
        )
import Set


type Resource
    = Resource (List Entry)


resource : Parser Resource
resource =
    listOf entry
        |> map Resource


type Entry
    = Message EntryComment Identifier MessageDefinition
    | Term EntryComment Identifier Pattern (List Attribute)
    | ResourceComment String
    | GroupComment String
    | StandaloneComment String


entry : Parser Entry
entry =
    oneOf
        [ resourceComment
        , groupComment
        , entryComment
            |> andThen
                (\maybeComment ->
                    oneOf
                        [ term maybeComment
                        , message maybeComment
                        , case maybeComment of
                            Just comment ->
                                succeed (StandaloneComment comment)

                            Nothing ->
                                problem "Expecting # (Standalone Comment)"
                        ]
                )
        ]


type MessageDefinition
    = ValueMessage Pattern (List Attribute)
    | AttributeMessage Attribute (List Attribute)


message : EntryComment -> Parser Entry
message comment =
    succeed (Message comment)
        |= identifier
        |. spaces
        |. symbol "="
        |. sameLineSpaces
        |= messageDefinition


messageDefinition : Parser MessageDefinition
messageDefinition =
    succeed Tuple.pair
        |= optionalPattern
        |= listOf attribute
        |> andThen buildMessageDefinition


buildMessageDefinition : ( List PatternElement, List Attribute ) -> Parser MessageDefinition
buildMessageDefinition found =
    case found of
        ( head :: tail, attributes ) ->
            succeed (ValueMessage (Pattern head tail) attributes)

        ( [], head :: tail ) ->
            succeed (AttributeMessage head tail)

        ( [], [] ) ->
            problem "E0005: Expected message to have a value or attributes"


term : EntryComment -> Parser Entry
term comment =
    succeed (Term comment)
        |. symbol "-"
        |= identifier
        |. spaces
        |. symbol "="
        |. sameLineSpaces
        |= requiredPattern
        |= listOf attribute


type Attribute
    = Attribute Identifier Pattern


attribute : Parser Attribute
attribute =
    succeed Attribute
        |. spaces
        |. symbol "."
        |= identifier
        |. spaces
        |. symbol "="
        |. sameLineSpaces
        |= requiredPattern


type alias EntryComment =
    Maybe String


entryComment : Parser EntryComment
entryComment =
    optionalComment 1


optionalComment : Int -> Parser (Maybe String)
optionalComment count =
    let
        line : Parser String
        line =
            (Parser.lineComment (String.repeat count "#")
                |> getChompedString
                |> map (String.dropLeft <| count + 1)
            )
                |. oneOf [ symbol "\n", end ]

        finish : List String -> Parser (Maybe String)
        finish lines =
            if List.isEmpty lines then
                succeed Nothing

            else
                succeed (Just <| String.join "\n" lines)
    in
    listOf line
        |> andThen finish


requiredComment : (String -> b) -> Int -> Parser b
requiredComment tag count =
    let
        finish maybe =
            case maybe of
                Just comment ->
                    succeed (tag comment)

                Nothing ->
                    problem "Expected comment"
    in
    optionalComment count |> andThen finish


resourceComment : Parser Entry
resourceComment =
    requiredComment ResourceComment 3


groupComment : Parser Entry
groupComment =
    requiredComment GroupComment 2


type Pattern
    = Pattern PatternElement (List PatternElement)


type PatternElement
    = SelectExpression InlineExpression (List Variant) Variant (List Variant)
    | InlineExpression InlineExpression
    | TextElement String


optionalPattern : Parser (List PatternElement)
optionalPattern =
    loop
        { elements = []
        , seenLines = ""
        , seenSpaces = ""
        , minIndent = Nothing
        }
        patternStep


requiredPattern : Parser Pattern
requiredPattern =
    let
        finish items =
            case items of
                [] ->
                    problem "Expected at least one pattern element."

                head :: tail ->
                    succeed (Pattern head tail)
    in
    optionalPattern
        |> andThen finish


type alias PatternState =
    { elements : List PatternElement
    , seenLines : String
    , seenSpaces : String
    , minIndent : Maybe Int
    }


patternStep :
    PatternState
    -> Parser (Step PatternState (List PatternElement))
patternStep state =
    oneOf
        [ succeed identity
            |. symbol "\n"
            |= oneOf
                [ succeed (rememberPatternNewLines state)
                    |= zeroOrMore ((==) '\n')
                    |= oneOrMore ((==) ' ')
                , succeed () |> map (\_ -> finishPattern state)
                ]
        , placeable
            |> map (addPatternElement state)
        , succeed ()
            |. chompIf
                (if String.isEmpty state.seenLines then
                    textChunkValid

                 else
                    -- Disable starting with "}", "[", "*", or "."  after the first line
                    \c -> c /= '[' && c /= '*' && c /= '.' && textChunkValid c
                )
            |. chompWhile textChunkValid
            |> getChompedString
            |> map (TextElement >> addPatternElement state)
        , succeed () |> map (\_ -> finishPattern state)
        ]


textChunkValid : Char -> Bool
textChunkValid c =
    c /= '\n' && c /= '{' && c /= '}'


rememberPatternNewLines : PatternState -> String -> String -> Step PatternState (List PatternElement)
rememberPatternNewLines state lines spaces =
    Loop
        { state
            | seenLines = state.seenLines ++ "\n" ++ lines
            , seenSpaces = spaces
        }


addPatternElement : PatternState -> PatternElement -> Step PatternState (List PatternElement)
addPatternElement state element =
    Loop
        { state
            | elements =
                case ( element, state.elements ) of
                    ( TextElement new, [] ) ->
                        [ TextElement (state.seenSpaces ++ new) ]

                    ( TextElement new, (TextElement previous) :: rest ) ->
                        TextElement (previous ++ state.seenLines ++ state.seenSpaces ++ new) :: rest

                    ( TextElement new, _ ) ->
                        TextElement (state.seenLines ++ state.seenSpaces ++ new) :: state.elements

                    ( _, [] ) ->
                        [ element, TextElement state.seenSpaces ]

                    ( _, (TextElement previous) :: rest ) ->
                        element :: TextElement (previous ++ state.seenLines ++ state.seenSpaces) :: rest

                    ( _, _ ) ->
                        element :: TextElement (state.seenLines ++ state.seenSpaces) :: state.elements
            , seenLines = ""
            , seenSpaces = ""
            , minIndent =
                if state.seenSpaces == "" then
                    state.minIndent

                else
                    let
                        spaceIndent : Int
                        spaceIndent =
                            String.length state.seenSpaces
                    in
                    case state.minIndent of
                        Just current ->
                            if spaceIndent < current then
                                Just spaceIndent

                            else
                                Just current

                        Nothing ->
                            Just spaceIndent
        }


finishPattern : PatternState -> Step PatternState (List PatternElement)
finishPattern state =
    let
        indent : Int
        indent =
            Maybe.withDefault 0 state.minIndent

        trimmed : List PatternElement
        trimmed =
            case state.elements of
                (TextElement last) :: rest ->
                    TextElement (String.trimRight last) :: rest

                _ ->
                    state.elements
    in
    Done <| alignPattern indent trimmed []


alignPattern : Int -> List PatternElement -> List PatternElement -> List PatternElement
alignPattern indent remaining done =
    case remaining of
        [] ->
            done

        (TextElement text) :: rest ->
            let
                alignLine : String -> String
                alignLine line =
                    if String.startsWith " " line then
                        String.dropLeft indent line

                    else
                        line

                aligned : String
                aligned =
                    text
                        |> String.lines
                        |> List.map alignLine
                        |> String.join "\n"
            in
            if String.isEmpty aligned then
                -- Skip elements that are empty after alignment
                alignPattern indent rest done

            else
                alignPattern indent rest (TextElement aligned :: done)

        element :: rest ->
            alignPattern indent rest (element :: done)


placeable : Parser PatternElement
placeable =
    let
        expression expr =
            oneOf
                [ selectExpression expr
                , succeed (InlineExpression expr)
                ]
    in
    succeed identity
        |. symbol "{"
        |. spaces
        |= (inlineExpression |> andThen expression)
        |. spaces
        |. symbol "}"


selectExpression : InlineExpression -> Parser PatternElement
selectExpression expr =
    succeed SelectExpression
        |. symbol " ->"
        |= selector expr
        |. spaces
        |= listOf variant
        |. oneOf
            [ symbol "*"
            , problem "E0010: Expected one of the variants to be marked as default (*)"
            ]
        |= variant
        |= listOf variant


selector : InlineExpression -> Parser InlineExpression
selector expr =
    case expr of
        MessageReference _ Nothing ->
            problem "E0016: Message references cannot be used as selectors"

        TermReference _ Nothing _ ->
            problem "E0017: Terms cannot be used as selectors"

        MessageReference _ (Just _) ->
            problem "E0018: Attributes of messages cannot be used as selectors"

        _ ->
            succeed expr


type Variant
    = Variant Identifier Pattern


variant : Parser Variant
variant =
    succeed Variant
        |. symbol "["
        |= identifier
        |. symbol "]"
        |. sameLineSpaces
        |= requiredPattern
        |. spaces


type InlineExpression
    = Literal Literal
    | FunctionReference Identifier CallArguments
    | TermReference Identifier AttributeAccessor CallArguments
    | MessageReference Identifier AttributeAccessor
    | VariableReference Identifier


inlineExpression : Parser InlineExpression
inlineExpression =
    oneOf
        [ succeed identity
            |. symbol "-"
            |= oneOf
                [ succeed TermReference
                    |= identifier
                    |= attributeAccessor
                    |= oneOf
                        [ callArguments
                        , succeed noArguments
                        ]
                , unsignedNumber |> map (negate >> Number >> Literal)
                ]
        , identifier
            |> andThen
                (\id ->
                    oneOf
                        [ map (FunctionReference id) callArguments
                        , succeed (MessageReference id)
                            |= attributeAccessor
                        ]
                )
        , unsignedNumber |> map (Number >> Literal)
        , stringLiteral |> map Literal
        , variableReference
        ]


type alias AttributeAccessor =
    Maybe Identifier


attributeAccessor : Parser AttributeAccessor
attributeAccessor =
    oneOf
        [ succeed Just
            |. symbol "."
            |= identifier
        , succeed Nothing
        ]


type alias CallArguments =
    { positional : List InlineExpression
    , named : List ( Identifier, Literal )
    }


noArguments : CallArguments
noArguments =
    { positional = [], named = [] }


callArguments : Parser CallArguments
callArguments =
    succeed identity
        |. symbol "("
        |. spaces
        |= oneOf
            [ succeed noArguments
                |. symbol ")"
            , loop noArguments argumentStep
            ]


argumentStep : CallArguments -> Parser (Step CallArguments CallArguments)
argumentStep args =
    let
        finish next =
            succeed next
                |= oneOf
                    [ succeed Loop
                        |. spaces
                        |. symbol ","
                        |. spaces
                    , succeed
                        (\{ positional, named } ->
                            Done
                                { positional = List.reverse positional
                                , named = List.reverse named
                                }
                        )
                        |. spaces
                        |. symbol ")"
                    ]
    in
    oneOf
        [ succeed (\id lit next -> next { args | named = ( id, lit ) :: args.named })
            |= identifier
            |. spaces
            |. symbol ":"
            |. spaces
            |= literal
            |. spaces
        , succeed (\expr next -> next { args | positional = expr :: args.positional })
            |= inlineExpression
            |. (if List.isEmpty args.named then
                    succeed ()

                else
                    problem "E0021: Positional arguments must not follow named arguments"
               )
        ]
        |> andThen finish


variableReference : Parser InlineExpression
variableReference =
    succeed VariableReference
        |. symbol "$"
        |= identifier


type Identifier
    = Identifier String


identifier : Parser Identifier
identifier =
    variable
        { start = Char.isAlpha
        , inner = \c -> c == '-' || c == '_' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> map Identifier


type Literal
    = Number Float
    | String String


literal : Parser Literal
literal =
    oneOf
        [ succeed (negate >> Number)
            |. symbol "-"
            |= unsignedNumber
        , unsignedNumber |> map Number
        , stringLiteral
        ]


unsignedNumber : Parser Float
unsignedNumber =
    oneOf
        [ floatDigitRequired
        , map toFloat int
        ]


floatDigitRequired : Parser Float
floatDigitRequired =
    oneOf
        [ symbol "."
            |> andThen
                (\_ ->
                    problem "Floating point numbers must start with a digit, like 0.25"
                )
        , float
        ]


stringLiteral : Parser Literal
stringLiteral =
    succeed String
        |. symbol "\""
        |= loop [] stringStep


stringStep : List String -> Parser (Step (List String) String)
stringStep revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\"") (token "\"")
                , map (\_ -> "\\") (token "\\")
                , succeed identity
                    |. token "u"
                    |= unicode 4
                , succeed identity
                    |. token "U"
                    |= unicode 6
                ]
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
        , chompWhile (\c -> c /= '\\' && c /= '"')
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


unicode : Int -> Parser String
unicode length =
    loop 0
        (\count ->
            if count == length then
                succeed () |> map (\_ -> Done count)

            else
                oneOf
                    [ succeed (Loop (count + 1))
                        |. chompIf Char.isHexDigit
                    , invalidUnicode
                    ]
        )
        |> getChompedString
        |> andThen codeToChar
        |> map String.fromChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        code =
            String.foldl addHex 0 str
    in
    if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        invalidUnicode


invalidUnicode : Parser a
invalidUnicode =
    problem "E0026: Invalid Unicode escape sequence"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)



-- Utilities


zeroOrMore : (Char -> Bool) -> Parser String
zeroOrMore isOk =
    chompWhile isOk
        |> getChompedString


oneOrMore : (Char -> Bool) -> Parser String
oneOrMore isOk =
    succeed ()
        |. chompIf isOk
        |. chompWhile isOk
        |> getChompedString


listOf : Parser a -> Parser (List a)
listOf item =
    let
        step : List a -> Parser (Step (List a) (List a))
        step revItems =
            oneOf
                [ item |> map (\e -> Loop (e :: revItems))
                , succeed () |> map (\_ -> Done (List.reverse revItems))
                ]
    in
    loop [] step


nonEmptyList : (a -> List a -> b) -> Parser a -> Parser b
nonEmptyList tag item =
    let
        finish items =
            case items of
                head :: tail ->
                    succeed (tag head tail)

                _ ->
                    problem "Expected at least one"
    in
    listOf item |> andThen finish


{-| Like Parser.spaces but doesn't allow new lines or carriage returns
-}
sameLineSpaces : Parser ()
sameLineSpaces =
    chompWhile ((==) ' ')
