module Tests.Parser exposing (..)

import Expect
import Fluent.Parser as P
import Parser
import Test exposing (..)


failsToParse : Parser.Parser a -> String -> Expect.Expectation
failsToParse parser =
    Parser.run parser >> Expect.err


expectErrorCode : String -> Result (List Parser.DeadEnd) a -> Expect.Expectation
expectErrorCode code result =
    case result of
        Ok _ ->
            Expect.fail "Result was Ok"

        Err errors ->
            let
                test { problem } =
                    case problem of
                        Parser.Problem message ->
                            String.startsWith code message

                        _ ->
                            False
            in
            if List.any test errors then
                Expect.pass

            else
                Expect.fail ("Result did not include error code: " ++ code)


suite : Test
suite =
    describe "Parser"
        [ describe "Entry"
            [ describe "Message"
                [ test "parses pattern" <|
                    \_ ->
                        "welcome-home = Welcome home, {$name}!"
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Message Nothing
                                        (P.Identifier "welcome-home")
                                        (P.ValueMessage
                                            (P.Pattern
                                                (P.TextElement "Welcome home, ")
                                                [ P.InlineExpression <|
                                                    P.VariableReference <|
                                                        P.Identifier "name"
                                                , P.TextElement "!"
                                                ]
                                            )
                                            []
                                        )
                                )
                , test "ignores spaces around equal sign" <|
                    \_ ->
                        "hi    =  Hi"
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Message Nothing
                                        (P.Identifier "hi")
                                        (P.ValueMessage
                                            (P.Pattern (P.TextElement "Hi") [])
                                            []
                                        )
                                )
                , test "supports attributes in addition to value" <|
                    \_ ->
                        "submit-button = Submit \n.aria-label = Submit form"
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Message Nothing
                                        (P.Identifier "submit-button")
                                        (P.ValueMessage
                                            (P.Pattern (P.TextElement "Submit") [])
                                            [ P.Attribute (P.Identifier "aria-label") (P.Pattern (P.TextElement "Submit form") []) ]
                                        )
                                )
                , test "supports standalone attributes" <|
                    \_ ->
                        "submit-button = \n  .aria-label = Submit form"
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Message Nothing
                                        (P.Identifier "submit-button")
                                        (P.AttributeMessage
                                            (P.Attribute (P.Identifier "aria-label") (P.Pattern (P.TextElement "Submit form") []))
                                            []
                                        )
                                )
                , test "fails if no value or attributes are provided" <|
                    \_ ->
                        "submit-button = \n \n"
                            |> Parser.run P.entry
                            |> expectErrorCode "E0005"
                ]
            , describe "Term"
                [ test "supports patterns" <|
                    \_ ->
                        "-brand-name = Fluent"
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Term Nothing
                                        (P.Identifier "brand-name")
                                        (P.Pattern (P.TextElement "Fluent") [])
                                        []
                                )
                , test "supports attributes" <|
                    \_ ->
                        "-company = Google\n.parent = Alphabet\n  .full = Alphabet Inc."
                            |> Parser.run P.entry
                            |> Expect.equal
                                (Ok <|
                                    P.Term Nothing
                                        (P.Identifier "company")
                                        (P.Pattern (P.TextElement "Google") [])
                                        [ P.Attribute (P.Identifier "parent")
                                            (P.Pattern (P.TextElement "Alphabet") [])
                                        , P.Attribute (P.Identifier "full")
                                            (P.Pattern (P.TextElement "Alphabet Inc.") [])
                                        ]
                                )
                , test "requires pattern even if attribute is provided" <|
                    \_ ->
                        "-company =\n.parent = Alphabet\n  .full = Alphabet Inc."
                            |> Parser.run P.entry
                            |> Expect.err
                ]
            , test "supports comments" <|
                \_ ->
                    "# Say hello to user\nhi = Hi"
                        |> Parser.run P.entry
                        |> Expect.equal
                            (Ok <|
                                P.Message (Just "Say hello to user")
                                    (P.Identifier "hi")
                                    (P.ValueMessage
                                        (P.Pattern (P.TextElement "Hi") [])
                                        []
                                    )
                            )
            , test "joins comment lines" <|
                \_ ->
                    "# Say hello to user\n# in a casual manner,\n#  without getting too formal\n-hi = Hi"
                        |> Parser.run P.entry
                        |> Expect.equal
                            (Ok <|
                                P.Term (Just "Say hello to user\nin a casual manner,\n without getting too formal")
                                    (P.Identifier "hi")
                                    (P.Pattern (P.TextElement "Hi") [])
                                    []
                            )
            , describe "ResourceComment"
                [ test "parses single line" <|
                    \_ ->
                        "### Resource Comment"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.ResourceComment "Resource Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "### The quick brown fox\n### jumped over the lazy dog"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.ResourceComment "The quick brown fox\njumped over the lazy dog")
                ]
            , describe "GroupComment"
                [ test "parses single line" <|
                    \_ ->
                        "## Group Comment"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.GroupComment "Group Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "## The quick brown fox\n## jumped over the lazy dog"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.GroupComment "The quick brown fox\njumped over the lazy dog")
                ]
            , describe "StandloneComment"
                [ test "parses single line" <|
                    \_ ->
                        "# Standalone Comment"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.StandaloneComment "Standalone Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "# The quick brown fox\n# jumped over the lazy dog"
                            |> Parser.run P.entry
                            |> Expect.equal (Ok <| P.StandaloneComment "The quick brown fox\njumped over the lazy dog")
                ]
            ]
        , describe "Pattern" <|
            let
                justText : String -> Result e P.Pattern
                justText value =
                    Ok <| P.Pattern (P.TextElement value) []
            in
            [ test "fails if empty" <|
                \_ ->
                    ""
                        |> failsToParse P.requiredPattern
            , test "fails if only whitespace" <|
                \_ ->
                    "\n\n  \n"
                        |> failsToParse P.requiredPattern
            , test "parses simple text" <|
                \_ ->
                    "Hello, World!"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "Hello, World!")
            , test "keeps newlines that are directly followed by another newline" <|
                \_ ->
                    "lorem\n\n\n ipsum"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "lorem\n\n\nipsum")
            , test "parses two lines" <|
                \_ ->
                    "lorem\n ipsum"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "lorem\nipsum")
            , test "parses multiple aligned lines" <|
                \_ ->
                    "lorem\n ipsum\n dolor"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "lorem\nipsum\ndolor")
            , test "ignore spaces and newlines at the end" <|
                \_ ->
                    "Hello, World! \n\n  "
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "Hello, World!")
            , test "ignores all newlines before the first element" <|
                \_ ->
                    "\n\n  \n lorem\n ipsum"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "lorem\nipsum")
            , test "ignore spaces in lines containing only spaces" <|
                \_ ->
                    "\n  lorem\n    \n  ipsum"
                        |> Parser.run P.requiredPattern
                        |> Expect.equal (justText "lorem\n\nipsum")
            , describe "Special"
                [ test "stops if there is no space after newline" <|
                    \_ ->
                        "lorem\nipsum"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "stops when it finds a closing brace after a newline" <|
                    \_ ->
                        "lorem\n }"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "stops when it finds a square bracket (variant) after a newline" <|
                    \_ ->
                        "lorem\n [1, 2, 3"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "supports square brackets in the middle of a line" <|
                    \_ ->
                        "\n I'm tired [sigh]."
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "I'm tired [sigh].")
                , test "supports starting with a square bracket on the first line" <|
                    \_ ->
                        "[1, 2, 3]" |> Parser.run P.requiredPattern |> Expect.equal (justText "[1, 2, 3]")
                , test "stops when it finds a star (default variant) after a newline" <|
                    \_ ->
                        "Shopping list:\n * Milk\n * Eggs"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "Shopping list:")
                , test "supports stars in the middle of a line" <|
                    \_ ->
                        "\n That was **amazing**."
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "That was **amazing**.")
                , test "supports starting with star on the first line" <|
                    \_ ->
                        "*magic*" |> Parser.run P.requiredPattern |> Expect.equal (justText "*magic*")
                , test "stops when it finds a period (attribute) after a newline" <|
                    \_ ->
                        "lorem\n .txt"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "supports periods in the middle of a line" <|
                    \_ ->
                        "\n Hi. How are you?"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "Hi. How are you?")
                , test "supports starting with period on the first line" <|
                    \_ ->
                        ".txt" |> Parser.run P.requiredPattern |> Expect.equal (justText ".txt")
                ]
            , describe "Indentation"
                [ test "starting in first line" <|
                    \_ ->
                        "lorem\n ipsum\n  dolor"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText "lorem\nipsum\n dolor")
                , test "starting in second line" <|
                    \_ ->
                        "\n    lorem\n    ipsum\n     dolor\n   sit"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal (justText " lorem\n ipsum\n  dolor\nsit")
                , test "with placeables" <|
                    \_ ->
                        "\n    You have\n      {$count}\n      messages"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    P.Pattern (P.TextElement "You have\n  ")
                                        [ P.InlineExpression (P.VariableReference (P.Identifier "count"))
                                        , P.TextElement "\n  messages"
                                        ]
                                )
                ]
            , describe "Placeable" <|
                [ test "parses inside braces" <|
                    \_ ->
                        "{$name}"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    P.Pattern
                                        (P.InlineExpression <|
                                            P.VariableReference <|
                                                P.Identifier "name"
                                        )
                                        []
                                )
                , test "surrounding spaces are allowed" <|
                    \_ ->
                        "{ $name   }"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    P.Pattern
                                        (P.InlineExpression <|
                                            P.VariableReference <|
                                                P.Identifier "name"
                                        )
                                        []
                                )
                , test "supports braces inside string literals" <|
                    \_ ->
                        "{\"{}\"}"
                            |> Parser.run P.requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    P.Pattern
                                        (P.InlineExpression <|
                                            P.Literal <|
                                                P.String "{}"
                                        )
                                        []
                                )
                , describe "SelectExpression"
                    [ test "parses" <|
                        \_ ->
                            "{$count ->\n  [zero] You have no messages\n  [one] You have a message\n  *[other] You have {$count} messages\n}"
                                |> Parser.run P.requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        P.Pattern
                                            (P.SelectExpression (P.VariableReference (P.Identifier "count"))
                                                [ P.Variant
                                                    (P.Identifier "zero")
                                                    (P.Pattern (P.TextElement "You have no messages") [])
                                                , P.Variant
                                                    (P.Identifier "one")
                                                    (P.Pattern (P.TextElement "You have a message") [])
                                                ]
                                                (P.Variant
                                                    (P.Identifier "other")
                                                    (P.Pattern
                                                        (P.TextElement "You have ")
                                                        [ P.InlineExpression (P.VariableReference (P.Identifier "count"))
                                                        , P.TextElement " messages"
                                                        ]
                                                    )
                                                )
                                                []
                                            )
                                            []
                                    )
                    , test "requires default variant" <|
                        \_ ->
                            "{$count ->\n  [zero] You have no messages\n  [one] You have a message\n}"
                                |> Parser.run P.requiredPattern
                                |> expectErrorCode "E0010"
                    , test "default variant doesn't have to be the last one" <|
                        \_ ->
                            "{$count ->\n  [zero] You have no messages\n  *[other] You have {$count} messages\n  [one] You have a message\n }"
                                |> Parser.run P.requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        P.Pattern
                                            (P.SelectExpression (P.VariableReference (P.Identifier "count"))
                                                [ P.Variant
                                                    (P.Identifier "zero")
                                                    (P.Pattern (P.TextElement "You have no messages") [])
                                                ]
                                                (P.Variant
                                                    (P.Identifier "other")
                                                    (P.Pattern
                                                        (P.TextElement "You have ")
                                                        [ P.InlineExpression (P.VariableReference (P.Identifier "count"))
                                                        , P.TextElement " messages"
                                                        ]
                                                    )
                                                )
                                                [ P.Variant
                                                    (P.Identifier "one")
                                                    (P.Pattern (P.TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                    )
                    , test "default variant can be the first one" <|
                        \_ ->
                            "{$count ->\n  *[other] You have {$count} messages\n  [one] You have a message\n  }"
                                |> Parser.run P.requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        P.Pattern
                                            (P.SelectExpression (P.VariableReference (P.Identifier "count"))
                                                []
                                                (P.Variant
                                                    (P.Identifier "other")
                                                    (P.Pattern
                                                        (P.TextElement "You have ")
                                                        [ P.InlineExpression (P.VariableReference (P.Identifier "count"))
                                                        , P.TextElement " messages"
                                                        ]
                                                    )
                                                )
                                                [ P.Variant
                                                    (P.Identifier "one")
                                                    (P.Pattern (P.TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                    )
                    , test "nested indentation" <|
                        \_ ->
                            "\n  {$count ->\n    *[other]\n      You have\n       {$count}\n      messages\n    [one]     You have a message\n }"
                                |> Parser.run P.requiredPattern
                                |> Expect.equal
                                    (Ok
                                        (P.Pattern
                                            (P.SelectExpression
                                                (P.VariableReference (P.Identifier "count"))
                                                []
                                                (P.Variant (P.Identifier "other")
                                                    (P.Pattern (P.TextElement "You have\n ")
                                                        [ P.InlineExpression (P.VariableReference (P.Identifier "count"))
                                                        , P.TextElement "\nmessages"
                                                        ]
                                                    )
                                                )
                                                [ P.Variant (P.Identifier "one")
                                                    (P.Pattern (P.TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                        )
                                    )
                    , test "select on a term's attribute" <|
                        \_ ->
                            "{-company.employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run P.requiredPattern
                                |> Expect.equal
                                    (Ok
                                        (P.Pattern
                                            (P.SelectExpression
                                                (P.TermReference
                                                    (P.Identifier "company")
                                                    (Just (P.Identifier "employees"))
                                                    P.noArguments
                                                )
                                                [ P.Variant
                                                    (P.Identifier "one")
                                                    (P.Pattern (P.TextElement "One employee") [])
                                                ]
                                                (P.Variant
                                                    (P.Identifier "other")
                                                    (P.Pattern
                                                        (P.InlineExpression (P.VariableReference (P.Identifier "count")))
                                                        [ P.TextElement " employees"
                                                        ]
                                                    )
                                                )
                                                []
                                            )
                                            []
                                        )
                                    )
                    , test "message references cannot be used as selectors" <|
                        \_ ->
                            "{employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run P.requiredPattern
                                |> expectErrorCode "E0016"
                    , test "terms cannot be used as selectors" <|
                        \_ ->
                            "{-employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run P.requiredPattern
                                |> expectErrorCode "E0017"
                    , test "attributes of messages cannot be used as selectors" <|
                        \_ ->
                            "{company.employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run P.requiredPattern
                                |> expectErrorCode "E0018"
                    ]
                ]
            ]
        , describe "Identifier"
            [ test "supports alpha" <|
                \_ ->
                    "hello"
                        |> Parser.run P.identifier
                        |> Expect.equal (Ok <| P.Identifier "hello")
            , test "supports alphanum" <|
                \_ ->
                    "h3llo"
                        |> Parser.run P.identifier
                        |> Expect.equal (Ok <| P.Identifier "h3llo")
            , test "supports dashes" <|
                \_ ->
                    "hello-world"
                        |> Parser.run P.identifier
                        |> Expect.equal (Ok <| P.Identifier "hello-world")
            , test "supports underscore" <|
                \_ ->
                    "hello_world"
                        |> Parser.run P.identifier
                        |> Expect.equal (Ok <| P.Identifier "hello_world")
            , test "cannot start with numbers" <|
                \_ ->
                    "1hello"
                        |> failsToParse P.identifier
            , test "cannot start with dash" <|
                \_ ->
                    "-hello"
                        |> failsToParse P.identifier
            , test "cannot start with underscore" <|
                \_ ->
                    "_hello"
                        |> failsToParse P.identifier
            ]
        , describe "Literal"
            [ describe "Number"
                [ test "supports positive integers" <|
                    \_ ->
                        "42"
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.Number 42.0)
                , test "supports positive floats" <|
                    \_ ->
                        "3.14"
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.Number 3.14)
                , test "supports negative integers" <|
                    \_ ->
                        "-8"
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.Number -8)
                , test "supports negative floats" <|
                    \_ ->
                        "-3.56"
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.Number -3.56)
                ]
            , describe "String"
                [ test "supports common characters" <|
                    \_ ->
                        "\"Hello, world!\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "Hello, world!")
                , test "supports braces" <|
                    \_ ->
                        "\"This is a brace: {\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "This is a brace: {")
                , test "supports escaped quotes" <|
                    \_ ->
                        "\"Hello \\\"John\\\"!\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "Hello \"John\"!")
                , test "supports escaped backslashes" <|
                    \_ ->
                        "\"C:\\\\ drive\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "C:\\ drive")
                , test "supports escaped 4-length unicode" <|
                    \_ ->
                        "\"\\u0048ello World\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "Hello World")
                , test "supports escaped 6-length unicode" <|
                    \_ ->
                        "\"\\U000048ello World\""
                            |> Parser.run P.literal
                            |> Expect.equal (Ok <| P.String "Hello World")
                , test "disallows out of range unicode" <|
                    \_ ->
                        "\"\\UFFFFFF\""
                            |> Parser.run P.literal
                            |> expectErrorCode "E0026"
                , test "disallows wrong length unicode" <|
                    \_ ->
                        "\"\\U00048X\""
                            |> Parser.run P.literal
                            |> expectErrorCode "E0026"
                , test "disallows line breaks" <|
                    \_ ->
                        "\"\\n\""
                            |> failsToParse P.literal
                , test "disallows carriage return" <|
                    \_ ->
                        "\"\\r\""
                            |> failsToParse P.literal
                ]
            ]
        , describe "InlineExpression"
            [ describe "Literal"
                [ test "supports numbers" <|
                    \_ ->
                        "-42.5"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal (Ok <| P.Literal <| P.Number -42.5)
                , test "supports strings" <|
                    \_ ->
                        "\"Hello, \\\"World\\\"\""
                            |> Parser.run P.inlineExpression
                            |> Expect.equal (Ok <| P.Literal <| P.String "Hello, \"World\"")
                ]
            , describe "FunctionReference" <|
                testCallArguments "FUNC" <|
                    P.FunctionReference <|
                        P.Identifier "FUNC"
            , describe "VariableReference"
                [ test "does not include $ in identifier" <|
                    \_ ->
                        "$name"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    P.VariableReference <|
                                        P.Identifier "name"
                                )
                , test "requires $" <|
                    \_ ->
                        "name"
                            |> failsToParse P.variableReference
                ]
            , describe "TermReference"
                ([ test "does not include - in identifier" <|
                    \_ ->
                        "-brand-name"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    P.TermReference (P.Identifier "brand-name")
                                        Nothing
                                        P.noArguments
                                )
                 , test "requires -" <|
                    \_ ->
                        "brand-name"
                            |> failsToParse P.variableReference
                 , test "supports attribute accessing" <|
                    \_ ->
                        "-brand.name"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    P.TermReference (P.Identifier "brand")
                                        (Just <| P.Identifier "name")
                                        P.noArguments
                                )
                 ]
                    ++ testCallArguments "-brand" (P.TermReference (P.Identifier "brand") Nothing)
                )
            , describe "MessageReference"
                [ test "supports identifier" <|
                    \_ ->
                        "brand-name"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    P.MessageReference (P.Identifier "brand-name") Nothing
                                )
                , test "supports attribute accessing" <|
                    \_ ->
                        "brand.name"
                            |> Parser.run P.inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    P.MessageReference (P.Identifier "brand")
                                        (Just <| P.Identifier "name")
                                )
                ]
            ]
        ]


testCallArguments : String -> (P.CallArguments -> P.InlineExpression) -> List Test
testCallArguments prefix expecting =
    [ test "supports an empty arguments list" <|
        \_ ->
            prefix
                ++ "()"
                |> Parser.run P.inlineExpression
                |> Expect.equal (Ok <| expecting P.noArguments)
    , test "supports positional arguments" <|
        \_ ->
            prefix
                ++ "(2, \"cats\")"
                |> Parser.run P.inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional =
                                [ P.Literal <| P.Number 2
                                , P.Literal <| P.String "cats"
                                ]
                            , named =
                                []
                            }
                    )
    , test "supports named arguments" <|
        \_ ->
            prefix
                ++ "(count: 2, label:\"cats\")"
                |> Parser.run P.inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional = []
                            , named =
                                [ ( P.Identifier "count", P.Number 2 )
                                , ( P.Identifier "label", P.String "cats" )
                                ]
                            }
                    )
    , test "supports a combination of positional and named arguments" <|
        \_ ->
            prefix
                ++ "(2, y: 3, label:\"cats\")"
                |> Parser.run P.inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional = [ P.Literal <| P.Number 2 ]
                            , named =
                                [ ( P.Identifier "y", P.Number 3 )
                                , ( P.Identifier "label", P.String "cats" )
                                ]
                            }
                    )
    , test "disallows positional arguments with message reference" <|
        \_ ->
            prefix
                ++ "(label: hi)"
                |> failsToParse P.inlineExpression
    , test "disallows positional argument after named argument" <|
        \_ ->
            prefix
                ++ "(2, label:\"cats\", 4)"
                |> Parser.run P.inlineExpression
                |> expectErrorCode "E0021"
    ]
