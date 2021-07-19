module ParserTests exposing (..)

import Expect
import Fluent.Parser exposing (..)
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


testResource : String
testResource =
    """
## Closing tabs

tabs-close-button = Close
tabs-close-tooltip = {$tabCount ->
    [one] Close {$tabCount} tab
   *[other] Close {$tabCount} tabs
}
tabs-close-warning =
    You are about to close {$tabCount} tabs.
    Are you sure you want to continue?

## Syncing

-sync-brand-name = Firefox Account

sync-dialog-title = {-sync-brand-name}
sync-headline-title =
    {-sync-brand-name}: The best way to bring
    your data always with you
sync-signedout-title =
    Connect with your {-sync-brand-name}
"""


suite : Test
suite =
    describe "Parser"
        [ describe "Resource" <|
            [ test "parses all entries" <|
                \_ ->
                    testResource
                        |> Parser.run resource
                        |> Expect.equal
                            (Ok <|
                                Resource
                                    [ GroupComment "Closing tabs"
                                    , Message Nothing
                                        (Identifier "tabs-close-button")
                                        (ValueMessage (Pattern (TextElement "Close") []) [])
                                    , Message Nothing
                                        (Identifier "tabs-close-tooltip")
                                        (ValueMessage
                                            (Pattern
                                                (SelectExpression
                                                    (VariableReference (Identifier "tabCount"))
                                                    [ Variant (Identifier "one")
                                                        (Pattern (TextElement "Close ")
                                                            [ InlineExpression (VariableReference (Identifier "tabCount"))
                                                            , TextElement " tab"
                                                            ]
                                                        )
                                                    ]
                                                    (Variant (Identifier "other")
                                                        (Pattern (TextElement "Close ")
                                                            [ InlineExpression (VariableReference (Identifier "tabCount"))
                                                            , TextElement " tabs"
                                                            ]
                                                        )
                                                    )
                                                    []
                                                )
                                                []
                                            )
                                            []
                                        )
                                    , Message Nothing
                                        (Identifier "tabs-close-warning")
                                        (ValueMessage
                                            (Pattern (TextElement "You are about to close ")
                                                [ InlineExpression (VariableReference (Identifier "tabCount"))
                                                , TextElement " tabs.\nAre you sure you want to continue?"
                                                ]
                                            )
                                            []
                                        )
                                    , GroupComment "Syncing"
                                    , Term Nothing
                                        (Identifier "sync-brand-name")
                                        (Pattern (TextElement "Firefox Account") [])
                                        []
                                    , Message Nothing
                                        (Identifier "sync-dialog-title")
                                        (ValueMessage
                                            (Pattern
                                                (InlineExpression
                                                    (TermReference (Identifier "sync-brand-name")
                                                        Nothing
                                                        noArguments
                                                    )
                                                )
                                                []
                                            )
                                            []
                                        )
                                    , Message Nothing
                                        (Identifier "sync-headline-title")
                                        (ValueMessage
                                            (Pattern
                                                (InlineExpression
                                                    (TermReference (Identifier "sync-brand-name")
                                                        Nothing
                                                        noArguments
                                                    )
                                                )
                                                [ TextElement ": The best way to bring\nyour data always with you" ]
                                            )
                                            []
                                        )
                                    , Message Nothing
                                        (Identifier "sync-signedout-title")
                                        (ValueMessage
                                            (Pattern (TextElement "Connect with your ")
                                                [ InlineExpression (TermReference (Identifier "sync-brand-name") Nothing { named = [], positional = [] })
                                                ]
                                            )
                                            []
                                        )
                                    ]
                            )
            ]
        , describe "Entry"
            [ describe "Message"
                [ test "parses pattern" <|
                    \_ ->
                        "welcome-home = Welcome home, {$name}!"
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Message Nothing
                                        (Identifier "welcome-home")
                                        (ValueMessage
                                            (Pattern
                                                (TextElement "Welcome home, ")
                                                [ InlineExpression <|
                                                    VariableReference <|
                                                        Identifier "name"
                                                , TextElement "!"
                                                ]
                                            )
                                            []
                                        )
                                )
                , test "ignores spaces around equal sign" <|
                    \_ ->
                        "hi    =  Hi"
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Message Nothing
                                        (Identifier "hi")
                                        (ValueMessage
                                            (Pattern (TextElement "Hi") [])
                                            []
                                        )
                                )
                , test "supports attributes in addition to value" <|
                    \_ ->
                        "submit-button = Submit \n.aria-label = Submit form"
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Message Nothing
                                        (Identifier "submit-button")
                                        (ValueMessage
                                            (Pattern (TextElement "Submit") [])
                                            [ Attribute (Identifier "aria-label") (Pattern (TextElement "Submit form") []) ]
                                        )
                                )
                , test "supports standalone attributes" <|
                    \_ ->
                        "submit-button = \n  .aria-label = Submit form"
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Message Nothing
                                        (Identifier "submit-button")
                                        (AttributeMessage
                                            (Attribute (Identifier "aria-label") (Pattern (TextElement "Submit form") []))
                                            []
                                        )
                                )
                , test "fails if no value or attributes are provided" <|
                    \_ ->
                        "submit-button = \n \n"
                            |> Parser.run entry
                            |> expectErrorCode "E0005"
                ]
            , describe "Term"
                [ test "supports patterns" <|
                    \_ ->
                        "-brand-name = Fluent"
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Term Nothing
                                        (Identifier "brand-name")
                                        (Pattern (TextElement "Fluent") [])
                                        []
                                )
                , test "supports attributes" <|
                    \_ ->
                        "-company = Google\n.parent = Alphabet\n  .full = Alphabet Inc."
                            |> Parser.run entry
                            |> Expect.equal
                                (Ok <|
                                    Term Nothing
                                        (Identifier "company")
                                        (Pattern (TextElement "Google") [])
                                        [ Attribute (Identifier "parent")
                                            (Pattern (TextElement "Alphabet") [])
                                        , Attribute (Identifier "full")
                                            (Pattern (TextElement "Alphabet Inc.") [])
                                        ]
                                )
                , test "requires pattern even if attribute is provided" <|
                    \_ ->
                        "-company =\n.parent = Alphabet\n  .full = Alphabet Inc."
                            |> Parser.run entry
                            |> Expect.err
                ]
            , test "supports comments" <|
                \_ ->
                    "# Say hello to user\nhi = Hi"
                        |> Parser.run entry
                        |> Expect.equal
                            (Ok <|
                                Message (Just "Say hello to user")
                                    (Identifier "hi")
                                    (ValueMessage
                                        (Pattern (TextElement "Hi") [])
                                        []
                                    )
                            )
            , test "joins comment lines" <|
                \_ ->
                    "# Say hello to user\n# in a casual manner,\n#  without getting too formal\n-hi = Hi"
                        |> Parser.run entry
                        |> Expect.equal
                            (Ok <|
                                Term (Just "Say hello to user\nin a casual manner,\n without getting too formal")
                                    (Identifier "hi")
                                    (Pattern (TextElement "Hi") [])
                                    []
                            )
            , describe "ResourceComment"
                [ test "parses single line" <|
                    \_ ->
                        "### Resource Comment"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| ResourceComment "Resource Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "### The quick brown fox\n### jumped over the lazy dog"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| ResourceComment "The quick brown fox\njumped over the lazy dog")
                ]
            , describe "GroupComment"
                [ test "parses single line" <|
                    \_ ->
                        "## Group Comment"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| GroupComment "Group Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "## The quick brown fox\n## jumped over the lazy dog"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| GroupComment "The quick brown fox\njumped over the lazy dog")
                ]
            , describe "StandloneComment"
                [ test "parses single line" <|
                    \_ ->
                        "# Standalone Comment"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| StandaloneComment "Standalone Comment")
                , test "parses multiple lines" <|
                    \_ ->
                        "# The quick brown fox\n# jumped over the lazy dog"
                            |> Parser.run entry
                            |> Expect.equal (Ok <| StandaloneComment "The quick brown fox\njumped over the lazy dog")
                ]
            ]
        , describe "Pattern" <|
            let
                justText : String -> Result e Pattern
                justText value =
                    Ok <| Pattern (TextElement value) []
            in
            [ test "fails if empty" <|
                \_ ->
                    ""
                        |> failsToParse requiredPattern
            , test "fails if only whitespace" <|
                \_ ->
                    "\n\n  \n"
                        |> failsToParse requiredPattern
            , test "parses simple text" <|
                \_ ->
                    "Hello, World!"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "Hello, World!")
            , test "keeps newlines that are directly followed by another newline" <|
                \_ ->
                    "lorem\n\n\n ipsum"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "lorem\n\n\nipsum")
            , test "parses two lines" <|
                \_ ->
                    "lorem\n ipsum"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "lorem\nipsum")
            , test "parses multiple aligned lines" <|
                \_ ->
                    "lorem\n ipsum\n dolor"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "lorem\nipsum\ndolor")
            , test "ignore spaces and newlines at the end" <|
                \_ ->
                    "Hello, World! \n\n  "
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "Hello, World!")
            , test "ignores all newlines before the first element" <|
                \_ ->
                    "\n\n  \n lorem\n ipsum"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "lorem\nipsum")
            , test "ignore spaces in lines containing only spaces" <|
                \_ ->
                    "\n  lorem\n    \n  ipsum"
                        |> Parser.run requiredPattern
                        |> Expect.equal (justText "lorem\n\nipsum")
            , describe "Special"
                [ test "stops if there is no space after newline" <|
                    \_ ->
                        "lorem\nipsum"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "stops when it finds a closing brace after a newline" <|
                    \_ ->
                        "lorem\n }"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "stops when it finds a square bracket (variant) after a newline" <|
                    \_ ->
                        "lorem\n [1, 2, 3"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "supports square brackets in the middle of a line" <|
                    \_ ->
                        "\n I'm tired [sigh]."
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "I'm tired [sigh].")
                , test "supports starting with a square bracket on the first line" <|
                    \_ ->
                        "[1, 2, 3]" |> Parser.run requiredPattern |> Expect.equal (justText "[1, 2, 3]")
                , test "stops when it finds a star (default variant) after a newline" <|
                    \_ ->
                        "Shopping list:\n * Milk\n * Eggs"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "Shopping list:")
                , test "supports stars in the middle of a line" <|
                    \_ ->
                        "\n That was **amazing**."
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "That was **amazing**.")
                , test "supports starting with star on the first line" <|
                    \_ ->
                        "*magic*" |> Parser.run requiredPattern |> Expect.equal (justText "*magic*")
                , test "stops when it finds a period (attribute) after a newline" <|
                    \_ ->
                        "lorem\n .txt"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "lorem")
                , test "supports periods in the middle of a line" <|
                    \_ ->
                        "\n Hi. How are you?"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "Hi. How are you?")
                , test "supports starting with period on the first line" <|
                    \_ ->
                        ".txt" |> Parser.run requiredPattern |> Expect.equal (justText ".txt")
                ]
            , describe "Indentation"
                [ test "starting in first line" <|
                    \_ ->
                        "lorem\n ipsum\n  dolor"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText "lorem\nipsum\n dolor")
                , test "starting in second line" <|
                    \_ ->
                        "\n    lorem\n    ipsum\n     dolor\n   sit"
                            |> Parser.run requiredPattern
                            |> Expect.equal (justText " lorem\n ipsum\n  dolor\nsit")
                , test "with text following placeables in the same line" <|
                    \_ ->
                        "\n    You have\n      {$count} messages.\n    Read them here."
                            |> Parser.run requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    Pattern (TextElement "You have\n  ")
                                        [ InlineExpression (VariableReference (Identifier "count"))
                                        , TextElement " messages.\nRead them here."
                                        ]
                                )
                , test "with text following placeables in the next line" <|
                    \_ ->
                        "\n    You have\n      {$count}\n      messages"
                            |> Parser.run requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    Pattern (TextElement "You have\n  ")
                                        [ InlineExpression (VariableReference (Identifier "count"))
                                        , TextElement "\n  messages"
                                        ]
                                )
                ]
            , describe "Placeable" <|
                [ test "parses inside braces" <|
                    \_ ->
                        "{$name}"
                            |> Parser.run requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    Pattern
                                        (InlineExpression <|
                                            VariableReference <|
                                                Identifier "name"
                                        )
                                        []
                                )
                , test "surrounding spaces are allowed" <|
                    \_ ->
                        "{ $name   }"
                            |> Parser.run requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    Pattern
                                        (InlineExpression <|
                                            VariableReference <|
                                                Identifier "name"
                                        )
                                        []
                                )
                , test "supports braces inside string literals" <|
                    \_ ->
                        "{\"{}\"}"
                            |> Parser.run requiredPattern
                            |> Expect.equal
                                (Ok <|
                                    Pattern
                                        (InlineExpression <|
                                            Literal <|
                                                String "{}"
                                        )
                                        []
                                )
                , describe "SelectExpression"
                    [ test "parses" <|
                        \_ ->
                            "{$count ->\n  [zero] You have no messages\n  [one] You have a message\n  *[other] You have {$count} messages\n}"
                                |> Parser.run requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        Pattern
                                            (SelectExpression (VariableReference (Identifier "count"))
                                                [ Variant
                                                    (Identifier "zero")
                                                    (Pattern (TextElement "You have no messages") [])
                                                , Variant
                                                    (Identifier "one")
                                                    (Pattern (TextElement "You have a message") [])
                                                ]
                                                (Variant
                                                    (Identifier "other")
                                                    (Pattern
                                                        (TextElement "You have ")
                                                        [ InlineExpression (VariableReference (Identifier "count"))
                                                        , TextElement " messages"
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
                                |> Parser.run requiredPattern
                                |> expectErrorCode "E0010"
                    , test "default variant doesn't have to be the last one" <|
                        \_ ->
                            "{$count ->\n  [zero] You have no messages\n  *[other] You have {$count} messages\n  [one] You have a message\n }"
                                |> Parser.run requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        Pattern
                                            (SelectExpression (VariableReference (Identifier "count"))
                                                [ Variant
                                                    (Identifier "zero")
                                                    (Pattern (TextElement "You have no messages") [])
                                                ]
                                                (Variant
                                                    (Identifier "other")
                                                    (Pattern
                                                        (TextElement "You have ")
                                                        [ InlineExpression (VariableReference (Identifier "count"))
                                                        , TextElement " messages"
                                                        ]
                                                    )
                                                )
                                                [ Variant
                                                    (Identifier "one")
                                                    (Pattern (TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                    )
                    , test "default variant can be the first one" <|
                        \_ ->
                            "{$count ->\n  *[other] You have {$count} messages\n  [one] You have a message\n  }"
                                |> Parser.run requiredPattern
                                |> Expect.equal
                                    (Ok <|
                                        Pattern
                                            (SelectExpression (VariableReference (Identifier "count"))
                                                []
                                                (Variant
                                                    (Identifier "other")
                                                    (Pattern
                                                        (TextElement "You have ")
                                                        [ InlineExpression (VariableReference (Identifier "count"))
                                                        , TextElement " messages"
                                                        ]
                                                    )
                                                )
                                                [ Variant
                                                    (Identifier "one")
                                                    (Pattern (TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                    )
                    , test "nested indentation" <|
                        \_ ->
                            "\n  {$count ->\n    *[other]\n      You have\n       {$count}\n      messages\n    [one]     You have a message\n }"
                                |> Parser.run requiredPattern
                                |> Expect.equal
                                    (Ok
                                        (Pattern
                                            (SelectExpression
                                                (VariableReference (Identifier "count"))
                                                []
                                                (Variant (Identifier "other")
                                                    (Pattern (TextElement "You have\n ")
                                                        [ InlineExpression (VariableReference (Identifier "count"))
                                                        , TextElement "\nmessages"
                                                        ]
                                                    )
                                                )
                                                [ Variant (Identifier "one")
                                                    (Pattern (TextElement "You have a message") [])
                                                ]
                                            )
                                            []
                                        )
                                    )
                    , test "select on a term's attribute" <|
                        \_ ->
                            "{-company.employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run requiredPattern
                                |> Expect.equal
                                    (Ok
                                        (Pattern
                                            (SelectExpression
                                                (TermReference
                                                    (Identifier "company")
                                                    (Just (Identifier "employees"))
                                                    noArguments
                                                )
                                                [ Variant
                                                    (Identifier "one")
                                                    (Pattern (TextElement "One employee") [])
                                                ]
                                                (Variant
                                                    (Identifier "other")
                                                    (Pattern
                                                        (InlineExpression (VariableReference (Identifier "count")))
                                                        [ TextElement " employees"
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
                                |> Parser.run requiredPattern
                                |> expectErrorCode "E0016"
                    , test "terms cannot be used as selectors" <|
                        \_ ->
                            "{-employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run requiredPattern
                                |> expectErrorCode "E0017"
                    , test "attributes of messages cannot be used as selectors" <|
                        \_ ->
                            "{company.employees ->\n [one] One employee\n *[other] {$count} employees\n }"
                                |> Parser.run requiredPattern
                                |> expectErrorCode "E0018"
                    ]
                ]
            ]
        , describe "Identifier"
            [ test "supports alpha" <|
                \_ ->
                    "hello"
                        |> Parser.run identifier
                        |> Expect.equal (Ok <| Identifier "hello")
            , test "supports alphanum" <|
                \_ ->
                    "h3llo"
                        |> Parser.run identifier
                        |> Expect.equal (Ok <| Identifier "h3llo")
            , test "supports dashes" <|
                \_ ->
                    "hello-world"
                        |> Parser.run identifier
                        |> Expect.equal (Ok <| Identifier "hello-world")
            , test "supports underscore" <|
                \_ ->
                    "hello_world"
                        |> Parser.run identifier
                        |> Expect.equal (Ok <| Identifier "hello_world")
            , test "cannot start with numbers" <|
                \_ ->
                    "1hello"
                        |> failsToParse identifier
            , test "cannot start with dash" <|
                \_ ->
                    "-hello"
                        |> failsToParse identifier
            , test "cannot start with underscore" <|
                \_ ->
                    "_hello"
                        |> failsToParse identifier
            ]
        , describe "Literal"
            [ describe "Number"
                [ test "supports positive integers" <|
                    \_ ->
                        "42"
                            |> Parser.run literal
                            |> Expect.equal (Ok <| Number 42.0)
                , test "supports positive floats" <|
                    \_ ->
                        "3.14"
                            |> Parser.run literal
                            |> Expect.equal (Ok <| Number 3.14)
                , test "supports negative integers" <|
                    \_ ->
                        "-8"
                            |> Parser.run literal
                            |> Expect.equal (Ok <| Number -8)
                , test "supports negative floats" <|
                    \_ ->
                        "-3.56"
                            |> Parser.run literal
                            |> Expect.equal (Ok <| Number -3.56)
                ]
            , describe "String"
                [ test "supports common characters" <|
                    \_ ->
                        "\"Hello, world!\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "Hello, world!")
                , test "supports braces" <|
                    \_ ->
                        "\"This is a brace: {\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "This is a brace: {")
                , test "supports escaped quotes" <|
                    \_ ->
                        "\"Hello \\\"John\\\"!\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "Hello \"John\"!")
                , test "supports escaped backslashes" <|
                    \_ ->
                        "\"C:\\\\ drive\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "C:\\ drive")
                , test "supports escaped 4-length unicode" <|
                    \_ ->
                        "\"\\u0048ello World\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "Hello World")
                , test "supports escaped 6-length unicode" <|
                    \_ ->
                        "\"\\U000048ello World\""
                            |> Parser.run literal
                            |> Expect.equal (Ok <| String "Hello World")
                , test "disallows out of range unicode" <|
                    \_ ->
                        "\"\\UFFFFFF\""
                            |> Parser.run literal
                            |> expectErrorCode "E0026"
                , test "disallows wrong length unicode" <|
                    \_ ->
                        "\"\\U00048X\""
                            |> Parser.run literal
                            |> expectErrorCode "E0026"
                , test "disallows line breaks" <|
                    \_ ->
                        "\"\\n\""
                            |> failsToParse literal
                , test "disallows carriage return" <|
                    \_ ->
                        "\"\\r\""
                            |> failsToParse literal
                ]
            ]
        , describe "InlineExpression"
            [ describe "Literal"
                [ test "supports numbers" <|
                    \_ ->
                        "-42.5"
                            |> Parser.run inlineExpression
                            |> Expect.equal (Ok <| Literal <| Number -42.5)
                , test "supports strings" <|
                    \_ ->
                        "\"Hello, \\\"World\\\"\""
                            |> Parser.run inlineExpression
                            |> Expect.equal (Ok <| Literal <| String "Hello, \"World\"")
                ]
            , describe "FunctionReference" <|
                testCallArguments "FUNC" <|
                    FunctionReference <|
                        Identifier "FUNC"
            , describe "VariableReference"
                [ test "does not include $ in identifier" <|
                    \_ ->
                        "$name"
                            |> Parser.run inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    VariableReference <|
                                        Identifier "name"
                                )
                , test "requires $" <|
                    \_ ->
                        "name"
                            |> failsToParse variableReference
                ]
            , describe "TermReference"
                ([ test "does not include - in identifier" <|
                    \_ ->
                        "-brand-name"
                            |> Parser.run inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    TermReference (Identifier "brand-name")
                                        Nothing
                                        noArguments
                                )
                 , test "requires -" <|
                    \_ ->
                        "brand-name"
                            |> failsToParse variableReference
                 , test "supports attribute accessing" <|
                    \_ ->
                        "-brand.name"
                            |> Parser.run inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    TermReference (Identifier "brand")
                                        (Just <| Identifier "name")
                                        noArguments
                                )
                 ]
                    ++ testCallArguments "-brand" (TermReference (Identifier "brand") Nothing)
                )
            , describe "MessageReference"
                [ test "supports identifier" <|
                    \_ ->
                        "brand-name"
                            |> Parser.run inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    MessageReference (Identifier "brand-name") Nothing
                                )
                , test "supports attribute accessing" <|
                    \_ ->
                        "brand.name"
                            |> Parser.run inlineExpression
                            |> Expect.equal
                                (Ok <|
                                    MessageReference (Identifier "brand")
                                        (Just <| Identifier "name")
                                )
                ]
            ]
        ]


testCallArguments : String -> (CallArguments -> InlineExpression) -> List Test
testCallArguments prefix expecting =
    [ test "supports an empty arguments list" <|
        \_ ->
            prefix
                ++ "()"
                |> Parser.run inlineExpression
                |> Expect.equal (Ok <| expecting noArguments)
    , test "supports positional arguments" <|
        \_ ->
            prefix
                ++ "(2, \"cats\")"
                |> Parser.run inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional =
                                [ Literal <| Number 2
                                , Literal <| String "cats"
                                ]
                            , named =
                                []
                            }
                    )
    , test "supports named arguments" <|
        \_ ->
            prefix
                ++ "(count: 2, label:\"cats\")"
                |> Parser.run inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional = []
                            , named =
                                [ ( Identifier "count", Number 2 )
                                , ( Identifier "label", String "cats" )
                                ]
                            }
                    )
    , test "supports a combination of positional and named arguments" <|
        \_ ->
            prefix
                ++ "(2, y: 3, label:\"cats\")"
                |> Parser.run inlineExpression
                |> Expect.equal
                    (Ok <|
                        expecting
                            { positional = [ Literal <| Number 2 ]
                            , named =
                                [ ( Identifier "y", Number 3 )
                                , ( Identifier "label", String "cats" )
                                ]
                            }
                    )
    , test "disallows positional arguments with message reference" <|
        \_ ->
            prefix
                ++ "(label: hi)"
                |> failsToParse inlineExpression
    , test "disallows positional argument after named argument" <|
        \_ ->
            prefix
                ++ "(2, label:\"cats\", 4)"
                |> Parser.run inlineExpression
                |> expectErrorCode "E0021"
    ]
