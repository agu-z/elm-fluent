module CompilerTests exposing (suite)

import Elm.Pretty
import Expect
import Fluent.Compile as Compile
import Fluent.Language as Parse
import Parser
import Test exposing (Test, test)


suite : Test
suite =
    test "compiles to Elm" <|
        \_ ->
            """

# Great the user
hi = Hola

# An email example.
#
# Make sure to display our markdown capabilities.
email-example = 
  Hi! Here's the roadmap for next year.

  January:
    - Login flow
    - Account creation 

  February:
    - Products page
    - Referrals
  
  Talk to you tomorrow!
"""
                |> Parser.run Parse.resource
                |> Result.map (Compile.resource [ "Es" ])
                |> Result.map (Elm.Pretty.pretty 2)
                |> Expect.equal (Ok <| String.trimLeft """
module Es exposing (emailExample, hi)


{-| Great the user


-}
hi :
    String
hi =
    "Hola"


{-| An email example.

Make sure to display our markdown capabilities.


-}
emailExample :
    String
emailExample =
    "Hi! Here's the roadmap for next year.\\n\\nJanuary:\\n  - Login flow\\n  - Account creation \\n\\nFebruary:\\n  - Products page\\n  - Referrals\\n\\nTalk to you tomorrow!"
""")
