module CompilerTests exposing (suite)

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
### Workflow translations

welcome = Welcome, {$name}!

new-task = 
  { $priority -> 
      [high] High priority. Drop what you're doing and address this.
      [medium] Medium priority. Address Today. Hard deadline: {$deadline}.
      *[low] Address before {$deadline}.
  }

## Tabs

# Incoming emails section
inbox = Inbox

# Outgoing emails section
outbox = Outbox ({120})

## Feature Demo
## Examples of different kinds of email

# Make sure to display multi-line messages with white-space: pre

# An email example.
#
# Make sure to display our markdown capabilities.
roadmap-email-example = 
  Hi! Here's the roadmap for next year.

  January:
    - Login flow
    - Account creation 

  February:
    - Products page
    - Referrals
  
  Talk to you tomorrow!

## Shortcuts

go-back = 
  Use Cmd+{"{"} to go back


"""
                |> Parser.run Parse.resource
                |> Result.map (Compile.resource [ "Es" ])
                |> Expect.equal (Ok <| String.trimLeft """
module Es exposing (goBack, inbox, newTask, outbox, roadmapEmailExample, welcome)


{-| Workflow translations
-}


welcome : { name : String } -> String
welcome { name } =
    "Welcome, " ++ name ++ "!"


newTask : { deadline : String, priority : String } -> String
newTask { deadline, priority } =
    case priority of
        "high" ->
            "High priority. Drop what you're doing and address this."

        "medium" ->
            "Medium priority. Address Today. Hard deadline: " ++ deadline ++ "."

        _ ->
            "Address before " ++ deadline ++ "."


----------
-- Tabs --
----------


{-| Incoming emails section


-}
inbox : String
inbox =
    "Inbox"


{-| Outgoing emails section


-}
outbox : String
outbox =
    "Outbox (" ++ V.format (V.float 120) ++ ")"


------------------------------------------
-- Feature Demo                         --
-- Examples of different kinds of email --
------------------------------------------


-- Make sure to display multi-line messages with white-space: pre


{-| An email example.

Make sure to display our markdown capabilities.


-}
roadmapEmailExample : String
roadmapEmailExample =
    "Hi! Here's the roadmap for next year.\\n\\nJanuary:\\n  - Login flow\\n  - Account creation \\n\\nFebruary:\\n  - Products page\\n  - Referrals\\n\\nTalk to you tomorrow!"


---------------
-- Shortcuts --
---------------


goBack : String
goBack =
    "Use Cmd+" ++ "{" ++ " to go back"
""")
