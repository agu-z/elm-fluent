module Fluent.Compile exposing (resource)

import Elm.CodeGen as G
import Elm.Pretty
import Fluent.Language
    exposing
        ( Entry(..)
        , EntryComment
        , Identifier(..)
        , InlineExpression(..)
        , Literal(..)
        , MessageDefinition(..)
        , Pattern(..)
        , PatternElement(..)
        , Resource(..)
        )
import Pretty
import String.Case


toString : Pretty.Doc -> String
toString =
    Pretty.pretty 120


resource : G.ModuleName -> Resource -> String
resource moduleName (Resource entries) =
    let
        { exposes, docs, content } =
            List.foldl foldEntry
                { exposes = []
                , docs = ""
                , content = ""
                }
                entries

        expose : String
        expose =
            exposes
                |> G.exposeExplicit
                |> Elm.Pretty.prettyExposing
                |> toString

        moduleDef : String
        moduleDef =
            "module " ++ String.join "." moduleName ++ " " ++ expose
    in
    moduleDef
        ++ "\n\n\n{-| "
        ++ String.trim docs
        ++ "\n-}\n\n\n"
        ++ String.trim content
        ++ "\n"


type alias ModuleState =
    { exposes : List G.TopLevelExpose
    , docs : String
    , content : String
    }


foldEntry :
    Entry
    -> ModuleState
    -> ModuleState
foldEntry value state =
    case value of
        Message comment (Identifier name) (ValueMessage patternValue []) ->
            let
                camelCasedName : String
                camelCasedName =
                    String.Case.toCamelCaseLower name

                declaration : G.Declaration
                declaration =
                    G.valDecl (entryComment comment)
                        (Just G.stringAnn)
                        camelCasedName
                        (pattern patternValue)
            in
            { state
                | exposes = G.funExpose camelCasedName :: state.exposes
                , content =
                    declaration
                        |> Elm.Pretty.prettyDeclaration 4
                        |> toString
                        |> (++) (state.content ++ "\n\n\n")
            }

        StandaloneComment comment ->
            { state
                | content =
                    comment
                        |> String.lines
                        |> List.map ((++) "-- ")
                        |> String.join "\n"
                        |> (++) (state.content ++ "\n\n\n")
            }

        GroupComment comment ->
            let
                lines : List String
                lines =
                    String.lines comment

                longestLine : Int
                longestLine =
                    lines
                        |> List.map String.length
                        |> List.maximum
                        |> Maybe.withDefault 0

                lineComment : String -> String
                lineComment text =
                    "-- "
                        ++ text
                        ++ String.repeat (longestLine - String.length text) " "
                        ++ " --"

                content : String
                content =
                    lines
                        |> List.map lineComment
                        |> String.join "\n"

                divisor : String
                divisor =
                    String.repeat (longestLine + 6) "-"
            in
            { state
                | content =
                    state.content
                        ++ "\n\n\n"
                        ++ divisor
                        ++ "\n"
                        ++ content
                        ++ "\n"
                        ++ divisor
            }

        ResourceComment comment ->
            { state | docs = state.docs ++ "\n\n" ++ comment }

        _ ->
            Debug.todo "other entries"


entryComment : EntryComment -> Maybe (G.Comment G.DocComment)
entryComment =
    Maybe.map (\value -> G.emptyDocComment |> G.markdown value)


pattern : Pattern -> G.Expression
pattern (Pattern first rest) =
    List.foldl append (patternElement first) rest


append : PatternElement -> G.Expression -> G.Expression
append b a =
    G.applyBinOp a G.append (patternElement b)


patternElement : PatternElement -> G.Expression
patternElement element =
    case element of
        TextElement text ->
            G.string text

        InlineExpression (Literal (Number value)) ->
            G.apply [ G.fun "formatNumber", G.float value ]

        InlineExpression expr ->
            inlineExpression expr

        _ ->
            Debug.todo "other elements"


inlineExpression : InlineExpression -> G.Expression
inlineExpression expr =
    case expr of
        Literal (Number value) ->
            G.float value

        Literal (String value) ->
            G.string value

        _ ->
            Debug.todo "other inline expressions"
