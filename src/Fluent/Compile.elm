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
import Set exposing (Set)
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
        Message comment id (ValueMessage patternValue []) ->
            let
                elmName : String
                elmName =
                    idToString id

                elmComment =
                    entryComment comment

                ( valueExpression, references ) =
                    pattern patternValue

                declaration : G.Declaration
                declaration =
                    if Set.isEmpty references then
                        G.valDecl elmComment
                            (Just G.stringAnn)
                            elmName
                            valueExpression

                    else
                        let
                            refList : List String
                            refList =
                                Set.toList references

                            annotation : G.TypeAnnotation
                            annotation =
                                refList
                                    |> List.map (\name -> ( name, G.fqTyped [ "V" ] "Value" [] ))
                                    |> G.recordAnn
                                    |> (\arg -> G.funAnn arg G.stringAnn)

                            -- TODO: Cleanup
                        in
                        G.funDecl (entryComment comment)
                            (Just annotation)
                            elmName
                            [ G.recordPattern refList ]
                            valueExpression
            in
            { state
                | exposes = G.funExpose elmName :: state.exposes
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


type alias CompiledPattern =
    ( G.Expression, Set String )


pattern : Pattern -> CompiledPattern
pattern (Pattern first rest) =
    List.foldl appendPattern (patternElement first) rest


appendPattern : PatternElement -> CompiledPattern -> CompiledPattern
appendPattern b ( aValue, aRefs ) =
    let
        ( bValue, bRefs ) =
            patternElement b
    in
    ( G.applyBinOp aValue G.append bValue
    , Set.union aRefs bRefs
    )


patternElement : PatternElement -> CompiledPattern
patternElement element =
    case element of
        TextElement text ->
            ( G.string text, Set.empty )

        InlineExpression (Literal (Number value)) ->
            ( applyFormat <|
                G.parens (G.apply [ G.fqFun [ "V" ] "float", G.float value ])
            , Set.empty
            )

        InlineExpression expr ->
            inlineExpression expr

        _ ->
            Debug.todo "other elements"


applyFormat : G.Expression -> G.Expression
applyFormat value =
    G.apply [ G.fqFun [ "V" ] "format", value ]


idToString : Identifier -> String
idToString (Identifier name) =
    String.Case.toCamelCaseLower name


type alias References =
    Set String


inlineExpression : InlineExpression -> CompiledPattern
inlineExpression expr =
    case expr of
        Literal (Number value) ->
            ( G.float value, Set.empty )

        Literal (String value) ->
            ( G.string value, Set.empty )

        VariableReference id ->
            let
                elmName : String
                elmName =
                    idToString id
            in
            ( applyFormat (G.fun elmName)
            , Set.fromList [ elmName ]
            )

        _ ->
            Debug.todo "other inline expressions"
