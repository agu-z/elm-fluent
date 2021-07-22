module Fluent.Compile exposing (resource)

import Elm.CodeGen as G
import Fluent.Language
    exposing
        ( Entry(..)
        , EntryComment
        , Identifier(..)
        , MessageDefinition(..)
        , Pattern(..)
        , PatternElement(..)
        , Resource(..)
        )
import String.Case


resource : G.ModuleName -> Resource -> G.File
resource moduleName (Resource entries) =
    let
        ( ( _, expose ), declarations ) =
            entries
                |> List.map entry
                |> List.unzip
                |> Tuple.mapFirst G.combineLinkage
    in
    G.file (G.normalModule moduleName expose) [] declarations Nothing


entry : Entry -> ( G.Linkage, G.Declaration )
entry value =
    case value of
        Message comment (Identifier name) (ValueMessage patternValue []) ->
            let
                camelCasedName : String
                camelCasedName =
                    String.Case.toCamelCaseLower name
            in
            ( G.emptyLinkage |> G.addExposing (G.funExpose camelCasedName)
            , G.valDecl (entryComment comment)
                (Just G.stringAnn)
                camelCasedName
                (pattern patternValue)
            )

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

        _ ->
            Debug.todo "other elements"
