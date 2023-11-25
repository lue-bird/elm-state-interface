module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import EqualsCaseable
import MultipleAppendToConcat
import NoAlways
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDeprecated
import NoDuplicatePorts
import NoExposingEverything
import NoForbiddenWords
import NoFunctionOutsideOfModules
import NoImportAs
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoPrimitiveTypeAlias
import NoRecordAliasConstructor
import NoRecursiveUpdate
import NoSimpleLetBody
import NoSinglePatternCase
import NoUnnecessaryTrailingUnderscore
import NoUnoptimizedRecursion
import NoUnsafeDivision
import NoUnsafePorts
import NoUnsortedCases
import NoUnsortedLetDeclarations
import NoUnsortedTopLevelDeclarations
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import OnlyAllSingleUseTypeVarsEndWith_
import Review.Pattern.As
import Review.Pattern.Record
import Review.Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes
import ReviewPipelineStyles.Predicates
import Simplify
import UseCamelCase
import VariablesBetweenCaseOf.AccessInCases


config : List Rule
config =
    [ -- ## documentation
      Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }
    , Docs.UpToDateReadmeLinks.rule

    -- ## simplify
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Review.Rule.ignoreErrorsForFiles [ "src/N/Local.elm" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , Simplify.rule
        (Simplify.defaults |> Simplify.expectNaN)
    , NoSinglePatternCase.rule
        (NoSinglePatternCase.fixInArgument
            |> NoSinglePatternCase.ifAsPatternRequired
                (NoSinglePatternCase.fixInLetInstead
                    |> NoSinglePatternCase.andIfNoLetExists
                        NoSinglePatternCase.createNewLet
                )
        )
    , MultipleAppendToConcat.rule MultipleAppendToConcat.PipeRightList

    -- ## sort
    , NoUnsortedCases.rule
        (NoUnsortedCases.defaults
            |> -- i would want to sort by complexity last (number of arguments (+ their respective complexity))
               -- but such an ordering is not supported by the rule
               NoUnsortedCases.sortOnlyMatchingTypes (\_ _ -> False)
            |> NoUnsortedCases.doNotSortLiterals
            |> NoUnsortedCases.sortListPatternsByLength
        )
    , NoUnsortedTopLevelDeclarations.rule
        (NoUnsortedTopLevelDeclarations.sortTopLevelDeclarations
            |> NoUnsortedTopLevelDeclarations.glueHelpersAfter
            |> NoUnsortedTopLevelDeclarations.glueDependenciesBeforeFirstDependent
        )
    , NoUnsortedLetDeclarations.rule
        (NoUnsortedLetDeclarations.sortLetDeclarations
            |> NoUnsortedLetDeclarations.glueDependenciesBeforeFirstDependent
        )

    -- ## limit
    , [ ReviewPipelineStyles.rightPizzaPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.that
                (ReviewPipelineStyles.Predicates.haveAnyStepThatIs
                    ReviewPipelineStyles.Predicates.aConfusingNonCommutativeFunction
                )
            |> ReviewPipelineStyles.andCallThem
                "|> pipeline with confusing non-commutative function"
      , ReviewPipelineStyles.parentheticalApplicationPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.that
                (ReviewPipelineStyles.Predicates.haveAnyStepThatIs
                    ReviewPipelineStyles.Predicates.aConfusingNonCommutativePrefixOperator
                )
            |> ReviewPipelineStyles.andCallThem
                "parenthetical application with confusing non-commutative prefix operator"
      , ReviewPipelineStyles.leftPizzaPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andTryToFixThemBy
                ReviewPipelineStyles.Fixes.convertingToParentheticalApplication
            |> ReviewPipelineStyles.andReportCustomError
                "<| pipeline"
                [ "Forbidding `f <| a s` for reasons of simplicity, consistency:"
                , "  - Pipe data before the function: `food |> op ...`"
                , "  - Feed arguments after the function: `... |> opWith (a ...) (b ...)`"
                , "Use the application style `f (a s)` instead"
                ]
      , ReviewPipelineStyles.rightCompositionPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andReportCustomError
                ">> pipeline"
                [ "Avoid `g >> f` for easier to understand, more consistent code:"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
      , ReviewPipelineStyles.leftCompositionPipelines
            |> ReviewPipelineStyles.forbid
            |> ReviewPipelineStyles.andReportCustomError
                "<< pipeline"
                [ "Avoid `g << f` for easier to understand, more consistent code:"
                , "  - Keep the order data comes from before and gets piped through functions after: `... |> opF |> opG`"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
      ]
        |> ReviewPipelineStyles.rule
    , UseCamelCase.rule UseCamelCase.default
    , NoPrimitiveTypeAlias.rule
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , NoRecordAliasConstructor.rule
    , NoExposingEverything.rule
    , NoForbiddenWords.rule forbiddenWords
    , NoImportingEverything.rule []
    , NoImportAs.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoFunctionOutsideOfModules.rule
        [ ( forbiddenFunctionOrValues, [] ) ]
    , NoAlways.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests/" ]
    , VariablesBetweenCaseOf.AccessInCases.forbid
    , EqualsCaseable.forbid EqualsCaseable.Everywhere
    , NoDeprecated.rule NoDeprecated.defaults
    , NoPrematureLetComputation.rule
    , NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoRecursiveUpdate.rule
    , NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO"
        |> NoUnoptimizedRecursion.rule
    , NoSimpleLetBody.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnsafeDivision.rule
    , Review.Pattern.Record.forbid
    , Review.Pattern.As.forbid
    ]
        |> List.map (Review.Rule.ignoreErrorsForDirectories [ "VerifyExamples/" ])


forbiddenFunctionOrValues : List String
forbiddenFunctionOrValues =
    -- these should one day be fully fledged
    [ -- use tuple destructuring instead
      -- for improved descriptiveness
      "Tuple.first"
    , "Tuple.second"
    , -- use `mapFirst |> mapSecond` instead
      "Tuple.mapBoth"
    , -- use `String.indexes` instead
      "String.indices"
    , -- use a `case` instead
      "String.isEmpty"
    , "List.isEmpty"
    , "List.tail"

    -- use a `Set`, `Dict` or `List.sortWith`
    , "List.sort"
    , "List.sortBy"
    ]


forbiddenWords : List String
forbiddenWords =
    [ [ "REPLACEME", "FIXME", "REMOVEME", "CHECKME" ]
    , [ "TOREPLACE", "TOFIX", "TOREMOVE", "TOCHECK", "TODO" ]
    , [ "- []" ]
    ]
        |> List.concat
