open Expecto
open System
open System.IO

Expecto.Expect.defaultDiffPrinter <- Expecto.Diff.colourisedDiff

open TestsConfig

type Args =
    { RunOnlyFlaky: bool
      RunOnlyKnownFailure: bool }

let rec parseArgs (config, runArgs, args) =
    match args with
    | "--flaky" :: xs ->
        parseArgs ({ config with SkipFlaky = false }, { runArgs with RunOnlyFlaky = true }, xs)
    | "--known-failure" :: xs ->
        parseArgs ({ config with SkipKnownFailure = false }, { runArgs with RunOnlyKnownFailure = true }, xs)
    | xs ->
        config, runArgs, xs

[<EntryPoint>]
let main argv =

    let suiteConfig, runArgs, otherArgs =
        let defaultConfig = { SkipFlaky = true; SkipKnownFailure = true }
        let defaultRunArgs = { Args.RunOnlyFlaky = false; RunOnlyKnownFailure = false }
        parseArgs (defaultConfig, defaultRunArgs, List.ofArray argv)

    Environment.SetEnvironmentVariable("DOTNET_PROJ_INFO_MSBUILD_BL", "1")
    Environment.SetEnvironmentVariable("MSBuildExtensionsPath", null)

    let resultsPath = Path.Combine(__SOURCE_DIRECTORY__,"..","..","bin","test_results","Workspace.TestResults.xml")

    let writeResults = TestResults.writeNUnitSummary resultsPath
    let config = defaultConfig.appendSummaryHandler writeResults

    let tests =
        Tests.tests suiteConfig
        |> Test.filter "flaky tests" (fun s -> if runArgs.RunOnlyFlaky then List.contains "[flaky]" s else true)
        |> Test.filter "known failures" (fun s -> if runArgs.RunOnlyKnownFailure then List.contains "[known-failure]" s else true)

    Tests.runTestsWithArgs config (otherArgs |> Array.ofList) tests
