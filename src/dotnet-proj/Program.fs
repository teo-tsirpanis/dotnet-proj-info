open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text.Json
open Argu
open Arguments
open Microsoft.Build.Framework
open Microsoft.Build.Locator
open Railway
open Dotnet.ProjInfo.MSBuild
open Dotnet.ProjInfo.Inspect

type ShellCommandResult = ShellCommandResult of workingDir: string * exePath: string * args: string * output: seq<bool*string>

type Errors =
    | InvalidArgs of ArguParseException
    | InvalidArgsState of string
    | ProjectFileNotFound of string
    | GenericError of string
    | RaisedException of exn * string
    | EvaluationError of exn option
    | ExecutionError of GetProjectInfoErrors<ShellCommandResult>

let parseArgsCommandLine argv =
    try
        let parser = ArgumentParser.Create<CLIArguments>(programName = "dotnet-proj")
        let results = parser.Parse argv
        Ok results
    with
    | :? ArguParseException as ex ->
        Error (InvalidArgs ex)

let runCmd log workingDir exePath args =
    log (sprintf "running '%s %s'" exePath (args |> String.concat " "))

    let logOutput = ConcurrentQueue<bool*string>()

    let exitCode =
        let psi = ProcessStartInfo()
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        Seq.iter psi.ArgumentList.Add args
        psi.CreateNoWindow <- true
        psi.UseShellExecute <- false

        //Some env var like `MSBUILD_EXE_PATH` override the msbuild used.
        //The dotnet cli (`dotnet`) set these when calling child processes, and
        //is wrong because these override some properties of the called msbuild
        let msbuildEnvVars =
            psi.Environment.Keys
            |> Seq.filter (fun s -> s.StartsWith("msbuild", StringComparison.OrdinalIgnoreCase))
            |> Seq.toList
        for msbuildEnvVar in msbuildEnvVars do
            psi.Environment.Remove(msbuildEnvVar) |> ignore

        use p = new Process()
        p.StartInfo <- psi

        p.OutputDataReceived.Add(fun ea -> logOutput.Enqueue (false, ea.Data))

        p.ErrorDataReceived.Add(fun ea -> logOutput.Enqueue (true, ea.Data))

        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        p.ExitCode

    let error, output =
        logOutput
        |> List.ofSeq
        |> List.partition fst

    log "output:"
    List.iter (snd >> log) output

    log "error:"
    List.iter (snd >> log) error

    exitCode, (ShellCommandResult (workingDir, exePath, String.concat " " args, output))

let validateProj log projOpt = attempt {
    let scanDirForProj workDir =
        match Directory.GetFiles(workDir, "*.*proj") |> List.ofArray with
        | [] ->
            Error (InvalidArgsState "no .*proj project found in current directory, use --project argument to specify path")
        | [x] -> Ok x
        | _xs ->
            Error (InvalidArgsState "multiple .*proj found in current directory, use --project argument to specify path")

    let workDir = Directory.GetCurrentDirectory()

    let! proj =
        match projOpt with
        | Some p ->
            if File.Exists(p) then
                Ok p
            else
                // scan given directory
                scanDirForProj (Path.Combine(workDir, p))
        | None ->
            // scan current directory
            scanDirForProj workDir

    let projPath = proj |> Path.GetFullPath

    log (sprintf "resolved project path '%s'" projPath)

    do! if not (File.Exists projPath)
        then Error (ProjectFileNotFound projPath)
        else Ok ()

    return projPath
    }

open Dotnet.ProjInfo
open Dotnet.ProjInfo.ProjectRecognizer

let analyzeProj projPath = attempt {

    let! (isDotnetSdk, pi) =
        match kindOfProjectSdk projPath, ProjectLanguageRecognizer.languageOfProject projPath with
        | Some ProjectSdkKind.DotNetSdk, pi ->
            Ok (true, pi)
        | Some ProjectSdkKind.VerboseSdk, pi ->
            Ok (false, pi)
        | Some ProjectSdkKind.ProjectJson, _
        | None, _ ->
            Errors.GenericError "unsupported project format"
            |> Result.Error

    return isDotnetSdk, pi
}

let evaluateProj config requests project =
    Inspect.evaluateProject config requests project
    |> Result.mapError EvaluationError

let doNothing _log _msbuildExec projPath =
    Result.Ok None

let doRestore _log msbuildExec projPath =
    msbuildExec projPath [ MSBuild.MSbuildCli.Target "Restore" ]
    |> Result.map Some

let restoreIfNeededBySdk isDotnetSdk =
    if isDotnetSdk then doRestore else doNothing

let pickMsbuild isDotnetSdk (msbuildArg: Quotations.Expr<(string -> 'a)>) (dotnetcliArg: Quotations.Expr<(string -> 'a)>) (msbuildhostArg: Quotations.Expr<(MSBuildHostPicker -> 'a)>) (results: ParseResults<'a>) =

    let msbuildPath = results.GetResult(msbuildArg, defaultValue = "msbuild")
    let dotnetPath = results.GetResult(dotnetcliArg, defaultValue = "dotnet")
    let dotnetHostPicker = results.GetResult(msbuildhostArg, defaultValue = MSBuildHostPicker.Auto)

    match dotnetHostPicker, isDotnetSdk with
    | MSBuildHostPicker.MSBuild, _
    | MSBuildHostPicker.Auto, false ->
        MSBuildExePath.Path msbuildPath
    | MSBuildHostPicker.DotnetMSBuild, _
    | MSBuildHostPicker.Auto, true ->
        MSBuildExePath.DotnetMsbuild dotnetPath
    | x, _ -> failwithf "Unexpected msbuild host '%A'" x

let propMain config log (results: ParseResults<PropCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult PropCLIArguments.Project
        |> validateProj log

    let requests = ProjectEvaluationRequest.Restore :: List.choose id [
        results.TryGetResult PropCLIArguments.Framework |> Option.map ProjectEvaluationRequest.TargetFramework
        results.TryGetResult PropCLIArguments.Runtime |> Option.map ProjectEvaluationRequest.RuntimeIdentifier
        results.TryGetResult PropCLIArguments.Configuration |> Option.map ProjectEvaluationRequest.Configuration
    ]

    let props = results.GetResults GetProperty

    let! project = evaluateProj config requests projPath

    let projInstance = project.GetMutableProjectInstance()
    props
    |> Seq.map (fun propName -> propName, projInstance.GetPropertyValue propName)
    |> (fun x ->
        if results.Contains PropCLIArguments.Json then
            x |> dict |> JsonSerializer.Serialize |> Console.WriteLine
        else
            x |> Seq.iter ((<||) (sprintf "%s=%s") >> Console.WriteLine)
    )
}

let itemMain config log (results: ParseResults<ItemCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult ItemCLIArguments.Project
        |> validateProj log

    let dependsOn = results.GetResults ItemCLIArguments.Depends_On

    let requests = ProjectEvaluationRequest.Restore :: ProjectEvaluationRequest.Custom(dependsOn, []) :: List.choose id [
        results.TryGetResult ItemCLIArguments.Framework |> Option.map ProjectEvaluationRequest.TargetFramework
        results.TryGetResult ItemCLIArguments.Runtime |> Option.map ProjectEvaluationRequest.RuntimeIdentifier
        results.TryGetResult ItemCLIArguments.Configuration |> Option.map ProjectEvaluationRequest.Configuration
    ]

    let parseItemPath (path: string) =
        match path.Split('.') with
        | [|p|] -> p, "Identity"
        | [|p; m|] -> p, m
        | _ -> failwithf "Unexpected item path '%s'. Expected format is 'ItemName' or 'ItemName.Metadata' (like Compile.Identity or Compile.FullPath)" path

    let! proj = evaluateProj config requests projPath

    let items =
        results.GetResults ItemCLIArguments.GetItem
        |> Seq.map parseItemPath
        |> Seq.groupBy fst
        |> Seq.collect (fun (itemType, metadata) ->
            let metadata = Seq.map snd metadata |> List.ofSeq
            let items = proj.GetAllMutableItems itemType
            items
            |> Seq.map (fun item ->
                let metadata =
                    metadata
                    |> Seq.map (fun mdName -> mdName, item.GetMetadataValue mdName)
                    |> readOnlyDict
                {|ItemType = itemType; Metadata = metadata|}))
        |> List.ofSeq

    if results.Contains ItemCLIArguments.Json then
        printfn "%s" (JsonSerializer.Serialize items)
    else
        for item in items do
            for KeyValue(mdName, mdValue) in item.Metadata do
                printfn "%s.%s=%s" item.ItemType mdName mdValue
}

let compilerArgsMain config log (results: ParseResults<_>) = attempt {

    let! projPath =
        results.TryGetResult CommonProjectCLIArguments.Project
        |> validateProj log

    let requests = ProjectEvaluationRequest.Restore :: ProjectEvaluationRequest.GetCompilerArgs :: List.choose id [
        results.TryGetResult CommonProjectCLIArguments.Framework |> Option.map ProjectEvaluationRequest.TargetFramework
        results.TryGetResult CommonProjectCLIArguments.Runtime |> Option.map ProjectEvaluationRequest.RuntimeIdentifier
        results.TryGetResult CommonProjectCLIArguments.Configuration |> Option.map ProjectEvaluationRequest.Configuration
    ]

    let! proj = evaluateProj config requests projPath
    let! compilerArgs =
        Inspect.getCompilerArgs proj
        |> Result.mapError GenericError

    if results.Contains CommonProjectCLIArguments.Json then
        printfn "%s" (JsonSerializer.Serialize compilerArgs)
    else
        List.iter (printfn "%s") compilerArgs
}

let p2pMain config log (results: ParseResults<CommonProjectCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult Project
        |> validateProj log

    let requests = ProjectEvaluationRequest.Restore :: List.choose id [
        results.TryGetResult CommonProjectCLIArguments.Framework |> Option.map ProjectEvaluationRequest.TargetFramework
        results.TryGetResult CommonProjectCLIArguments.Runtime |> Option.map ProjectEvaluationRequest.RuntimeIdentifier
        results.TryGetResult CommonProjectCLIArguments.Configuration |> Option.map ProjectEvaluationRequest.Configuration
    ]

    let! proj = evaluateProj config requests projPath

    let projectReferences =
        proj.GetAllMutableItems("ProjectReference")
        |> Seq.map(fun item -> item.GetMetadataValue("FullPath"))
        |> List.ofSeq

    if results.Contains CommonProjectCLIArguments.Json then
        printfn "%s" (JsonSerializer.Serialize projectReferences)
    else
        List.iter (printfn "%s") projectReferences
}

let netFwMain log (results: ParseResults<NetFwCLIArguments>) = attempt {

    let projPath =
        //create the proj file
        Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.createEnvInfoProj ()
        |> Path.GetFullPath

    let msbuildPath = results.GetResult(NetFwCLIArguments.MSBuild, defaultValue = "msbuild")

    let cmd = Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.installedNETFrameworks

    let msbuildHost = MSBuildExePath.Path msbuildPath

    return projPath, cmd, msbuildHost, [], doNothing
    }

let netFwRefMain log (results: ParseResults<_>) = attempt {

    let! props =
        match results.GetResults Assembly with
        | [] -> Error (InvalidArgsState "multiple .*proj found in current directory, use --project argument to specify path")
        | props -> Ok props

    let projPath =
        //create the proj file
        Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.createEnvInfoProj ()
        |> Path.GetFullPath

    let msbuildPath = results.GetResult(NetFwRefCLIArguments.MSBuild, defaultValue = "msbuild")

    let cmd () = Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.getReferencePaths props

    let msbuildHost = MSBuildExePath.Path msbuildPath

    return projPath, cmd, msbuildHost, [], doNothing
    }

let createEvaluationConfig (results: ParseResults<_>) =
    let consoleVerbosity = if results.Contains Verbose then LoggerVerbosity.Normal else LoggerVerbosity.Minimal
    {ProjectEvaluationConfig.Default with ConsoleLogVerbosity = Some consoleVerbosity}

let realMain argv = attempt {

    let! results = parseArgsCommandLine argv

    do
        match results.TryGetResult MSBuild with
        | Some msBuildDir ->
            msBuildDir
            |> Path.GetDirectoryName
            |> MSBuildLocator.RegisterMSBuildPath
        | None -> MSBuildLocator.RegisterDefaults() |> ignore

    let config = createEvaluationConfig results

    let log =
        match results.TryGetResult Verbose with
        | Some _ -> printfn "%s"
        | None -> ignore

    let! (projPath, cmd, msbuildHost, globalArgs, preAction) =
        match results.TryGetSubCommand() with
        | Some (Prop subCmd) ->
            propMain config log subCmd
        | Some (Item subCmd) ->
            itemMain config log subCmd
        | Some (Compiler_Args subCmd) ->
            compilerArgsMain config log subCmd
        | Some (Fsc_Args subCmd)
        | Some (Csc_Args subCmd) ->
            log "This subcommand is deprecated. Use compiler-args instead."
            compilerArgsMain config log subCmd
        | Some (P2p subCmd) ->
            p2pMain config log subCmd
        | Some (Net_Fw subCmd) ->
            netFwMain log subCmd
        | Some (Net_Fw_Ref subCmd) ->
            netFwRefMain log subCmd
        | Some _ ->
            fun _ -> Error (InvalidArgsState "unknown sub command")
        | None ->
            fun _ ->  Error (InvalidArgsState "specify one command")

    let globalArgs =
        match Environment.GetEnvironmentVariable("DOTNET_PROJ_INFO_MSBUILD_BL") with
        | "1" -> MSBuild.MSbuildCli.Switch("bl") :: globalArgs
        | _ -> globalArgs

    let msbuildExec proj args =
        let projDir = Path.GetDirectoryName(projPath)
        msbuild msbuildHost (runCmd log projDir) proj (globalArgs @ args)

    do! preAction log msbuildExec projPath
        |> Result.map ignore
        |> Result.mapError ExecutionError

    let! r =
        projPath
        |> getProjectInfo log msbuildExec cmd
        |> Result.mapError ExecutionError

    let out =
        match r with
        | FscArgs args -> args
        | CscArgs args -> args
        | P2PRefs args -> args
        | Properties args -> args |> List.map (fun (x,y) -> sprintf "%s=%s" x y)
        | Items args ->
            [ for item in args do
                yield sprintf "%s=%s" item.Name item.Identity
                for (metadata, value) in item.Metadata do
                    yield sprintf "%s.%s=%s" item.Name (getItemsModifierMSBuildProperty metadata) value ]
        | ResolvedP2PRefs args ->
            let optionalTfm t =
                t |> Option.map (sprintf " (%s)") |> Option.defaultValue ""
            args |> List.map (fun r -> sprintf "%s%s" r.ProjectReferenceFullPath (optionalTfm r.TargetFramework))
        | ResolvedNETRefs args -> args
        | InstalledNETFw args -> args

    out |> List.iter (printfn "%s")
}

let wrapEx m f a =
    try
        f a
    with ex ->
        Error (RaisedException (ex, m))

let (|HelpRequested|_|) (ex: ArguParseException) =
    match ex.ErrorCode with
    | ErrorCode.HelpText -> Some ex.Message
    | _ -> None

[<EntryPoint>]
let main argv =
    match wrapEx "uncaught exception" (realMain >> runAttempt) argv with
    | Ok () -> 0
    | Error err ->
        match err with
        | InvalidArgs (HelpRequested helpText) ->
            printfn "dotnet-proj."
            printfn " "
            printfn "%s" helpText
            0
        | InvalidArgs ex ->
            printfn "%s" (ex.Message)
            1
        | InvalidArgsState message ->
            printfn "%s" message
            printfn "see --help for more info"
            2
        | ProjectFileNotFound projPath ->
            printfn "project file '%s' not found" projPath
            3
        | GenericError message ->
            printfn "%s" message
            4
        | RaisedException (ex, message) ->
            printfn "%s:" message
            printfn "%A" ex
            6
        | EvaluationError ex ->
            printfn "Project evaluation failed."
            match ex with
            | Some ex -> string ex
            | None -> "MSBuild did not raise an exception."
            |> Console.WriteLine
            7
        | ExecutionError (MSBuildFailed (i, ShellCommandResult(wd, exePath, args, output))) ->
            printfn "msbuild exit code: %i" i
            printfn "command line was: %s> %s %s" wd exePath args
            for (isErr, line) in output do
                if isErr then
                    printfn "stderr: %s" line
                else
                    printfn "stdout: %s" line
            7
        | ExecutionError (UnexpectedMSBuildResult r) ->
            printfn "%A" r
            8
        | ExecutionError (MSBuildSkippedTarget) ->
            printfn "internal error, target was skipped"
            9
