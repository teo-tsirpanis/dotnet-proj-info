open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open Argu
open Arguments
open Railway
open Dotnet.ProjInfo.Inspect

type ShellCommandResult = ShellCommandResult of workingDir: string * exePath: string * args: string * output: seq<bool*string>

type Errors =
    | InvalidArgs of Argu.ArguParseException
    | InvalidArgsState of string
    | ProjectFileNotFound of string
    | GenericError of string
    | RaisedException of exn * string
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

open Dotnet.ProjInfo.Workspace.ProjectRecognizer

let analizeProj projPath = attempt {

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

let propMain log (results: ParseResults<PropCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult PropCLIArguments.Project
        |> validateProj log

    let! (isDotnetSdk, _) = analizeProj projPath

    let globalArgs =
        [ results.TryGetResult PropCLIArguments.Framework, if isDotnetSdk then "TargetFramework" else "TargetFrameworkVersion"
          results.TryGetResult PropCLIArguments.Runtime, "RuntimeIdentifier"
          results.TryGetResult PropCLIArguments.Configuration, "Configuration" ]
        |> List.choose (fun (a,p) -> a |> Option.map (fun x -> (p,x)))
        |> List.map (MSBuild.MSbuildCli.Property)

    let props = results.GetResults PropCLIArguments.GetProperty

    let cmd () = getProperties props

    let msbuildHost =
        results
        |> pickMsbuild isDotnetSdk <@ PropCLIArguments.MSBuild @> <@ PropCLIArguments.DotnetCli @> <@ PropCLIArguments.MSBuild_Host @>

    return projPath, cmd, msbuildHost, globalArgs, (restoreIfNeededBySdk isDotnetSdk)
    }

let itemMain log (results: ParseResults<ItemCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult ItemCLIArguments.Project
        |> validateProj log

    let! (isDotnetSdk, _) = analizeProj projPath

    let globalArgs =
        [ results.TryGetResult ItemCLIArguments.Framework, if isDotnetSdk then "TargetFramework" else "TargetFrameworkVersion"
          results.TryGetResult ItemCLIArguments.Runtime, "RuntimeIdentifier"
          results.TryGetResult ItemCLIArguments.Configuration, "Configuration" ]
        |> List.choose (fun (a,p) -> a |> Option.map (fun x -> (p,x)))
        |> List.map (MSBuild.MSbuildCli.Property)

    let parseItemPath (path: string) =
        match path.Split('.') |> List.ofArray with
        | [p] -> p, GetItemsModifier.Identity
        | [p; m] ->
            let modifier =
                match m.ToLower() with
                | "identity" -> GetItemsModifier.Identity
                | "fullpath" -> GetItemsModifier.FullPath
                | _ -> GetItemsModifier.Custom m
            p, modifier
        | _ -> failwithf "Unexpected item path '%s'. Expected format is 'ItemName' or 'ItemName.Metadata' (like Compile.Identity or Compile.FullPath)" path

    let items =
        results.GetResults ItemCLIArguments.GetItem
        |> List.map parseItemPath

    let dependsOn = results.GetResults ItemCLIArguments.Depends_On

    let cmd () = getItems items dependsOn

    let msbuildHost =
        results
        |> pickMsbuild isDotnetSdk <@ ItemCLIArguments.MSBuild @> <@ ItemCLIArguments.DotnetCli @> <@ ItemCLIArguments.MSBuild_Host @>

    return projPath, cmd, msbuildHost, globalArgs, (restoreIfNeededBySdk isDotnetSdk)
    }

let fscArgsMain log (results: ParseResults<_>) = attempt {

    let! projPath =
        results.TryGetResult CommonProjectCLIArguments.Project
        |> validateProj log

    let! (isDotnetSdk, projectLanguage) = analizeProj projPath

    let! getCompilerArgsBySdk =
        match isDotnetSdk, projectLanguage with
        | true, ProjectLanguageRecognizer.ProjectLanguage.FSharp ->
            Ok getFscArgs
        | false, ProjectLanguageRecognizer.ProjectLanguage.FSharp ->
            let asFscArgs props =
                let fsc = Microsoft.FSharp.Build.Fsc()
                Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
            Ok (getFscArgsOldSdk (asFscArgs >> Ok))
        | _, ProjectLanguageRecognizer.ProjectLanguage.CSharp ->
            Errors.GenericError (sprintf "fsc args not supported on .csproj, expected an .fsproj" )
            |> Result.Error
        | _, ProjectLanguageRecognizer.ProjectLanguage.Unknown ext ->
            Errors.GenericError (sprintf "compiler args not supported on project with extension %s, expected .fsproj" ext)
            |> Result.Error

    let globalArgs =
        [ results.TryGetResult CommonProjectCLIArguments.Framework , if isDotnetSdk then "TargetFramework" else "TargetFrameworkVersion"
          results.TryGetResult CommonProjectCLIArguments.Runtime, "RuntimeIdentifier"
          results.TryGetResult CommonProjectCLIArguments.Configuration, "Configuration" ]
        |> List.choose (fun (a,p) -> a |> Option.map (fun x -> (p,x)))
        |> List.map (MSBuild.MSbuildCli.Property)

    let cmd = getCompilerArgsBySdk

    let msbuildHost =
        results
        |> pickMsbuild isDotnetSdk <@ CommonProjectCLIArguments.MSBuild @> <@ CommonProjectCLIArguments.DotnetCli @> <@ CommonProjectCLIArguments.MSBuild_Host @>

    return projPath, cmd, msbuildHost, globalArgs, (restoreIfNeededBySdk isDotnetSdk)
    }

let cscArgsMain log (results: ParseResults<CommonProjectCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult CommonProjectCLIArguments.Project
        |> validateProj log

    let! (isDotnetSdk, projectLanguage) = analizeProj projPath

    let! getCompilerArgsBySdk =
        match isDotnetSdk, projectLanguage with
        | true, ProjectLanguageRecognizer.ProjectLanguage.CSharp ->
            Ok getCscArgs
        | false, ProjectLanguageRecognizer.ProjectLanguage.CSharp ->
            Errors.GenericError "csc args not supported on old sdk"
            |> Result.Error
        | _, ProjectLanguageRecognizer.ProjectLanguage.FSharp ->
            Errors.GenericError (sprintf "csc args not supported on .fsproj, expected an .csproj" )
            |> Result.Error
        | _, ProjectLanguageRecognizer.ProjectLanguage.Unknown ext ->
            Errors.GenericError (sprintf "compiler args not supported on project with extension %s" ext)
            |> Result.Error

    let globalArgs =
        [ results.TryGetResult CommonProjectCLIArguments.Framework, if isDotnetSdk then "TargetFramework" else "TargetFrameworkVersion"
          results.TryGetResult CommonProjectCLIArguments.Runtime, "RuntimeIdentifier"
          results.TryGetResult CommonProjectCLIArguments.Configuration, "Configuration" ]
        |> List.choose (fun (a,p) -> a |> Option.map (fun x -> (p,x)))
        |> List.map (MSBuild.MSbuildCli.Property)

    let cmd = getCompilerArgsBySdk

    let msbuildHost =
        results
        |> pickMsbuild isDotnetSdk <@ CommonProjectCLIArguments.MSBuild @> <@ CommonProjectCLIArguments.DotnetCli @> <@ CommonProjectCLIArguments.MSBuild_Host @>

    return projPath, cmd, msbuildHost, globalArgs, (restoreIfNeededBySdk isDotnetSdk)
    }

let p2pMain log (results: ParseResults<CommonProjectCLIArguments>) = attempt {

    let! projPath =
        results.TryGetResult Project
        |> validateProj log

    let! (isDotnetSdk, _projectLanguage) = analizeProj projPath

    let globalArgs =
        [ results.TryGetResult Framework, if isDotnetSdk then "TargetFramework" else "TargetFrameworkVersion"
          results.TryGetResult Runtime, "RuntimeIdentifier"
          results.TryGetResult Configuration, "Configuration" ]
        |> List.choose (fun (a,p) -> a |> Option.map (fun x -> (p,x)))
        |> List.map (MSBuild.MSbuildCli.Property)

    let cmd = getP2PRefs

    let msbuildHost =
        results
        |> pickMsbuild isDotnetSdk <@ CommonProjectCLIArguments.MSBuild @> <@ DotnetCli @> <@ MSBuild_Host @>

    return projPath, cmd, msbuildHost, globalArgs, (restoreIfNeededBySdk isDotnetSdk)
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

    let msbuildPath = results.GetResult(MSBuild, defaultValue = "msbuild")

    let cmd () = Dotnet.ProjInfo.NETFrameworkInfoFromMSBuild.getReferencePaths props

    let msbuildHost = MSBuildExePath.Path msbuildPath

    return projPath, cmd, msbuildHost, [], doNothing
    }

let realMain argv = attempt {

    let! results = parseArgsCommandLine argv

    let log =
        match results.TryGetResult Verbose with
        | Some _ -> printfn "%s"
        | None -> ignore

    let! (projPath, cmd, msbuildHost, globalArgs, preAction) =
        match results.TryGetSubCommand () with
        | Some (Prop subCmd) ->
            propMain log subCmd
        | Some (Item subCmd) ->
            itemMain log subCmd
        | Some (Fsc_Args subCmd) ->
            fscArgsMain log subCmd
        | Some (Csc_Args subCmd) ->
            cscArgsMain log subCmd
        | Some (P2p subCmd) ->
            p2pMain log subCmd
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

    return r
    }

let wrapEx m f a =
    try
        f a
    with ex ->
        Error (RaisedException (ex, m))

let (|HelpRequested|_|) (ex: ArguParseException) =
    match ex.ErrorCode with
    | Argu.ErrorCode.HelpText -> Some ex.Message
    | _ -> None

[<EntryPoint>]
let main argv =
    match wrapEx "uncaught exception" (realMain >> runAttempt) argv with
    | Ok _ -> 0
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
