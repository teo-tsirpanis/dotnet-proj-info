namespace Dotnet.ProjInfo.MSBuild

open Microsoft.Build.Execution
open Microsoft.Build.Framework
open Microsoft.Build.Logging
open System
open System.IO
open System.Threading

/// Additional parameters to customize project evaluation.
type ProjectEvaluationConfig =
    {
        /// The path of the *.binlog file; should it be generated.
        BinaryLogPath: string option
        /// The verbosity of the MSBuild messages to be printed to the console; should they be printed.
        ConsoleLogVerbosity: LoggerVerbosity option
        /// Any additional MSBuild loggers that will track the project evaluation.
        AdditionalLoggers: ILogger seq
    }
    /// The default `ProjectEvaluationConfig`.
    /// Messages are printed to the console with minimal verbosity.
    static member Default = {
        BinaryLogPath = None
        ConsoleLogVerbosity = Some LoggerVerbosity.Minimal
        AdditionalLoggers = Seq.empty
    }

/// An object specifying targets to be run and global
/// properties to be set when a project gets evaluated.
[<RequireQualifiedAccess>]
type ProjectEvaluationRequest =
    /// Sets the appropriate parameters to the evaluator
    /// to get the compiler arguments of the project.
    /// As a parameter, whether the project is SDK-style has to be passed.
    | GetCompilerArgs of isSdkStyleProject: bool
    /// Sets the appropriate parameters to get
    /// the resolved project references of the project.
    | GetResolvedProjectToProjectReferences
    /// Sets the target framework of the project to the specified one.
    | TargetFramework of tfm: string
    /// Sets the runtime identifier of the project to the specified one.
    | RuntimeIdentifier of rid: string
    /// Sets the configuration of the project (Debug/Release) to the specified one.
    | Configuration of string
    /// Sets custom targets and global properties to the specified ones.
    | Custom of targets: string seq * globalProperties: (string * string) seq

/// The result of a project evaluation. It wraps a
/// `Microsoft.Build.Execution.ProjectInstance` with some convenience methods.
type ProjectEvaluationResult internal(requests, proj: ProjectInstance) =
    let loadTime = DateTimeOffset.Now
    /// The project's full path.
    member _.ProjectFullPath = proj.FullPath
    /// The date and time the evaluation happened.
    member _.LoadTime = loadTime
    /// The `ProjectEvaluationRequest`s that were used for this evalutation.
    member _.EvaluationRequests = requests
    /// The project's global properties.
    member _.GlobalProperties = proj.GlobalProperties
    /// Tries to get the value of the specified property, if it exists.
    member _.TryGetPropertyValue propName =
        let prop = proj.GetPropertyValue(propName)
        if String.IsNullOrEmpty prop then
            ValueNone
        else
            ValueSome prop
    /// Gets all the items of the specified type, or an empty collection if none were found.
    /// The returned items are subject to MSBuild's mutability rules.
    member _.GetAllMutableItems itemType =
        proj.GetItems(itemType)
    /// Clones the `ProjectInstance` within this object and returns it.
    member _.CloneProjectInstance() = proj.DeepCopy(false)
    /// Returns the `ProjectInstance` within this object as-is.
    /// `ProjectInstance` objects are mutable; care should be exercised
    /// to contain any side-effects of mutability when directly used.
    member _.GetMutableProjectInstance() = proj

/// Functions to evaluate MSBuild projects using the official APIs.
/// Before using any of the functions, the MSBuild assemblies must be located
/// with one of `Microsoft.Build.Locator.MSBuildLocator`'s methods.
module Inspect =

    let private dummyTargetArray =
        [|"Dotnet.ProjInfo.Unique." + Guid.NewGuid().ToString()|]

    let private createBuildParams config =
        let parameters = BuildParameters()
        parameters.MaxNodeCount <- 1
        // FSAC has been observed opening excess MSBuild node processes, so let's try to avoid it.
        // Besides, most MSBuild invocations through this library are trivial.
        parameters.EnableNodeReuse <- false
        let loggers = ResizeArray(config.AdditionalLoggers)
        match config.BinaryLogPath with
        | Some path ->
            let binLog = BinaryLogger()
            binLog.Parameters <- path
            loggers.Add binLog
        | None -> ()
        match config.ConsoleLogVerbosity with
        | Some verbosity ->
            let consoleLog = ConsoleLogger()
            consoleLog.Verbosity <- verbosity
            loggers.Add consoleLog
        | None -> ()
        parameters.Loggers <- loggers
        parameters

    let private createBuildRequestData targetsToExecute projectInstance =
        let runNoTarget = Array.isEmpty targetsToExecute
        let flags = if runNoTarget then BuildRequestDataFlags.None else BuildRequestDataFlags.SkipNonexistentTargets
        let targets = if runNoTarget then targetsToExecute else dummyTargetArray
        BuildRequestData(projectInstance, targets, null, flags)

    /// Returns the targets and global properties that the given `ProjectEvaluationRequest`s set.
    /// Duplicate targets are ignored. In case of duplicate global properties the latter one takes precedence.
    /// Unless overriden, the global properties "DesigntimeBuild" and "DotnetProjInfo" are always set to true.
    let expandEvaluationRequests requests =
        let targetsRaw, globalPropertiesRaw =
            requests
            |> Seq.map (function
                | ProjectEvaluationRequest.GetCompilerArgs true ->
                    ["ResolveReferences"; "CoreCompile"],
                    [
                        "SkipCompilerExecution", "true"
                        "ProvideCommandLineArgs" , "true"
                        "CopyBuildOutputToOutputDirectory", "false"
                        "UseCommonOutputDirectory", "true"
                    ]
                | ProjectEvaluationRequest.GetCompilerArgs false ->
                    "Getting compiler arguments for legacy projects is not supported."
                    |> NotSupportedException
                    |> raise
                | ProjectEvaluationRequest.GetResolvedProjectToProjectReferences ->
                    ["ResolveProjectReferencesDesignTime"], []
                | ProjectEvaluationRequest.TargetFramework tfm ->
                    [], ["TargetFramework", tfm]
                | ProjectEvaluationRequest.RuntimeIdentifier rid ->
                    [], ["RuntimeIdentifier", rid]
                | ProjectEvaluationRequest.Configuration x ->
                    [], ["Configuration", x]
                | ProjectEvaluationRequest.Custom(targets, globalProperties) ->
                    List.ofSeq targets, List.ofSeq globalProperties)
            |> List.ofSeq
            |> List.unzip
        let targets =
            match List.concat targetsRaw with
            | [] -> dummyTargetArray
            | xs -> xs |> Seq.distinct |> Array.ofSeq
        let globalProperties =
            // Let's put something to identify ourselves.
            // Other tools can set their own properties as well.
            ("DotnetProjInfo", "true")
            // Always marking it as a design-time build seems to be a good idea.
            :: ("DesigntimeBuild", "true")
            :: List.concat globalPropertiesRaw
            |> dict
        targets, globalProperties

    let private buildManagers =
        new ThreadLocal<_>(fun () ->
            new BuildManager(
                sprintf "Dotnet.ProjInfo build manager for thread %d" Thread.CurrentThread.ManagedThreadId))

    /// Evaluates an MSBuild project.
    let evaluateProject config requests projectPath =
        let requests = List.ofSeq requests
        let buildManager = buildManagers.Value
        let projectFullPath = Path.GetFullPath(projectPath)
        let targetsToExecute, globalProperties = expandEvaluationRequests requests
        let projectInstance = ProjectInstance(projectFullPath, globalProperties, null)
        let buildParams = createBuildParams config
        let buildRequestData = createBuildRequestData targetsToExecute projectInstance

        let buildResult = buildManager.Build(buildParams, buildRequestData)

        match buildResult.OverallResult, buildResult.Exception with
        | BuildResultCode.Success, _ ->
            ProjectEvaluationResult(requests, projectInstance) |> Ok
        | _, null -> Error None
        | _, ex -> Error (Some ex)
