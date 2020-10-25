namespace Dotnet.ProjInfo.Workspace

open System
open System.IO

type FilePath = string

[<RequireQualifiedAccess>]
type ProjectSdkType =
    | Verbose of ProjectSdkTypeVerbose
    | DotnetSdk of ProjectSdkTypeDotnetSdk
and ProjectSdkTypeVerbose = {
    TargetFrameworkVersion: string
    Configuration: string
}
and ProjectSdkTypeDotnetSdk = {
    IsTestProject: bool
    Configuration: string // Debug
    IsPackable: bool // true
    TargetFramework: string // netcoreapp1.0
    TargetFrameworkIdentifier: string // .NETCoreApp
    TargetFrameworkVersion: string // v1.0

    MSBuildAllProjects: FilePath list //;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.SupportedTargetFrameworks.props;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.BeforeCommon.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultOutputPaths.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.TargetFrameworkInference.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.RuntimeIdentifierInference.targets;C:\Users\e.sada\.nuget\packages\fsharp.net.sdk\1.0.5\build\FSharp.NET.Core.Sdk.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\c1.fsproj;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.Common.CurrentVersion.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\NuGet.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\15.0\Microsoft.Common.targets\ImportAfter\Microsoft.TestPlatform.ImportAfter.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.TestPlatform.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.proj-info.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.Common.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.PackageDependencyResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DisableStandardFrameworkResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.GenerateAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Publish.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.PreserveCompilationContext.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\NuGet.Build.Tasks.Pack\build\NuGet.Build.Tasks.Pack.targets
    MSBuildToolsVersion: string // 15.0

    ProjectAssetsFile: FilePath // e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\project.assets.json
    RestoreSuccess: bool // True

    Configurations: string list // Debug;Release
    TargetFrameworks: string list // netcoreapp1.0;netstandard1.6

    //may not exists
    RunArguments: string option // exec "e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\bin\Debug\netcoreapp1.0\c1.dll"
    RunCommand: string option // dotnet

    //from 2.0
    IsPublishable: bool option // true

}

type ExtraProjectInfoData =
    {
        TargetPath: string
        ProjectOutputType: ProjectOutputType
        ProjectSdkType: ProjectSdkType
    }
and ProjectOutputType =
    | Library
    | Exe
    | Custom of string


type GetProjectOptionsErrors =
    // projFile is duplicated in WorkspaceProjectState???
    | ProjectNotRestored of projFile:string
    | LanguageNotSupported of projFile:string
    | ProjectNotLoaded of projFile:string
    | MissingExtraProjectInfos of projFile:string
    | InvalidExtraProjectInfos of projFile:string * error:string
    | ReferencesNotLoaded of projFile:string * referenceErrors:seq<string*GetProjectOptionsErrors>
    | GenericError of projFile:string * string
    member x.ProjFile =
        match x with
        | ProjectNotRestored projFile
        | LanguageNotSupported projFile
        | ProjectNotLoaded projFile
        | MissingExtraProjectInfos projFile
        | InvalidExtraProjectInfos (projFile, _)
        | ReferencesNotLoaded (projFile, _)
        | GenericError (projFile, _) -> projFile

type ProjectOptions =
    {
        ProjectId: string option
        ProjectFileName: string
        TargetFramework: string
        SourceFiles: string list
        OtherOptions: string list
        ReferencedProjects: ProjectReference list
        LoadTime: DateTime
        ExtraProjectInfo: ExtraProjectInfoData
        Items: ProjectItem list
    }
and ProjectReference =
    {
        ProjectFileName: string
        TargetFramework: string
    }
and ProjectItem =
    | Compile of name: string * fullpath: string

type [<RequireQualifiedAccess>] WorkspaceProjectState =
    | Loading of string * ((string * string) list)
    | Loaded of ProjectOptions * Map<string,string> * fromCache: bool
    | Failed of string * GetProjectOptionsErrors

module internal FscArguments =

    open CommonHelpers

    let outType rsp =
        match List.tryPick (chooseByPrefix "--target:") rsp with
        | Some "library" -> ProjectOutputType.Library
        | Some "exe" -> ProjectOutputType.Exe
        | Some v -> ProjectOutputType.Custom v
        | None -> ProjectOutputType.Exe // default if arg is not passed to fsc

    let private outputFileArg = ["--out:"; "-o:"]

    let private makeAbs (projDir: string) (f: string) =
        if Path.IsPathRooted f then f else Path.Combine(projDir, f)

    let outputFile projDir rsp =
        rsp
        |> List.tryPick (chooseByPrefix2 outputFileArg)
        |> Option.map (makeAbs projDir)

    let isCompileFile (s:string) =
        //TODO check if is not an option, check prefix `-` ?
        s.EndsWith(".fs") || s.EndsWith (".fsi") || s.EndsWith (".fsx")

    let references =
        //TODO valid also --reference:
        List.choose (chooseByPrefix "-r:")

    let useFullPaths projDir (s: string) =
        match s |> splitByPrefix2 outputFileArg with
        | Some (prefix, v) ->
            prefix + (v |> makeAbs projDir)
        | None ->
            if isCompileFile s then
                s |> makeAbs projDir |> Path.GetFullPath
            else
                s

    let isTempFile (name: string) =
        let tempPath = Path.GetTempPath()
        let s = name.ToLower()
        s.StartsWith(tempPath.ToLower())

    let isDeprecatedArg n =
        // TODO put in FCS
        n = "--times" || n = "--no-jit-optimize"
