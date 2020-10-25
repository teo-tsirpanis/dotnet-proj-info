namespace Dotnet.ProjInfo

open System
open System.IO

[<RequireQualifiedAccess>]
type ProjectSdkKind =
    | ProjectJson
    | DotNetSdk
    | VerboseSdk

type ProjectLanguage =
    | CSharp
    | FSharp
    | Unknown of string

module ProjectLanguageRecognizer =

    let languageOfProject (file: string) =
        match Path.GetExtension(file) with
        | ".csproj" -> ProjectLanguage.CSharp
        | ".fsproj" -> ProjectLanguage.FSharp
        | ext -> ProjectLanguage.Unknown ext

module ProjectRecognizer =
    let kindOfProjectSdk (file: string) =
        //.NET Core Sdk preview3+ replace project.json with fsproj
        //Easy way to detect new fsproj is to check the msbuild version of .fsproj
        //Post preview5 has (`Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk"`), use that
        //  for checking .NET Core fsproj. NB: casing of FSharp may be inconsistent.
        //The `dotnet-compile-fsc.rsp` are created also in `preview3+`, so we can
        //  reuse the same behaviour of `preview2`
        let rec getProjectType (sr:StreamReader) limit =
            if limit = 0 then
                None // unknown project type
            else
                let line = sr.ReadLine()
                if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                    getProjectType sr (limit-1)
                else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                    if line.IndexOf("Sdk=", StringComparison.OrdinalIgnoreCase) = 0 then
                        ProjectSdkKind.DotNetSdk
                    else
                        ProjectSdkKind.VerboseSdk
                    |> Some
        if Path.GetExtension file = ".json" then
            Some ProjectSdkKind.ProjectJson // dotnet core preview 2 or earlier
        else
            use sr = File.OpenText(file)
            getProjectType sr 3
