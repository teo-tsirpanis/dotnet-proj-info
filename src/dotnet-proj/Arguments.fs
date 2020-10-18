module Arguments

open Argu

type MSBuildHostPicker =
    | Auto = 1
    | MSBuild  = 2
    | DotnetMSBuild = 3

type PropCLIArguments =
    | [<MainCommand; Unique>] Project of string
    | [<AltCommandLine("-get")>] GetProperty of string
    | [<AltCommandLine("-p")>] Property of string list
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-r")>] Runtime of string
    | [<AltCommandLine("-c")>] Configuration of string
    | MSBuild of string
    | DotnetCli of string
    | MSBuild_Host of MSBuildHostPicker
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "the MSBuild project file"
            | Framework _ -> "target framework, the TargetFramework msbuild property"
            | Runtime _ -> "target runtime, the RuntimeIdentifier msbuild property"
            | Configuration _ -> "configuration to use (like Debug), the Configuration msbuild property"
            | GetProperty _ -> "msbuild property to get (allow multiple)"
            | Property _ -> "msbuild property to use (allow multiple)"
            | MSBuild _ -> """MSBuild path (default "msbuild")"""
            | DotnetCli _ -> """Dotnet CLI path (default "dotnet")"""
            | MSBuild_Host _ -> "the Msbuild host, if auto then oldsdk=MSBuild dotnetSdk=DotnetCLI"

type ItemCLIArguments =
    | [<MainCommand; Unique>] Project of string
    | [<AltCommandLine("-get")>] GetItem of string
    | [<AltCommandLine("-p")>] Property of string list
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-r")>] Runtime of string
    | [<AltCommandLine("-c")>] Configuration of string
    | [<AltCommandLine("-d")>] Depends_On of string
    | MSBuild of string
    | DotnetCli of string
    | MSBuild_Host of MSBuildHostPicker
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "the MSBuild project file"
            | Framework _ -> "target framework, the TargetFramework msbuild property"
            | Runtime _ -> "target runtime, the RuntimeIdentifier msbuild property"
            | Configuration _ -> "configuration to use (like Debug), the Configuration msbuild property"
            | GetItem _ -> "msbuild item to get (allow multiple)"
            | Property _ -> "msbuild property to use (allow multiple)"
            | MSBuild _ -> """MSBuild path (default "msbuild")"""
            | DotnetCli _ -> """Dotnet CLI path (default "dotnet")"""
            | MSBuild_Host _ -> "the Msbuild host, if auto then oldsdk=MSBuild dotnetSdk=DotnetCLI"
            | Depends_On _ -> "the Msbuild host, if auto then oldsdk=MSBuild dotnetSdk=DotnetCLI"

type CommonProjectCLIArguments =
    | [<MainCommand; Unique>] Project of string
    | [<AltCommandLine("-p")>] Property of string list
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-r")>] Runtime of string
    | [<AltCommandLine("-c")>] Configuration of string
    | MSBuild of string
    | DotnetCli of string
    | MSBuild_Host of MSBuildHostPicker
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "the MSBuild project file"
            | Framework _ -> "target framework, the TargetFramework msbuild property"
            | Runtime _ -> "target runtime, the RuntimeIdentifier msbuild property"
            | Configuration _ -> "configuration to use (like Debug), the Configuration msbuild property"
            | Property _ -> "msbuild property to use (allow multiple)"
            | MSBuild _ -> """MSBuild path (default "msbuild")"""
            | DotnetCli _ -> """Dotnet CLI path (default "dotnet")"""
            | MSBuild_Host _ -> "the Msbuild host, if auto then oldsdk=MSBuild dotnetSdk=DotnetCLI"

type NetFwCLIArguments =
    | MSBuild of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | MSBuild _ -> """MSBuild path (default "msbuild")"""

type NetFwRefCLIArguments =
    | [<MainCommand>] Assembly of string
    | MSBuild of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Assembly _ -> "the name of the assembly"
            | MSBuild _ -> """MSBuild path (default "msbuild")"""

type CLIArguments =
    | [<AltCommandLine("-v")>] Verbose
    | [<CliPrefix(CliPrefix.None)>] Prop of ParseResults<PropCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Fsc_Args of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Csc_Args of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] P2p of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Item of ParseResults<ItemCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Net_Fw of ParseResults<NetFwCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Net_Fw_Ref of ParseResults<NetFwRefCLIArguments>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Verbose -> "verbose log"
            | Prop _ -> "get properties"
            | Fsc_Args _ -> "get fsc arguments"
            | Csc_Args _ -> "get csc arguments"
            | P2p _ -> "get project references"
            | Item _ -> "get items"
            | Net_Fw _ -> "list the installed .NET Frameworks"
            | Net_Fw_Ref _ -> "get the reference path of given .NET Framework assembly"
