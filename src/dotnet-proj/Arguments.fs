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
    | [<Unique>] Json
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "the MSBuild project file"
            | Framework _ -> "target framework, the TargetFramework msbuild property"
            | Runtime _ -> "target runtime, the RuntimeIdentifier msbuild property"
            | Configuration _ -> "configuration to use (like Debug), the Configuration msbuild property"
            | GetProperty _ -> "msbuild property to get (allow multiple)"
            | Property _ -> "msbuild global property to set (allow multiple)"
            | Json -> "print output as JSON"

type ItemCLIArguments =
    | [<MainCommand; Unique>] Project of string
    | [<AltCommandLine("-get")>] GetItem of string
    | [<AltCommandLine("-p")>] Property of string list
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-r")>] Runtime of string
    | [<AltCommandLine("-c")>] Configuration of string
    | [<AltCommandLine("-d")>] Depends_On of string
    | [<Unique>] Json
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
            | Depends_On _ -> "the Msbuild host, if auto then oldsdk=MSBuild dotnetSdk=DotnetCLI"
            | Json -> "print output as JSON"

type CommonProjectCLIArguments =
    | [<MainCommand; Unique>] Project of string
    | [<AltCommandLine("-p")>] Property of string list
    | [<AltCommandLine("-f")>] Framework of string
    | [<AltCommandLine("-r")>] Runtime of string
    | [<AltCommandLine("-c")>] Configuration of string
    | [<Unique>] Json
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Project _ -> "the MSBuild project file"
            | Framework _ -> "target framework, the TargetFramework msbuild property"
            | Runtime _ -> "target runtime, the RuntimeIdentifier msbuild property"
            | Configuration _ -> "configuration to use (like Debug), the Configuration msbuild property"
            | Property _ -> "msbuild property to use (allow multiple)"
            | Json _ -> "print output as JSON"

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
    | [<AltCommandLine("-v"); Inherit>] Verbose
    | [<Unique; Inherit>] MSBuild of string
    | [<CliPrefix(CliPrefix.None)>] Prop of ParseResults<PropCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Compiler_Args of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None); Hidden>] Fsc_Args of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None); Hidden>] Csc_Args of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] P2p of ParseResults<CommonProjectCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Item of ParseResults<ItemCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Net_Fw of ParseResults<NetFwCLIArguments>
    | [<CliPrefix(CliPrefix.None)>] Net_Fw_Ref of ParseResults<NetFwRefCLIArguments>
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Verbose -> "verbose log"
            | MSBuild _ -> "MSBuild directory"
            | Prop _ -> "get properties"
            | Compiler_Args _ -> "get compiler arguments"
            | Fsc_Args _ -> "get fsc arguments (deprecated; use compiler-args)"
            | Csc_Args _ -> "get csc arguments (deprecated; use compiler-args)"
            | P2p _ -> "get project references"
            | Item _ -> "get items"
            | Net_Fw _ -> "list the installed .NET Frameworks"
            | Net_Fw_Ref _ -> "get the reference path of given .NET Framework assembly"
