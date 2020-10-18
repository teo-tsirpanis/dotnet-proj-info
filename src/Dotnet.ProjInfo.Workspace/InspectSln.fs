module Dotnet.ProjInfo.Workspace.InspectSln

open System
open System.IO

let private normalizeDirSeparators (path: string) =
    match Path.DirectorySeparatorChar with
    | '\\' -> path.Replace('/', '\\')
    | '/' -> path.Replace('\\', '/')
    | _ -> path

type SolutionData = {
    Items: SolutionItem list
    Configurations: SolutionConfiguration list
}
and SolutionConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
    IncludeInBuild: bool
}
and SolutionItem = {
    Guid: Guid
    Name: string
    Kind: SolutionItemKind
}
and SolutionItemKind =
    | MsbuildFormat of SolutionItemMsbuildConfiguration list
    | Folder of (SolutionItem list) * (string list)
    | Unsupported
    | Unknown
and SolutionItemMsbuildConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
}

let tryParseSln (slnFilePath: string) = 
    let parseSln (sln: Microsoft.Build.Construction.SolutionFile) =
        let slnDir = Path.GetDirectoryName slnFilePath
        let makeAbsoluteFromSlnDir =
            let makeAbs (path: string) =
                if Path.IsPathRooted path then
                    path
                else
                    Path.Combine(slnDir, path)
                    |> Path.GetFullPath
            normalizeDirSeparators >> makeAbs
        let rec parseItem (item: Microsoft.Build.Construction.ProjectInSolution) =
            let parseKind (item: Microsoft.Build.Construction.ProjectInSolution) =
                match item.ProjectType with
                | Microsoft.Build.Construction.SolutionProjectType.KnownToBeMSBuildFormat ->
                    (item.RelativePath |> makeAbsoluteFromSlnDir), SolutionItemKind.MsbuildFormat []
                | Microsoft.Build.Construction.SolutionProjectType.SolutionFolder ->
                    let children =
                        sln.ProjectsInOrder
                        |> Seq.filter (fun x -> x.ParentProjectGuid = item.ProjectGuid)
                        |> Seq.map parseItem
                        |> List.ofSeq
                    let files =
                        item.FolderFiles
                        |> Seq.map makeAbsoluteFromSlnDir
                        |> List.ofSeq
                    item.ProjectName, SolutionItemKind.Folder (children, files)
                | Microsoft.Build.Construction.SolutionProjectType.EtpSubProject
                | Microsoft.Build.Construction.SolutionProjectType.WebDeploymentProject
                | Microsoft.Build.Construction.SolutionProjectType.WebProject ->
                    (item.ProjectName |> makeAbsoluteFromSlnDir), SolutionItemKind.Unsupported
                | Microsoft.Build.Construction.SolutionProjectType.Unknown
                | _ ->
                    (item.ProjectName |> makeAbsoluteFromSlnDir), SolutionItemKind.Unknown

            let name, itemKind = parseKind item 
            {
                Guid = item.ProjectGuid |> Guid.Parse
                Name = name
                Kind = itemKind
            }

        let items =
            sln.ProjectsInOrder
            |> Seq.filter (fun x -> isNull x.ParentProjectGuid)
            |> Seq.map parseItem
        let data = {
            Items = items |> List.ofSeq
            Configurations = []
        }
        (slnFilePath, data)

    try
        slnFilePath
        |> Microsoft.Build.Construction.SolutionFile.Parse
        |> parseSln
        |> Ok
    with ex ->
        Error ex

let loadingBuildOrder (data: SolutionData) =

    let rec projs (item: SolutionItem) =
        match item.Kind with
        | MsbuildFormat items -> [item.Name]
        | Folder (items, _) -> List.collect projs items
        | Unsupported
        | Unknown -> []

    data.Items
    |> List.collect projs
