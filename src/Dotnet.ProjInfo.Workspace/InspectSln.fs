module Dotnet.ProjInfo.Workspace.InspectSln

open EnricoSada.MSBuild.Construction
open System
open System.IO

let private normalizeDirSeparators (path: string) =
    match Path.DirectorySeparatorChar with
    | '\\' -> path.Replace('/', '\\')
    | '/' -> path.Replace('\\', '/')
    | _ -> path

type SolutionConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
    IncludeInBuild: bool
}
type SolutionItemMsbuildConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
}
type SolutionItemKind =
    | MsbuildFormat of SolutionItemMsbuildConfiguration list
    | Folder of (SolutionItem list) * (string list)
    | Unsupported
    | Unknown
and SolutionItem = {
    Guid: Guid
    Name: string
    Kind: SolutionItemKind
}
type SolutionData = {
    Items: SolutionItem list
    Configurations: SolutionConfiguration list
}

let tryParseSln (slnFilePath: string) = 
    let parseSln (sln: SolutionFile) =
        let slnDir = Path.GetDirectoryName slnFilePath
        let makeAbsoluteFromSlnDir =
            let makeAbs (path: string) =
                if Path.IsPathRooted path then
                    path
                else
                    Path.Combine(slnDir, path)
                    |> Path.GetFullPath
            normalizeDirSeparators >> makeAbs
        let rec parseItem (item: ProjectInSolution) =
            let name, itemKind =
                match item.ProjectType with
                | SolutionProjectType.KnownToBeMSBuildFormat ->
                    (item.RelativePath |> makeAbsoluteFromSlnDir), SolutionItemKind.MsbuildFormat []
                | SolutionProjectType.SolutionFolder ->
                    let children =
                        sln.ProjectsInOrder
                        |> Seq.filter (fun x -> x.ParentProjectGuid = item.ProjectGuid)
                        |> Seq.map parseItem
                        |> List.ofSeq
                    let files =
                        item.FolderFiles
                        |> Seq.map makeAbsoluteFromSlnDir
                        |> List.ofSeq
                    item.ProjectName, Folder (children, files)
                | SolutionProjectType.EtpSubProject
                | SolutionProjectType.WebDeploymentProject
                | SolutionProjectType.WebProject ->
                    (item.ProjectName |> makeAbsoluteFromSlnDir), Unsupported
                | SolutionProjectType.Unknown
                | _ ->
                    (item.ProjectName |> makeAbsoluteFromSlnDir), Unknown

            {
                Guid = Guid.Parse item.ProjectGuid
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
        data

    try
        slnFilePath
        |> SolutionFile.Parse
        |> parseSln
        |> Ok
    with ex ->
        Error ex

let loadingBuildOrder (data: SolutionData) =

    let rec projs (item: SolutionItem) =
        match item.Kind with
        | MsbuildFormat _ -> [item.Name]
        | Folder (items, _) -> List.collect projs items
        | Unsupported
        | Unknown -> []

    data.Items
    |> List.collect projs
