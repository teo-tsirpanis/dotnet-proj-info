namespace Dotnet.ProjInfo.Workspace

module internal CommonHelpers =

    let chooseByPrefix (prefix: string) (s: string) =
        if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
        else None

    let chooseByPrefix2 prefixes (s: string) =
        prefixes
        |> List.tryPick (fun prefix -> chooseByPrefix prefix s)

    let splitByPrefix (prefix: string) (s: string) =
        if s.StartsWith(prefix) then Some (prefix, s.Substring(prefix.Length))
        else None

    let splitByPrefix2 prefixes (s: string) =
        prefixes
        |> List.tryPick (fun prefix -> splitByPrefix prefix s)

module internal Utils =

    let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
        let psi = System.Diagnostics.ProcessStartInfo()
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.Arguments <- args
        psi.CreateNoWindow <- true
        psi.UseShellExecute <- false

        use p = new System.Diagnostics.Process()
        p.StartInfo <- psi

        p.OutputDataReceived.Add(fun ea -> log (ea.Data))

        p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

        // printfn "running: %s %s" psi.FileName psi.Arguments

        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        let exitCode = p.ExitCode

        exitCode, (workingDir, exePath, args)

    let isWindows () =
#if NET461
        System.Environment.OSVersion.Platform = System.PlatformID.Win32NT
#else  
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.Windows)
#endif
