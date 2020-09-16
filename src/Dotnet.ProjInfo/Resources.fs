module Dotnet.ProjInfo.Resources

open System
open System.Collections.Concurrent
open System.IO

type private Dummy() = class end

let private resourceDict = ConcurrentDictionary<string,string>()

let getResourceFileAsStringImpl = Func<_,_>(fun resourceName ->
    let assembly = typeof<Dummy>.Assembly

    use stream = assembly.GetManifestResourceStream(resourceName)
    match stream with
    | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
    | stream ->
        use reader = new StreamReader(stream)

        reader.ReadToEnd()
)

let getResourceFileAsString resourceName =
    resourceDict.GetOrAdd(resourceName, getResourceFileAsStringImpl)
