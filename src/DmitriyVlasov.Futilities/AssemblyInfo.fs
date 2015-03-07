namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("DmitriyVlasov.Futilities")>]
[<assembly: AssemblyProductAttribute("DmitriyVlasov.Futilities")>]
[<assembly: AssemblyDescriptionAttribute("My collection of utilities on FSharp.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
