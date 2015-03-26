namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Common")>]
[<assembly: AssemblyProductAttribute("Common")>]
[<assembly: AssemblyDescriptionAttribute("My collection of utilities on FSharp.")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
