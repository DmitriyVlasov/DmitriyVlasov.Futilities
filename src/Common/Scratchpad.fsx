#I @"..\..\bin\Common"
#r @"Incubator.FSharp.Common.dll"

open Incubator.Common
open Incubator.Common.Experimental

String.paste()
|> Text.clearStopSymbols
|> String.copy