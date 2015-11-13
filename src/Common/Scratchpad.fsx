#I @"..\..\bin\Common"
#r @"FSharp.Common.dll"

open Common
open Common.Experimental

String.paste()
|> Text.clearStopSymbols
|> String.copy