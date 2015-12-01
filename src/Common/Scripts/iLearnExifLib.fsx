#load "load-project-debug.fsx"

#I @"../../../packages/ExifLib/lib/net40"
#r "ExifLib.dll"

open DmitriyVlasov
open DmitriyVlasov.Scripting

open ExifLib

open System
open System.IO

// Ссылка на проект:
// http://www.codeproject.com/Articles/36342/ExifLib-A-Fast-Exif-Data-Extractor-for-NET

let tryExifValue<'T> exifTags fileName =
  try
    use reader = new ExifReader(fileName = fileName)
    match reader.GetTagValue<'T>(tag = exifTags) with 
    | true, value -> Some value
    | false, _    -> None
  with _ -> None

let exifValue<'T> exifTags fileName =
  match tryExifValue<'T> exifTags fileName with
  | Some value -> value
  | None -> ExifLibException(sprintf "%s" fileName) |> raise

/// Извлечь из пути к файлу иерархию формате даты: yyyy/mm/dd.
/// Иерархию даты искать относительно базового пути basePath.
let tryGetDateFromPath basePath path =
  try
    dirname path
    |> String.replace basePath ""
    |> String.split "/"
    |> Array.map int
    |> fun a -> Some <| DateTime(a.[0], a.[1], a.[2])
  with _ ->
    None

let ``Файлы некорректно распределенные в каталоги`` predicate path = 
  ls path
  |> Seq.map FileInfo
  |> Seq.filter predicate
  |> Seq.map (fun fi -> 
    fi.FullName,
    tryGetDateFromPath path fi.FullName,
    fi.CreationTime.Date,
    tryExifValue<DateTime> ExifTags.DateTimeOriginal fi.FullName)
  |> Seq.filter (fun (path, somePathDateTime, fileDireTime, someExifDateTime) -> 
    match somePathDateTime, someExifDateTime with
    | None, _    -> true
    | _,    None -> true
    | Some pathDateTime, Some exifDateTime -> pathDateTime <> exifDateTime.Date)
 