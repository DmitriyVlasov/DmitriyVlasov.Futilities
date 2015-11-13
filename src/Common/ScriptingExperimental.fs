namespace Common.Experimental

[<Experimental("Эксперементальные функции модуля Scripting")>]
module Scripting =
  open System
  open System.IO
  open Common
  open Common.Scripting

  /// Посчитать количество файлов в каталоге в разрезе в разрезе раширений.
  /// Если количество файлов меньше порогового показать пути к файлам.
  let filterGroupCountShowFiles rareFiles searchPattern dirPath = 
    let count = Seq.sumBy (fun _ -> 1)
    let showFiles = 
      Seq.map (String.replace dirPath "..")
      >> Seq.map (sprintf "\n\"%s\"")
      >> String.concat ""
    let rare files = 
      if count files <= rareFiles
      then showFiles files
      else ""
    find SearchOption.AllDirectories searchPattern dirPath
    |> Seq.groupBy extension
    |> Seq.map (fun (ext, files) -> ext, count files, files)
    |> Seq.sortBy (fun (_, count, _) -> count)
    |> Seq.iter (fun (ext, count, files) -> 
      printfn "%10s; %3d;%s" ext count (rare files))

  /// Статистика слов в файлах.
  let wordStatistics codepage delimiters searchPattern dirPath =
    find SearchOption.AllDirectories searchPattern dirPath
    |> Seq.map (fun filePath -> 
      filePath
      |> readText codepage
      |> String.splitMany delimiters)
    |> Seq.concat
    |> Seq.countBy id
    |> Seq.sortBy ( fun (_, number) -> - number )

  /// Список разделителей токенов в Sql.
  let sqlDelimiters = 
    [| "\r\n"; "\t"; " "
    ; ","; ";"; "."; "="; "|"
    ; "\""; "\'"; ")"; "("; "]"; "["
    ; "--" ; "/*"; "*\\"; |]

  /// Найти файлы вхождению в их содержимоме ключевых слов
  let searchFilesByContentKeywords keywords filePaths = 
    let content =
      filePaths 
      |> Seq.map (fun filePath -> filePath, readText 0 filePath)
    let search keyword = 
      content 
      |> Seq.filter (fun (_,(txt:string)) -> txt.Contains keyword)
      |> Seq.map fst
      |> Set.ofSeq
    keywords
    |> Seq.map search
    |> Set.intersectMany
    |> Set.toSeq

  ///
  let wget (url:string) = 
    use wc = new System.Net.WebClient() 
    let tmp = System.IO.Path.GetTempFileName() 
    wc.DownloadFile(url, tmp) 
    let target = filename url 
    System.IO.File.Move(tmp,target)

  /// Показать разницу в содержимом двух заданных каталогах
  let diff dirPath dirPath' =
    let fileNames = ls >> Seq.map filename >> Set.ofSeq
    let (-) x x' = fileNames x - fileNames x' |> Set.toSeq
    dirPath - dirPath', dirPath' - dirPath

  /// Возвращает последовательность пар 
  /// путь к текстовому файлу и найденное содержимое.filter 
  let findByContent  codepage searchString filePaths =
    let condition (line:string) = 
      line.IndexOf(value = searchString, comparisonType = System.StringComparison.OrdinalIgnoreCase) >= 0
    let foundLinesOfFile = lines codepage >> Seq.filter condition
    // TODO: Показывать контекст поиска: +-1 строка выше и ниже найденного вхождения.               
    filePaths
    |> Seq.map (fun path -> path, foundLinesOfFile path )
    |> Seq.filter (fun (_, foundLines) -> foundLines |> Seq.length > 0)

  /// Фильтрация, группировка и перемещение файлов расположенных в определенном каталоге.
  let filterGroupMoveFiles filterPredicate groupPredicate dirPath = 
    ls dirPath
    |> Seq.filter filterPredicate 
    |> Seq.toList
    |> List.hsGroupBy groupPredicate
    |> List.iteri (fun i groupFiles -> 
      let newDirPath = dirPath ++ (sprintf "%03i" i)
      mk newDirPath |> ignore
      groupFiles
      |> List.iter (fun filePath -> mv filePath <| newDirPath ++ filename filePath) )

  /// Файл является изображением.
  let isImageFile filePath = 
    match extension filePath with
    | ".JPG" |".jpg" -> true
    | _ -> false

  /// Время создания пары файлов меньше заданного времени минут.
  let timeSpanLess minutes file1 file2 = 
    let creationTime filePath = FileInfo(filePath).CreationTime
    creationTime file2 - creationTime file1 <= TimeSpan.FromMinutes(minutes)

