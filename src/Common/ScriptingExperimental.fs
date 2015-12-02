namespace DmitriyVlasov.Experimental

[<Experimental("Эксперементальные функции модуля Scripting")>]
module Scripting =
  open DmitriyVlasov
  open DmitriyVlasov.Collections
  open DmitriyVlasov.Scripting

  open System
  open System.IO
  open ExtCore

  /// Посчитать количество файлов в каталоге в разрезе в разрезе раширений.
  /// Если количество файлов меньше порогового показать пути к файлам.
  let filterGroupCountShowFiles rareFiles searchPattern dirPath = 
    let showFiles = 
      Seq.map (String.replace dirPath "..")
      >> Seq.map (sprintf "\n\"%s\"")
      >> String.concat ""
    let rare files = 
      if Seq.length files <= rareFiles
      then showFiles files
      else ""
    find SearchOption.AllDirectories searchPattern dirPath
    |> Seq.groupBy extension
    |> Seq.map (fun (ext, files) -> (ext, Seq.length files), files)
    |> Seq.sortBy (fst >> snd)
    |> Seq.iter (fun ((ext, count), files) -> 
      printfn "%10s; %3d;%s" ext count (rare files))

  /// Статистика слов в файлах.
  let wordStatistics codepage delimiters searchPattern dirPath =
    find SearchOption.AllDirectories searchPattern dirPath
    |> Seq.map (readText codepage >> String.splits delimiters)
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

  /// Время создания пары файлов меньше заданного времени минут.
  let timeSpanLess minutes file1 file2 = 
    let creationTime filePath = FileInfo(filePath).CreationTime
    creationTime file2 - creationTime file1 <= TimeSpan.FromMinutes(minutes)

  /// Удалить дубликаты файлов. Дубликаты ищутся с использованием хеширующей функции 
  let deleteDuplicateFilesBy hashFunction path =
    find SearchOption.AllDirectories "*.*" path
    |> Seq.map FileInfo
    |> Seq.groupBy hashFunction
    |> Seq.filter (snd >> Seq.length >> ((>) 1))
    |> Seq.map (snd >> Seq.tail)
    |> Seq.concat
    |> Seq.iter (fun fi -> 
      File.Delete fi.FullName)

  /// Удалить дубрикаты файлов у которых время создания и размер одинаковые
  let deleteDuplicateFiles path =
    deleteDuplicateFilesBy (fun fi -> 
      sprintf "#%s%d" (fi.CreationTime.ToString()) fi.Length) path

  let incBasename delim (basename:string) = 
    let inc = int >> (+) 1 >> string
    match String.splits [|delim|] basename with
    | [|x|]      -> String.concat delim [x; "1"]
    | [|x;iter|] -> String.concat delim [x; (inc iter)]
    | _ -> basename

  let nextFileName (path:string) =
    let newFileName = 
      path 
      |> basename 
      |> incBasename "~"
      |> Seq.singleton
      |> Seq.append [extension path]
      |> String.concat "" 
    (dirname path) ++ newFileName

  let distributeFilesByCreationTime sourcePath destinationPath = 
    ls sourcePath
    |> Seq.map (fun path ->
      let fi = FileInfo path 
      fi.FullName,
      destinationPath 
      ++ string fi.CreationTime.Year
      ++ sprintf "%02d" fi.CreationTime.Month
      ++ sprintf "%02d" fi.CreationTime.Day
      ++ fi.Name)

  let moveDistributeFilesByCreationTime sourcePath destinationPath = 
    distributeFilesByCreationTime sourcePath destinationPath
    |> Seq.iter (fun (sourceFileName, destFileName) -> 
      try
        let newDir = dirname destFileName
        if not <| Directory.Exists(newDir) then
          mk newDir |> ignore

        if fileExists destFileName then
          mv sourceFileName (nextFileName destFileName)
        else 
          mv sourceFileName destFileName
             
        // TODO: 
        // Если имена файлов одинаковые:
        // Проверяем одинаковый ли размер файла:
        // Если размер одинаковый считаем, что файл один и тот-же и из источника удаляем.
        // Если размер разный, файл переименовываем 

      with ex ->
        eprintfn "Source: '%s'; Destination: '%s'; %s:'%s'" sourceFileName destFileName (ex.GetType().ToString()) ex.Message
    )

  type MediaType =
    | Photo of string
    | Video of string
    | Text  of string 
    | Excel of string
    | Web   of string
    | Book  of string
    | Slide of string
    | Other of string

  let mediaType ext =
    match ext with
    | "jpeg" | "jpg" | "bmp"
    | "png"
      -> Photo ext
    | "wmv" | "nef" | "avi"
    | "mov" | "mp4" | "mts"
    | "3gp" | "mod" | "vob"
      -> Video ext
    | "doc" | "docx" | "rtf"
    | "txt" | "odt"
      -> Text ext
    | "xls" | "xlsx" | "xlsm"
      -> Excel ext
    | "ppt" | "pptx"
      -> Slide ext
    | "css" | "gif" | "htm" 
    | "html" | "js" | "mht"
      -> Web ext
    | "pdf" | "tiff"
      -> Book ext
    | _ -> Other ext

  let showMediaType =
     function
     | Photo _   -> "#photo"
     | Video _   -> "#video"
     | Text  _   -> "#text"
     | Excel _   -> "#excel"
     | Web   _   -> "#web"
     | Slide _   -> "#slide"
     | Book  _   -> "#book"
     | Other ext -> ext

  type FileInfo with
    member this.MediaType = 
     this.Extension.ToLowerInvariant().Replace(".","") |> mediaType
