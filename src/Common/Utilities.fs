namespace Incubator.Common

[<AutoOpen>]
module ExtraTopLevelOperators =

  /// Функция изменяет параметры местами для применяемой функции.
  let inline flip f x y = f y x

  ///
  let inline isTwins (state, x1) x2 = 
    match state with
    | true  -> x1 = x2, x2 
    | false -> false, x2

  /// Активный шаблон. Принимает пару: логическое состояние результата вычисление и значение.
  /// Если состояние вычисление - истина возвращает "Success значение", иначе возвращает "Failure".
  let (|Success|Failure|) (success, value) =
    match (success, value) with
    | true, value -> Success value
    | _ -> Failure
(** 
// Активный шаблон (|Success|Failure|) заимствован из статьи (F# and Output Parameters)[http://luketopia.net/2014/02/05/fsharp-and-output-parameters]

Пример использования: 
let intStr = "10"

match Int32.TryParse intStr with
| Success value -> printfn "It's the number %d." value
| Failure -> printfn "It's not a number."

*)
   
///
module Scripting =

  ///
  let cd dirPath =
    System.Environment.CurrentDirectory <- dirPath

  ///
  let mk dirPath = 
    System.IO.Directory.CreateDirectory(dirPath)

  /// Возвращает имена файлов (включая пути) в заданном каталоге,
  /// отвечающие условиям шаблона поиска.
  /// Поиск осуществляется включая подкаталоги.
  let find searchOption searchPattern dirPath =
    System.IO.Directory.EnumerateFiles(dirPath, searchPattern, searchOption)
    
  /// Возвращает имена файлов (включая пути) в заданном каталоге,
  /// отвечающие условиям шаблона поиска, используя значение, которое определяет,
  /// выполнять ли поиск в подкаталогах.
  let ls dirPath = 
    find System.IO.SearchOption.TopDirectoryOnly "*.*" dirPath
  
  ///
  let (++) a v = 
    System.IO.Path.Combine(a, v)
  
  ///
  let filename filePath = 
    System.IO.Path.GetFileName(filePath)
  
  ///
  let basename filePath =
    System.IO.Path.GetFileNameWithoutExtension(filePath)
  
  ///
  let dirname filePath = 
    System.IO.Path.GetDirectoryName(filePath)
  
  ///
  let fullpath filePath =
    System.IO.Path.GetFullPath(filePath)
  
  ///
  let rootdir filePath = 
    System.IO.Path.GetPathRoot(filePath)
  
  ///
  let extension filePath =
    System.IO.Path.GetExtension(filePath)
  
  ///
  let changext ext filePath 
    = System.IO.Path.ChangeExtension(filePath,ext)

  ///
  let (.>>) contents filePath =
    System.IO.File.AppendAllText(filePath, contents)
  
  ///
  let (.>) contents filePath =
    System.IO.File.WriteAllText(filePath, contents)
  
  ///
  let mv sourceFilePath destFilePath = 
    System.IO.File.Move(sourceFilePath, destFilePath)
  
  ///
  let writeText filePath contents =
    contents .> filePath
  
  ///
  let readText filePath = 
    System.IO.File.ReadAllText(filePath)
  
  ///
  let lines filePath =
    System.IO.File.ReadAllLines(filePath)
  
  ///
  let linesAsSeq filePath =
    System.IO.File.ReadLines(filePath)
  
  ///
  let fileExists filePath = 
    try 
      System.IO.File.Exists filePath
    with _ -> false
  
  ///
  let echo (text:string) =
    System.Console.Out.WriteLine(text)
  
  ///
  let cat filePath = 
    filePath 
    |> linesAsSeq 
    |> Seq.iter System.Console.Out.WriteLine

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
  let findByContent searchString filePaths =
    let condition (line:string) = 
      line.IndexOf(value = searchString, comparisonType = System.StringComparison.OrdinalIgnoreCase) >= 0
    let foundLinesOfFile = linesAsSeq >> Seq.filter condition
    // TODO: Показывать контекст поиска: +-1 строка выше и ниже найденного вхождения.               
    filePaths
    |> Seq.map (fun path -> path, foundLinesOfFile path )
    |> Seq.filter (fun (_, foundLines) -> foundLines |> Seq.length > 0)

[<RequireQualifiedAccess>]
module Array2D =
  // http://stackoverflow.com/questions/12870368/array2d-to-array
  /// Преобразовать (flatten) двухмерный массив в одномерный.
  let toArray (arr: 'T [,]) =
    arr |> Seq.cast<'T> |> Seq.toArray

[<RequireQualifiedAccess>]
module List =
  /// Возвращает true если все элементы списка одинаковые, иначе false.
  let equal list =
    List.fold isTwins (true, List.head list) list |> fst
    
  /// Группирует список на подсписки согласно предикату. Порядок элементов сохраняется во всех созданных подсписках.
  /// Идея и реализация функции заимствована функции groupBy языка Haskell в модуле Data.List.
  let rec hsGroupBy predicate list =
    match list with
    | [] -> []
    | (x::xs) -> 
      let ys, zs =
        List.partition (predicate x) xs
      (x::ys) :: hsGroupBy predicate zs

[<RequireQualifiedAccess>]
module Array = 
  /// Возвращает true если все элементы массива одинаковые, иначе false.
  let equal array =
    Array.fold isTwins (true, Array.get array 0) array |> fst

[<RequireQualifiedAccess>]
module Seq = 
  /// Возвращает true если все элементы последовательности одинаковые, иначе false.
  let equal seq =
    Seq.fold isTwins (true, Seq.head seq) seq |> fst

[<RequireQualifiedAccess>]
module String =

  /// Скопировать строку в буфер обмена Windows.
  let copy text = 
    System.Windows.Forms.Clipboard.SetText text

  /// Получить строку из буфера обмена Windows.
  let paste() = 
    System.Windows.Forms.Clipboard.GetText()    
    
  /// Обвязка для функции функции <c>Replace</c> объекта: <seealso cref="System.String"/>
  let replace oldValue newValue (str:string) = 
    str.Replace(oldValue = oldValue, newValue = newValue)

  /// Возвращает строковый массив без пустых элементов, содержащий подстроки данной строки, разделенные элементами заданного массива строк.
  let splitMany (separators:string[]) (str:string) = 
    str.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)
    
  /// Возвращает строковый массив без пустых элементов, содержащий подстроки данной строки, разделенные заданным элементом.
  let split (separator:string) (str:string) = 
    splitMany [|separator|] str

  /// <summary> Возвращает строку содержащую первый символ заданной строки. </summary>
  /// <exception cref="System.ArgumentNullException"></exception>
  /// <exception cref="System.ArgumentException"></exception>
  let head (str:string) = 
    str |> Seq.head |> string
        
  /// Возвращает некоторую строку содержащую первый символ заданной строки. Если строка пустая возвращает None.
  let tryHead (str:string) =
    try
      head str |> Some
    with 
      | :? System.ArgumentNullException
      | :? System.ArgumentException -> None

module Text =
  /// Активный шаблон для сравления текста по регулярным выражениям.
  let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

  // Транслитерация
  // ========================================================================
  type private ru = string
  type private en = {normal:string; first:string option; initial:string option}

  /// <summary>
  /// Транслитерация текста с русского в английский.
  /// </summary>
  /// <param name="text"></param>
  let tr (text:ru) =
    // TODO Использовать проект Bond для хранения таблицы транслитерации в файле. см. подробнее: https://github.com/microsoft/bond
    let dic:Map<ru,en> = 
      Map.ofArray [|
        // строчные           
        "а", { normal="a";   first=None;      initial=None     }
        "б", { normal="b";   first=None;      initial=None     }
        "в", { normal="v";   first=None;      initial=None     }
        "г", { normal="g";   first=None;      initial=None     }
        "д", { normal="d";   first=None;      initial=None     }
        "е", { normal="e";   first=None;      initial=None     }  
        "ё", { normal="e";   first=None;      initial=None     }   
        "ж", { normal="zh";  first=None;      initial=Some "z" }
        "з", { normal="z";   first=None;      initial=None     }
        "и", { normal="i";   first=None;      initial=None     }
        "й", { normal="y";   first=None;      initial=None     }
        "к", { normal="k";   first=None;      initial=None     }
        "л", { normal="l";   first=None;      initial=None     }
        "м", { normal="m";   first=None;      initial=None     }
        "н", { normal="n";   first=None;      initial=None     }
        "о", { normal="o";   first=None;      initial=None     }
        "п", { normal="p";   first=None;      initial=None     }
        "р", { normal="r";   first=None;      initial=None     }
        "с", { normal="s";   first=None;      initial=None     }
        "т", { normal="t";   first=None;      initial=None     }
        "у", { normal="u";   first=None;      initial=None     }
        "ф", { normal="f";   first=None;      initial=None     }
        "х", { normal="h";   first=Some "kh"; initial=None     }
        "ц", { normal="ts";  first=None;      initial=Some "c" }
        "ч", { normal="ch";  first=None;      initial=Some "c" }
        "ш", { normal="sh";  first=None;      initial=Some "s" }
        "щ", { normal="sch"; first=None;      initial=Some "s" }
        "ъ", { normal="";    first=None;      initial=None     }
        "ы", { normal="y";   first=None;      initial=None     }
        "ь", { normal="";    first=None;      initial=None     }
        "э", { normal="e";   first=None;      initial=None     }
        "ю", { normal="yu";  first=None;      initial=Some "y" }
        "я", { normal="ya";  first=None;      initial=Some "y" }
        // ПРОПИСНЫЕ                                           
        "А", { normal="A";   first=None;      initial=None     }
        "Б", { normal="B";   first=None;      initial=None     }
        "В", { normal="V";   first=None;      initial=None     }
        "Г", { normal="G";   first=None;      initial=None     }
        "Д", { normal="D";   first=None;      initial=None     }
        "Е", { normal="E";   first=None;      initial=None     }
        "Ё", { normal="E";   first=None;      initial=None     }
        "Ж", { normal="Zh";  first=None;      initial=Some "Z" }
        "З", { normal="Z";   first=None;      initial=None     }
        "И", { normal="I";   first=None;      initial=None     }
        "Й", { normal="Y";   first=None;      initial=None     }
        "К", { normal="K";   first=None;      initial=None     }
        "Л", { normal="L";   first=None;      initial=None     }
        "М", { normal="M";   first=None;      initial=None     }
        "Н", { normal="N";   first=None;      initial=None     }
        "О", { normal="O";   first=None;      initial=None     }
        "П", { normal="P";   first=None;      initial=None     }
        "Р", { normal="R";   first=None;      initial=None     }
        "С", { normal="S";   first=None;      initial=None     }
        "Т", { normal="T";   first=None;      initial=None     }
        "У", { normal="U";   first=None;      initial=None     }
        "Ф", { normal="F";   first=None;      initial=None     }
        "Х", { normal="H";   first=Some "Kh"; initial=None     }
        "Ц", { normal="Ts";  first=None;      initial=Some "C" }
        "Ч", { normal="Ch";  first=None;      initial=Some "C" }
        "Ш", { normal="Sh";  first=None;      initial=Some "S" }
        "Щ", { normal="Sch"; first=None;      initial=Some "S" }
        "Ъ", { normal="";    first=None;      initial=None     }
        "Ы", { normal="Y";   first=None;      initial=None     }
        "Ь", { normal="";    first=None;      initial=None     }
        "Э", { normal="E";   first=None;      initial=None     }
        "Ю", { normal="Yu";  first=None;      initial=Some "Y" }
        "Я", { normal="Ya";  first=None;      initial=Some "Y" }
      |]

    let toLatin i letter = 
      match Map.tryFind letter dic, i with
      | Some x, 0 -> 
        match x.first with 
        | Some y -> y 
        | None -> x.normal
      | Some x, _ -> x.normal
      | None,   _ -> letter

    let toLatinInitial _ letter = 
      match Map.tryFind letter dic with 
      | Some x -> 
        match x.initial with
        | Some y -> y
        | None   -> x.normal
      | None -> letter
    
    let demount = Array.ofSeq >> Array.map string
    let mount   = String.concat ""
    let convert xs = 
      match Array.length xs with
      | 0 -> [|""|]
      | 1 -> Array.mapi toLatinInitial xs
      | _ -> Array.mapi toLatin        xs

    text
    |> demount
    |> convert
    |> mount