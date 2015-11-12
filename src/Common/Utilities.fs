namespace Common

[<AutoOpen>]
module ExtraTopLevelOperators =

  /// Функция изменяет параметры местами для применяемой функции.
  let inline flip f x y = f y x

  /// Определяет равны ли предыдующее и текущее значение в последовательности.
  /// Используется для функций обработки списков, массивов, последовательностей.
  let inline previousAndCurrentEqual (state, previous) current = 
    match state with
    | true  -> previous = current, current 
    | false -> false, current
    
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

  /// Возвращает кодировку, связанную указанным идентификатором кодовой страницы.
  /// <param name="codepage"> Идентификатор кодовой страницы предпочтительной подировки.
  /// возможные значения перечисленны в столбце кодовой стринцы таблицы, которая отображается 
  /// в теме класса System.Text.Encoding. Или 0 (ноль) если требуется использовать кодировку по умолчанию.</param>
  let encoding codepage = 
    System.Text.Encoding.GetEncoding(codepage = codepage)

  /// Задает путь к текущей рабочей папке.
  let cd dirPath =
    System.Environment.CurrentDirectory <- dirPath

  /// Создает все каталоги и подкаталоги по указанному пути, если они еще не существуют.
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
  
  /// Объединяет две строки в путь.
  let (++) path1 path2 = 
    System.IO.Path.Combine(path1, path2)
  
  /// Возвращат имя файла и расширение указанной строки пути.
  let filename filePath = 
    System.IO.Path.GetFileName(filePath)
  
  /// Возвращает имя файла указанной строки пути без расширения.
  let basename filePath =
    System.IO.Path.GetFileNameWithoutExtension(filePath)
  
  /// Возвращает для указанной строки пути имя каталога.
  let dirname filePath = 
    System.IO.Path.GetDirectoryName(filePath)
  
  /// Возвращает для указанной строки пути абсолютный путь.
  let fullpath filePath =
    System.IO.Path.GetFullPath(filePath)
  
  /// Возвращает для указанной строки пути имя корневого каталога.
  let rootdir filePath = 
    System.IO.Path.GetPathRoot(filePath)
  
  /// Возвращает расширение для указанной строки пути.
  let extension filePath =
    System.IO.Path.GetExtension(filePath)
  
  /// Изменяет расширение строки пути.
  let changext ext filePath 
    = System.IO.Path.ChangeExtension(filePath,ext)

  /// Открывает файл, добавляет в него указанную строку и затем закрывает файл.
  /// Если файл не существует, эта функция создает файл, записывает в него указанную строку
  /// и затем закрывает файл.
  let (.>>) contents filePath =
    System.IO.File.AppendAllText(filePath, contents)
  
  /// Создает новый файл, записывает в него указанную строку и затем закрывает файл.
  /// Если целевой файл уже существует, он будет переопределен.
  let (.>) contents filePath =
    System.IO.File.WriteAllText(filePath, contents)
  
  /// Перемещает файл в новое местоположение и разрешает переименование файла.
  let mv sourceFilePath destFilePath = 
    System.IO.File.Move(sourceFilePath, destFilePath)
  
  /// Создает новый файл, записывает в него указанную строку и затем закрывает файл.
  /// Если целевой файл уже существует, он будет переопределен.
  let writeText filePath contents =
    contents .> filePath
  
  /// Открывает файл, считывает все строки файла с заданной кодовой страницей и затем закрывает файл.
  let readText codepage filePath =
    System.IO.File.ReadAllText(filePath, encoding codepage)
  
  /// Считывает сроки файла с заданной кодовой страницей.
  let lines codepage filePath = 
    System.IO.File.ReadLines(filePath, encoding codepage)

  /// Определяет, существует ли заданный файл.
  let fileExists filePath = 
    try 
      System.IO.File.Exists filePath
    with _ -> false
  
  /// Записывает текстовую строку, за которой следует признак конца строки в выходной буфер.
  let echo (text:string) =
    System.Console.Out.WriteLine(text)
  
  /// Выводит файл в заданной кодовой страницей в выходной буфер.
  let cat codepage filePath = 
    filePath 
    |> lines codepage 
    |> Seq.iter System.Console.Out.WriteLine

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
    List.fold previousAndCurrentEqual (true, List.head list) list |> fst
    
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
    Array.fold previousAndCurrentEqual (true, Array.get array 0) array |> fst

[<RequireQualifiedAccess>]
module Seq = 
  /// Возвращает true если все элементы последовательности одинаковые, иначе false.
  let equal seq =
    Seq.fold previousAndCurrentEqual (true, Seq.head seq) seq |> fst

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

[<RequireQualifiedAccess>]
module Text =
  /// Активный шаблон для сравления текста по регулярным выражениям.
  let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

[<RequireQualifiedAccess>]
module DateTime =
  /// Пустая дата с точки зрения Navision.
  let empty = new System.DateTime(1753,1,1)

  /// Дата год назад от текущей
  let yearAgo = System.DateTime.Now.AddYears(-1)

  /// Преобразовать дату время в краткий формат даты без времени.
  let toString (dt:System.DateTime) = dt.ToShortDateString()

[<RequireQualifiedAccess>]
module Month =
  /// Первый день месяца
  let firstDay (dt:System.DateTime) = 
    System.DateTime(dt.Year, dt.Month, 1)
  
  /// Последний день месяца
  let lastDay (dt:System.DateTime) = 
    (firstDay dt).AddMonths(1).AddDays(-1.0)
    
  /// Предыдущий месяц
  let previous (dt:System.DateTime) = 
    dt.AddMonths(-1)

  /// Имя месяца
  let getName month = 
    match month with
    | 1 -> "январь"
    | 2 -> "февраль"
    | 3 -> "март"
    | 4 -> "апрель"
    | 5 -> "май"
    | 6 -> "июнь"
    | 7 -> "июль"
    | 8 -> "август"
    | 9 -> "сентябрь"
    | 10 -> "октябрь"
    | 11 -> "ноябрь"
    | 12 -> "декабрь"
    | _  -> ""