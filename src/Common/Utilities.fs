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

  /// Возвращает кодировку, связанную указанным идентификатором кодовой страницы.
  /// <param name="codepage"> Идентификатор кодовой страницы предпочтительной подировки.
  /// возможные значения перечисленны в столбце кодовой стринцы таблицы, которая отображается 
  /// в теме класса System.Text.Encoding. Или 0 (ноль) если требуется использовать кодировку по умолчанию.</param>
  let encoding codepage = 
    System.Text.Encoding.GetEncoding(codepage = codepage)

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
  
  /// Открывает файл, считывает все строки файла с заданной кодовой страницей и затем закрывает файл.
  let readText codepage filePath =
    System.IO.File.ReadAllText(filePath, encoding codepage)
  
  /// Считывает сроки файла с заданной кодовой страницей.
  let lines codepage filePath = 
    System.IO.File.ReadLines(filePath, encoding codepage)

  ///
  let fileExists filePath = 
    try 
      System.IO.File.Exists filePath
    with _ -> false
  
  ///
  let echo (text:string) =
    System.Console.Out.WriteLine(text)
  
  ///
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