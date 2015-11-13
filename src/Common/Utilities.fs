namespace DmitriyVlasov

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