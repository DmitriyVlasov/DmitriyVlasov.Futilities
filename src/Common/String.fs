[<RequireQualifiedAccess>]
module DmitriyVlasov.String

/// <summary>
///  Возвращает копию строки, преобразуя в нижний регистр, с использование правила приведения инвариантной культуры. 
/// </summary>
/// <param name="str"></param>
/// <returns></returns>
let inline toLower (str : string) : string =
    str.ToLowerInvariant ()

/// Обвязка для функции функции <c>Replace</c> объекта: <seealso cref="System.String"/>
let replace oldValue newValue (str : string) = 
  str.Replace(oldValue = oldValue, newValue = newValue)

/// Возвращает строковый массив без пустых элементов, содержащий подстроки данной строки, разделенные элементами заданного массива строк.
let splitMany (separators : string[]) (str : string) = 
  str.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)
    
/// Возвращает строковый массив без пустых элементов, содержащий подстроки данной строки, разделенные заданным элементом.
let split (separator : string) (str : string) = 
  splitMany [|separator|] str

/// <summary> Возвращает строку содержащую первый символ заданной строки. </summary>
/// <exception cref="System.ArgumentNullException"></exception>
/// <exception cref="System.ArgumentException"></exception>
let head (str : string) = 
  str |> Seq.head |> string
        
/// Возвращает некоторую строку содержащую первый символ заданной строки. Если строка пустая возвращает None.
let tryHead (str : string) =
  try
    head str |> Some
  with 
    | :? System.ArgumentNullException
    | :? System.ArgumentException -> None