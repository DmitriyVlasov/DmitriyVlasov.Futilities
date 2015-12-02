[<RequireQualifiedAccess>]
module DmitriyVlasov.String

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