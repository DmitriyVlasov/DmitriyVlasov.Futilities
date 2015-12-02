namespace DmitriyVlasov.Collections

[<RequireQualifiedAccess>]
module Array =
  /// Возвращает true если все элементы массива одинаковые, иначе false.
  let equal array =
    array |> Array.distinct |> Array.length |> ((=) 1)

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
    list |> List.distinct |> List.length |> ((=) 1)

[<RequireQualifiedAccess>]
module Seq = 
  /// Возвращает true если все элементы последовательности одинаковые, иначе false.
  let equal seq =
    seq |> Seq.distinct |> Seq.length |> ((=) 1)