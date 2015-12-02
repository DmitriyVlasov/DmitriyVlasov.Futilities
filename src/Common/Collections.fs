namespace DmitriyVlasov.Collections

[<RequireQualifiedAccess>]
module Array =
  /// Возвращает true если все элементы массива одинаковые, иначе false.
  let equal array =
    array |> Array.distinct |> Array.length |> ((=) 1)

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