[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.Array
open DmitriyVlasov

/// Возвращает true если все элементы массива одинаковые, иначе false.
let equal array =
  array |> Array.distinct |> Array.length |> ((=) 1)