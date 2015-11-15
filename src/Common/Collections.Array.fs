[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.Array
open DmitriyVlasov

/// Возвращает true если все элементы массива одинаковые, иначе false.
let equal array =
  Array.fold previousAndCurrentEqual (true, Array.get array 0) array |> fst

