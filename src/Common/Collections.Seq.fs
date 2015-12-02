[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.Seq
open DmitriyVlasov

/// Возвращает true если все элементы последовательности одинаковые, иначе false.
let equal seq =
  seq |> Seq.distinct |> Seq.length |> ((=) 1)