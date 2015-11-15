[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.Seq
open DmitriyVlasov

/// Возвращает true если все элементы последовательности одинаковые, иначе false.
let equal seq =
  Seq.fold previousAndCurrentEqual (true, Seq.head seq) seq |> fst