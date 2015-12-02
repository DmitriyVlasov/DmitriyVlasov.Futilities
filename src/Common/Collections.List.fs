[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.List
open DmitriyVlasov

/// Возвращает true если все элементы списка одинаковые, иначе false.
let equal list =
  list |> List.distinct |> List.length |> ((=) 1)