module DmitriyVlasov.Experimental.Collections.List

/// Группирует список на подсписки согласно предикату. Порядок элементов сохраняется во всех созданных подсписках.
/// Идея и реализация функции заимствована функции groupBy языка Haskell в модуле Data.List.
let rec hsGroupBy predicate list =
  match list with
  | [] -> []
  | (x::xs) -> 
    let ys, zs =
      List.partition (predicate x) xs
    (x::ys) :: hsGroupBy predicate zs