namespace DmitriyVlasov

[<AutoOpen>]
module Operators =
  /// Определяет равны ли предыдующее и текущее значение в последовательности.
  /// Используется для функций обработки списков, массивов, последовательностей.
  let inline previousAndCurrentEqual (state, previous) current = 
    match state with
    | true  -> previous = current, current 
    | false -> false, current

[<AutoOpen>]
module Patterns =
  /// Активный шаблон для сравления текста по регулярным выражениям.
  let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None