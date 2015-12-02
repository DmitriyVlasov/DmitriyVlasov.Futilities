namespace DmitriyVlasov

[<AutoOpen>]
module Patterns =
  /// Активный шаблон для сравления текста по регулярным выражениям.
  let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None