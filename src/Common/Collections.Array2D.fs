[<RequireQualifiedAccess>]
module DmitriyVlasov.Collections.Array2D

// http://stackoverflow.com/questions/12870368/array2d-to-array
/// Преобразовать (flatten) двухмерный массив в одномерный.
let toArray (arr: 'T [,]) =
  arr |> Seq.cast<'T> |> Seq.toArray
