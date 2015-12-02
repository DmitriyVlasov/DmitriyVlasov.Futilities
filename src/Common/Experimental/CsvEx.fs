namespace DmitriyVlasov.Data.Experimental

[<Experimental("Библиотека экспериментальная обработки CSV файлов.")>]
module Csv = 
  open DmitriyVlasov
  open DmitriyVlasov.Collections

  /// Представление файла CSV
  type Header = Header of string []
  type Table  = Table  of string [,]
  type Column = Column of string []
  type Csv    = Csv of Header option * Table

  /// Загрузить строку @text в таблицу, используя разделитель @sep
  let readBySep (sep:string) (withHeader:bool) (text:string) = 
    let data = 
      try 
        text
        |> String.splits [|"\n"|]
        |> Array.map (String.splits [|sep|])
        |> array2D
      with 
        | :? System.ArgumentException
        | :? System.IndexOutOfRangeException as e -> 
          let message = "Используйте полностью заполненные файлы CSV или доработайте модуль CSV.fs"
          printfn "%s\n%s\nСистемная ошибка:\n<%s>" (text.Remove(1000)) message e.Message
          Array2D.zeroCreate 1 1 
    let h  = Header <| Array2D.toArray data.[0..0,0..]
    let t  = Table  <| data.[1..,0..]
    let t' = Table  <| data
    match withHeader with
    | true -> Csv ( Some h, t  )
    | _    -> Csv ( None  , t' )

  /// Загрузить строку @text в таблицу, используя табуляцию.
  let read (withHeader:bool) (text:string)  = readBySep "\t" withHeader text

  /// Cтолбец из таблицы @table по номеру @pos.
  /// Нумерация столбцов в таблице начинается с нуля.
  let column (pos:int) (Csv (_,(Table data))) = 
      Column <| Array2D.toArray data.[0..,pos..pos]

  /// Преобразовать столбец в строку с разделителями.
  let columnShow (sep:string) (Column data) = 
      data |> Array.toSeq |> String.concat sep

  /// Разница разницу между столбцами A и B.
  let columnDiff (Column colA) (Column colB) = 
      Set.difference (Set.ofArray colA) (Set.ofArray colB) |> Set.toArray |> Column

  /// Оставляет в столбце только уникальные значения
  let columnDistinct (Column value) = value |> Seq.distinct |> Seq.toArray |> Column
    
  /// Применяет функцию к столбцу
  let columnMap f (Column value) = value |> Array.map f |> Column

  // TODO Создать массив строк выгрузки длинной не более n символов
  // TODO Создать массив строк выгрузки количеством не более m элементов.



