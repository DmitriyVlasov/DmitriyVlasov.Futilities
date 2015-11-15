namespace DmitriyVlasov

[<RequireQualifiedAccess>]
module DateTime =
  /// Пустая дата с точки зрения Navision.
  let empty = new System.DateTime(1753,1,1)

  /// Дата год назад от текущей
  let yearAgo = System.DateTime.Now.AddYears(-1)

  /// Преобразовать дату время в краткий формат даты без времени.
  let toString (dt:System.DateTime) = dt.ToShortDateString()

[<RequireQualifiedAccess>]
module Month =
  /// Первый день месяца
  let firstDay (dt:System.DateTime) = 
    System.DateTime(dt.Year, dt.Month, 1)
  
  /// Последний день месяца
  let lastDay (dt:System.DateTime) = 
    (firstDay dt).AddMonths(1).AddDays(-1.0)
    
  /// Предыдущий месяц
  let previous (dt:System.DateTime) = 
    dt.AddMonths(-1)

  /// Имя месяца
  let getName month = 
    match month with
    |  1 -> "январь"
    |  2 -> "февраль"
    |  3 -> "март"
    |  4 -> "апрель"
    |  5 -> "май"
    |  6 -> "июнь"
    |  7 -> "июль"
    |  8 -> "август"
    |  9 -> "сентябрь"
    | 10 -> "октябрь"
    | 11 -> "ноябрь"
    | 12 -> "декабрь"
    | _  -> ""