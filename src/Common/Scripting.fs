namespace DmitriyVlasov

module Scripting = 

  /// Возвращает кодировку, связанную указанным идентификатором кодовой страницы.
  /// <param name="codepage"> Идентификатор кодовой страницы предпочтительной подировки.
  /// возможные значения перечисленны в столбце кодовой стринцы таблицы, которая отображается 
  /// в теме класса System.Text.Encoding. Или 0 (ноль) если требуется использовать кодировку по умолчанию.</param>
  let encoding codepage = 
    System.Text.Encoding.GetEncoding(codepage = codepage)

  /// Задает путь к текущей рабочей папке.
  let cd dirPath =
    System.Environment.CurrentDirectory <- dirPath

  /// Создает все каталоги и подкаталоги по указанному пути, если они еще не существуют.
  let mk dirPath = 
    System.IO.Directory.CreateDirectory(dirPath)

  /// Возвращает имена файлов (включая пути) в заданном каталоге,
  /// отвечающие условиям шаблона поиска.
  /// Поиск осуществляется включая подкаталоги.
  let find searchOption searchPattern dirPath =
    System.IO.Directory.EnumerateFiles(dirPath, searchPattern, searchOption)
    
  /// Возвращает имена файлов (включая пути) в заданном каталоге,
  /// отвечающие условиям шаблона поиска, используя значение, которое определяет,
  /// выполнять ли поиск в подкаталогах.
  let ls dirPath = 
    find System.IO.SearchOption.TopDirectoryOnly "*.*" dirPath
  
  /// Объединяет две строки в путь.
  let (++) path1 path2 = 
    System.IO.Path.Combine(path1, path2)
  
  /// Возвращат имя файла и расширение указанной строки пути.
  let filename filePath = 
    System.IO.Path.GetFileName(filePath)
  
  /// Возвращает имя файла указанной строки пути без расширения.
  let basename filePath =
    System.IO.Path.GetFileNameWithoutExtension(filePath)
  
  /// Возвращает для указанной строки пути имя каталога.
  let dirname filePath = 
    System.IO.Path.GetDirectoryName(filePath)
  
  /// Возвращает для указанной строки пути абсолютный путь.
  let fullpath filePath =
    System.IO.Path.GetFullPath(filePath)
  
  /// Возвращает для указанной строки пути имя корневого каталога.
  let rootdir filePath = 
    System.IO.Path.GetPathRoot(filePath)
  
  /// Возвращает расширение для указанной строки пути.
  let extension filePath =
    System.IO.Path.GetExtension(filePath)
  
  /// Изменяет расширение строки пути.
  let changext ext filePath 
    = System.IO.Path.ChangeExtension(filePath,ext)

  /// Открывает файл, добавляет в него указанную строку и затем закрывает файл.
  /// Если файл не существует, эта функция создает файл, записывает в него указанную строку
  /// и затем закрывает файл.
  let (.>>) contents filePath =
    System.IO.File.AppendAllText(filePath, contents)
  
  /// Создает новый файл, записывает в него указанную строку и затем закрывает файл.
  /// Если целевой файл уже существует, он будет переопределен.
  let (.>) contents filePath =
    System.IO.File.WriteAllText(filePath, contents)
  
  /// Перемещает файл в новое местоположение и разрешает переименование файла.
  let mv sourceFilePath destFilePath = 
    System.IO.File.Move(sourceFilePath, destFilePath)
  
  /// Создает новый файл, записывает в него указанную строку и затем закрывает файл.
  /// Если целевой файл уже существует, он будет переопределен.
  let writeText filePath contents =
    contents .> filePath
  
  /// Открывает файл, считывает все строки файла с заданной кодовой страницей и затем закрывает файл.
  let readText codepage filePath =
    System.IO.File.ReadAllText(filePath, encoding codepage)
  
  /// Считывает сроки файла с заданной кодовой страницей.
  let lines codepage filePath = 
    System.IO.File.ReadLines(filePath, encoding codepage)

  /// Определяет, существует ли заданный файл.
  let fileExists filePath = 
    try 
      System.IO.File.Exists filePath
    with _ -> false
  
  /// Записывает текстовую строку, за которой следует признак конца строки в выходной буфер.
  let echo (text:string) =
    System.Console.Out.WriteLine(text)
  
  /// Выводит файл в заданной кодовой страницей в выходной буфер.
  let cat codepage filePath = 
    filePath 
    |> lines codepage 
    |> Seq.iter System.Console.Out.WriteLine
