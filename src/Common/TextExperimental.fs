namespace DmitriyVlasov.Experimental.Text

module Common = 
  open System
  open System.Text
  open DmitriyVlasov

  // TODO: См. дополнительно хорошую статью с примерами по использованию активных шаблонов для обработки выходных параметров .Net: http://luketopia.net/2014/02/05/fsharp-and-output-parameters/
  
  /// Преобразует строковое представление числа в эквивалентное ему 32-битовое знаковое целове число.
  let (|Integer|_|) (str: string) =
     match Int32.TryParse(str) with
     | (true, value) -> Some value
     | _             -> None

  /// Преобрзовать строку в формат даты/времени. Поддерживаются форматы: mm.dd.yy, mm.dd.yyyy, yyyy-mm-dd, dd.mm.yyyy 
  let parseDate str =
     match str with
       | Regex "(\d{1,2})/(\d{1,2})/(\d{1,2})$"  [Integer m; Integer d; Integer y]
            -> new System.DateTime(y + 2000, m, d)
       | Regex "(\d{1,2})/(\d{1,2})/(\d{3,4})"   [Integer m; Integer d; Integer y]
       | Regex "(\d{1,4})-(\d{1,2})-(\d{1,2})"   [Integer y; Integer m; Integer d]
       | Regex "(\d{1,2})\.(\d{1,2})\.(\d{1,4})" [Integer d; Integer m; Integer y]
            -> new System.DateTime(y, m, d)
       | _ -> failwith "Ошибка преобразования даты"

  /// Возвращает максмальную ширину столбца в последовательности.
  let maxWidthColumn getColumn seq = 
    seq 
    |> Seq.map getColumn 
    |> Seq.maxBy String.length
    |> String.length

  /// Показывает таблицу кодовых страниц
  let showEncodings (eis: EncodingInfo seq) =
    let init i = String.init i (fun _ -> "-")
    let codePageWidth = 
      eis |> maxWidthColumn (fun x -> x.CodePage |> string)
    let nameWidth = 
      eis |> maxWidthColumn (fun x -> x.Name)
    let displayNameWidth = 
      eis |> maxWidthColumn (fun x -> x.DisplayName)
    printfn "|%-*s|%-*s|%-*s|" codePageWidth "Code" nameWidth "Name" displayNameWidth "DisplayName" 
    printfn "|%*s+%*s+%*s|" codePageWidth (init codePageWidth) nameWidth (init nameWidth) displayNameWidth (init displayNameWidth)
    eis
    |> Seq.iter ( fun ei -> 
      printfn "|%*i|%-*s|%-*s|" codePageWidth ei.CodePage nameWidth ei.Name displayNameWidth ei.DisplayName )

  type Encoding with
    /// Показывает таблицу кодовых страниц
    static member ShowEncodings = showEncodings


  /// <summary>Очищает строку от Unicode-символов форматирования.</summary>
  /// <remarks>
  /// <para>Пример использования:</para> 
  /// <para>Очистка текстов статей SharePoint от непечатных символов.
  /// При сохранении статей SharePoint автоматически конвертирует статью в HTML формат,
  /// иногда вставляя и текст статьи непечатные символы. При использовании текстов из статей Share Point,
  /// например копировании примера исходного кода из статьи в MSSMS и последующем исполнении или компиляции
  /// выходит сообщение об ошибке.</para>
  /// </remarks>
  let clearFormatSymbols (str:string) = 
    let clear c = 
      match Char.GetUnicodeCategory c with
      | Globalization.UnicodeCategory.Format -> ""
      | _ -> string c
    String.collect clear str

module Translit = 
  open FSharp.Data
  open ExtCore

  type TransliterationTable = JsonProvider<"../../data/TransliterationTable.json">
  let transliterationTable = TransliterationTable.GetSamples()

  // Транслитерация
  // ========================================================================
  type private EnLetter = {
    Normal  : string
    First   : string option
    Initial : string option}

  /// <summary>
  /// Транслитерация текста с русского в английский.
  /// </summary>
  /// <param name="text"></param>
  let run (text:string) =
    let dic:Map<string,EnLetter> = 
      transliterationTable
      |> Array.map (fun letter -> letter.Ru, {
           Normal  = letter.En.Normal |> Option.fill ""
           First   = letter.En.First
           Initial = letter.En.Initial
          } )
      |> Map.ofArray

    let toLatin i letter = 
      match Map.tryFind letter dic, i with
      | Some x, 0 -> 
        match x.First with 
        | Some y -> y 
        | None -> x.Normal
      | Some x, _ -> x.Normal
      | None,   _ -> letter

    let toLatinInitial _ letter = 
      match Map.tryFind letter dic with 
      | Some x -> 
        match x.Initial with
        | Some y -> y
        | None   -> x.Normal
      | None -> letter
    
    let demount = Array.ofSeq >> Array.map string
    let mount   = String.concat ""
    let convert xs = 
      match Array.length xs with
      | 0 -> [|""|]
      | 1 -> Array.mapi toLatinInitial xs
      | _ -> Array.mapi toLatin        xs

    text
    |> demount
    |> convert
    |> mount

module WordCounter = 
  open DmitriyVlasov

  open System.Text.RegularExpressions

  type T = {
    Word        : string
    FirstLetter : string
    WordLength  : int
    WordQty     : int }

  let showWordCounter wc =
    sprintf "%s\t%s\t%d\t%d" wc.FirstLetter wc.Word wc.WordLength wc.WordQty

  let wordCounter spliterArray filterPredicate text =
    text
    |> String.toLower
    |> String.splitMany spliterArray
    |> Array.filter filterPredicate
    |> Array.groupBy id
    |> Array.map (snd >> Array.countBy id)
    |> Array.concat
    |> Array.map (fun (word, qty) -> 
        {FirstLetter = String.head word
         Word        = word
         WordLength  = String.length word
         WordQty     = qty} )

  let sqlSpliterArray = [|
      "\r\n"; "\n"; "\t"
      " "; "["; "]"; "("; ")"
      "\""; ","; "."; ";"; ":"
      "*"; "#"; "_"; "/"; "`" 
      ">"; "="; "-"; "^"; "|"
      |]
  let sqlFilterPredicate str = 
      Regex.IsMatch(str, @"^[a-z']+$") &&
      String.length str >= 2 

  let sqlWordCounter sql = 
    wordCounter sqlSpliterArray sqlFilterPredicate sql