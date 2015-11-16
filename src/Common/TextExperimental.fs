namespace DmitriyVlasov.Experimental

[<Experimental("Эксперементальные функции обработки текста")>]
module Text = 
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

  // Транслитерация
  // ========================================================================
  type private RuLetter = string
  type private EnLetter = {
    Normal  : string
    First   : string option
    Initial : string option}

  /// <summary>
  /// Транслитерация текста с русского в английский.
  /// </summary>
  /// <param name="text"></param>
  let tr (text:RuLetter) =
    let dic:Map<RuLetter,EnLetter> = 
      Map.ofArray [|
        // строчные
        "а", { Normal="a";   First=None;      Initial=None     }
        "б", { Normal="b";   First=None;      Initial=None     }
        "в", { Normal="v";   First=None;      Initial=None     }
        "г", { Normal="g";   First=None;      Initial=None     }
        "д", { Normal="d";   First=None;      Initial=None     }
        "е", { Normal="e";   First=None;      Initial=None     }
        "ё", { Normal="e";   First=None;      Initial=None     }
        "ж", { Normal="zh";  First=None;      Initial=Some "z" }
        "з", { Normal="z";   First=None;      Initial=None     }
        "и", { Normal="i";   First=None;      Initial=None     }
        "й", { Normal="y";   First=None;      Initial=None     }
        "к", { Normal="k";   First=None;      Initial=None     }
        "л", { Normal="l";   First=None;      Initial=None     }
        "м", { Normal="m";   First=None;      Initial=None     }
        "н", { Normal="n";   First=None;      Initial=None     }
        "о", { Normal="o";   First=None;      Initial=None     }
        "п", { Normal="p";   First=None;      Initial=None     }
        "р", { Normal="r";   First=None;      Initial=None     }
        "с", { Normal="s";   First=None;      Initial=None     }
        "т", { Normal="t";   First=None;      Initial=None     }
        "у", { Normal="u";   First=None;      Initial=None     }
        "ф", { Normal="f";   First=None;      Initial=None     }
        "х", { Normal="h";   First=Some "kh"; Initial=None     }
        "ц", { Normal="ts";  First=None;      Initial=Some "c" }
        "ч", { Normal="ch";  First=None;      Initial=Some "c" }
        "ш", { Normal="sh";  First=None;      Initial=Some "s" }
        "щ", { Normal="sch"; First=None;      Initial=Some "s" }
        "ъ", { Normal="";    First=None;      Initial=None     }
        "ы", { Normal="y";   First=None;      Initial=None     }
        "ь", { Normal="";    First=None;      Initial=None     }
        "э", { Normal="e";   First=None;      Initial=None     }
        "ю", { Normal="yu";  First=None;      Initial=Some "y" }
        "я", { Normal="ya";  First=None;      Initial=Some "y" }
        // ПРОПИСНЫЕ
        "А", { Normal="A";   First=None;      Initial=None     }
        "Б", { Normal="B";   First=None;      Initial=None     }
        "В", { Normal="V";   First=None;      Initial=None     }
        "Г", { Normal="G";   First=None;      Initial=None     }
        "Д", { Normal="D";   First=None;      Initial=None     }
        "Е", { Normal="E";   First=None;      Initial=None     }
        "Ё", { Normal="E";   First=None;      Initial=None     }
        "Ж", { Normal="Zh";  First=None;      Initial=Some "Z" }
        "З", { Normal="Z";   First=None;      Initial=None     }
        "И", { Normal="I";   First=None;      Initial=None     }
        "Й", { Normal="Y";   First=None;      Initial=None     }
        "К", { Normal="K";   First=None;      Initial=None     }
        "Л", { Normal="L";   First=None;      Initial=None     }
        "М", { Normal="M";   First=None;      Initial=None     }
        "Н", { Normal="N";   First=None;      Initial=None     }
        "О", { Normal="O";   First=None;      Initial=None     }
        "П", { Normal="P";   First=None;      Initial=None     }
        "Р", { Normal="R";   First=None;      Initial=None     }
        "С", { Normal="S";   First=None;      Initial=None     }
        "Т", { Normal="T";   First=None;      Initial=None     }
        "У", { Normal="U";   First=None;      Initial=None     }
        "Ф", { Normal="F";   First=None;      Initial=None     }
        "Х", { Normal="H";   First=Some "Kh"; Initial=None     }
        "Ц", { Normal="Ts";  First=None;      Initial=Some "C" }
        "Ч", { Normal="Ch";  First=None;      Initial=Some "C" }
        "Ш", { Normal="Sh";  First=None;      Initial=Some "S" }
        "Щ", { Normal="Sch"; First=None;      Initial=Some "S" }
        "Ъ", { Normal="";    First=None;      Initial=None     }
        "Ы", { Normal="Y";   First=None;      Initial=None     }
        "Ь", { Normal="";    First=None;      Initial=None     }
        "Э", { Normal="E";   First=None;      Initial=None     }
        "Ю", { Normal="Yu";  First=None;      Initial=Some "Y" }
        "Я", { Normal="Ya";  First=None;      Initial=Some "Y" }
      |]

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