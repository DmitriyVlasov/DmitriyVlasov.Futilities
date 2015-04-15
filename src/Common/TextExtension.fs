namespace Incubator.Common.Experimental

[<Experimental("Эксперементальные функции обработки текста")>]
module Text = 
  open System
  open System.Text
  open Incubator.Common

  // TODO: См. дополнительно хорошую статью с примерами по использованию активных шаблонов для обработки выходных параметров .Net: http://luketopia.net/2014/02/05/fsharp-and-output-parameters/
  
  /// Преобразует строковое представление числа в эквивалентное ему 32-битовое знаковое целове число.
  let (|Integer|_|) (str: string) =
     match Int32.TryParse(str) with
     | (true, value) -> Some value
     | _             -> None

  /// Преобрзовать строку в формат даты/времени. Поддерживаются форматы: mm.dd.yy, mm.dd.yyyy, yyyy-mm-dd, dd.mm.yyyy 
  let parseDate str =
     match str with
       | Text.Regex "(\d{1,2})/(\d{1,2})/(\d{1,2})$"  [Integer m; Integer d; Integer y]
            -> new System.DateTime(y + 2000, m, d)
       | Text.Regex "(\d{1,2})/(\d{1,2})/(\d{3,4})"   [Integer m; Integer d; Integer y]
       | Text.Regex "(\d{1,4})-(\d{1,2})-(\d{1,2})"   [Integer y; Integer m; Integer d]
       | Text.Regex "(\d{1,2})\.(\d{1,2})\.(\d{1,4})" [Integer d; Integer m; Integer y]
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


  /// <summary>Очищает текст от непечатных символов.</summary>
  /// <remarks>
  /// <para>Пример использования:</para> 
  /// <para>Очистка текстов статей SharePoint от непечатных символов.
  /// При сохранении статей SharePoint автоматически конвертирует статью в HTML формат,
  /// иногда вставляя и текст статьи непечатные символы. При использовании текстов из статей Share Point,
  /// например копировании примера исходного кода из статьи в MSSMS и последующем исполнении или компиляции
  /// выходит сообщение об ошибке.</para>
  /// </remarks>
  let clearStopSymbols (str:string) = 
    let clear c = 
      match Char.GetUnicodeCategory c with
      | Globalization.UnicodeCategory.Format -> ""
      | _ -> string c
    String.collect clear str

  
  // Транслитерация
  // ========================================================================
  type private ru = string
  type private en = {normal:string; first:string option; initial:string option}

  /// <summary>
  /// Транслитерация текста с русского в английский.
  /// </summary>
  /// <param name="text"></param>
  let tr (text:ru) =
    let dic:Map<ru,en> = 
      Map.ofArray [|
        // строчные           
        "а", { normal="a";   first=None;      initial=None     }
        "б", { normal="b";   first=None;      initial=None     }
        "в", { normal="v";   first=None;      initial=None     }
        "г", { normal="g";   first=None;      initial=None     }
        "д", { normal="d";   first=None;      initial=None     }
        "е", { normal="e";   first=None;      initial=None     }  
        "ё", { normal="e";   first=None;      initial=None     }   
        "ж", { normal="zh";  first=None;      initial=Some "z" }
        "з", { normal="z";   first=None;      initial=None     }
        "и", { normal="i";   first=None;      initial=None     }
        "й", { normal="y";   first=None;      initial=None     }
        "к", { normal="k";   first=None;      initial=None     }
        "л", { normal="l";   first=None;      initial=None     }
        "м", { normal="m";   first=None;      initial=None     }
        "н", { normal="n";   first=None;      initial=None     }
        "о", { normal="o";   first=None;      initial=None     }
        "п", { normal="p";   first=None;      initial=None     }
        "р", { normal="r";   first=None;      initial=None     }
        "с", { normal="s";   first=None;      initial=None     }
        "т", { normal="t";   first=None;      initial=None     }
        "у", { normal="u";   first=None;      initial=None     }
        "ф", { normal="f";   first=None;      initial=None     }
        "х", { normal="h";   first=Some "kh"; initial=None     }
        "ц", { normal="ts";  first=None;      initial=Some "c" }
        "ч", { normal="ch";  first=None;      initial=Some "c" }
        "ш", { normal="sh";  first=None;      initial=Some "s" }
        "щ", { normal="sch"; first=None;      initial=Some "s" }
        "ъ", { normal="";    first=None;      initial=None     }
        "ы", { normal="y";   first=None;      initial=None     }
        "ь", { normal="";    first=None;      initial=None     }
        "э", { normal="e";   first=None;      initial=None     }
        "ю", { normal="yu";  first=None;      initial=Some "y" }
        "я", { normal="ya";  first=None;      initial=Some "y" }
        // ПРОПИСНЫЕ                                           
        "А", { normal="A";   first=None;      initial=None     }
        "Б", { normal="B";   first=None;      initial=None     }
        "В", { normal="V";   first=None;      initial=None     }
        "Г", { normal="G";   first=None;      initial=None     }
        "Д", { normal="D";   first=None;      initial=None     }
        "Е", { normal="E";   first=None;      initial=None     }
        "Ё", { normal="E";   first=None;      initial=None     }
        "Ж", { normal="Zh";  first=None;      initial=Some "Z" }
        "З", { normal="Z";   first=None;      initial=None     }
        "И", { normal="I";   first=None;      initial=None     }
        "Й", { normal="Y";   first=None;      initial=None     }
        "К", { normal="K";   first=None;      initial=None     }
        "Л", { normal="L";   first=None;      initial=None     }
        "М", { normal="M";   first=None;      initial=None     }
        "Н", { normal="N";   first=None;      initial=None     }
        "О", { normal="O";   first=None;      initial=None     }
        "П", { normal="P";   first=None;      initial=None     }
        "Р", { normal="R";   first=None;      initial=None     }
        "С", { normal="S";   first=None;      initial=None     }
        "Т", { normal="T";   first=None;      initial=None     }
        "У", { normal="U";   first=None;      initial=None     }
        "Ф", { normal="F";   first=None;      initial=None     }
        "Х", { normal="H";   first=Some "Kh"; initial=None     }
        "Ц", { normal="Ts";  first=None;      initial=Some "C" }
        "Ч", { normal="Ch";  first=None;      initial=Some "C" }
        "Ш", { normal="Sh";  first=None;      initial=Some "S" }
        "Щ", { normal="Sch"; first=None;      initial=Some "S" }
        "Ъ", { normal="";    first=None;      initial=None     }
        "Ы", { normal="Y";   first=None;      initial=None     }
        "Ь", { normal="";    first=None;      initial=None     }
        "Э", { normal="E";   first=None;      initial=None     }
        "Ю", { normal="Yu";  first=None;      initial=Some "Y" }
        "Я", { normal="Ya";  first=None;      initial=Some "Y" }
      |]

    let toLatin i letter = 
      match Map.tryFind letter dic, i with
      | Some x, 0 -> 
        match x.first with 
        | Some y -> y 
        | None -> x.normal
      | Some x, _ -> x.normal
      | None,   _ -> letter

    let toLatinInitial _ letter = 
      match Map.tryFind letter dic with 
      | Some x -> 
        match x.initial with
        | Some y -> y
        | None   -> x.normal
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
