namespace CompanyDueDiligence

open System
open System.Globalization
open System.Text
open System.Text.RegularExpressions

/// Names in a comparable form, and the fuzzy comparison. No dependency beyond the base library.
module internal NameMatching =

    let private transliterated =
        [
            "ß", "SS"
            "Æ", "AE"
            "Œ", "OE"
            "Ø", "O"
            "Ł", "L"
            "Đ", "D"
            "Þ", "TH"
            "Ð", "D"
            "İ", "I"
            "ı", "I"
        ]

    /// normalise for ASCII text, in one pass with no Unicode work, giving the same answer. A loop, as a bulk search normalises every name in a file.
    let private normaliseAscii (name: string) =
        let built = StringBuilder(name.Length)
        let mutable gap = false

        for c in name do
            if Char.IsAsciiLetterOrDigit c then
                if gap && built.Length > 0 then
                    built.Append ' ' |> ignore

                gap <- false
                built.Append(Char.ToUpperInvariant c) |> ignore
            elif c <> '.' && c <> '\'' then
                gap <- true

        built.ToString()

    /// For ASCII text: whether its normalised form is normal (whole) or starts with it, compared as it is read, without building it. normal is a
    /// normalised name.
    let internal asciiNormalMatches (whole: bool) (normal: string) (text: string) =
        // i is the place in text and j in normal; gap is a run of separators after a letter or digit, read as one space before the next.
        let rec go i j gap =
            if i = text.Length then
                j = normal.Length
            else
                let c = text[i]

                if Char.IsAsciiLetterOrDigit c then
                    let upper = Char.ToUpperInvariant c

                    if gap then
                        if j = normal.Length then
                            not whole
                        elif normal[j] <> ' ' || j + 1 = normal.Length || normal[j + 1] <> upper then
                            false
                        else
                            go (i + 1) (j + 2) false
                    elif j = normal.Length then
                        not whole
                    elif normal[j] <> upper then
                        false
                    else
                        go (i + 1) (j + 1) false
                elif c = '.' || c = '\'' then
                    go (i + 1) j gap
                else
                    go (i + 1) j (j > 0)

        go 0 0 false

    /// Upper-case Cyrillic letters as Latin, by BGN/PCGN for Russian without its rules of position: \u0415 is E everywhere, \u0401 is E, and the hard and
    /// soft signs are dropped. The letters only Ukrainian has follow BGN/PCGN for Ukrainian. A letter the languages read differently is read as
    /// Russian: \u0413 is G and \u0418 is I, where Ukrainian has H and Y; \u0429 is SHCH and \u042a is dropped, where Bulgarian has SHT and A.
    let private cyrillic =
        dict
            [
                '\u0410', "A"
                '\u0411', "B"
                '\u0412', "V"
                '\u0413', "G"
                '\u0490', "G"
                '\u0414', "D"
                '\u0415', "E"
                '\u0401', "E"
                '\u0404', "YE"
                '\u0416', "ZH"
                '\u0417', "Z"
                '\u0418', "I"
                '\u0406', "I"
                '\u0407', "YI"
                '\u0419', "Y"
                '\u041a', "K"
                '\u041b', "L"
                '\u041c', "M"
                '\u041d', "N"
                '\u041e', "O"
                '\u041f', "P"
                '\u0420', "R"
                '\u0421', "S"
                '\u0422', "T"
                '\u0423', "U"
                '\u0424', "F"
                '\u0425', "KH"
                '\u0426', "TS"
                '\u0427', "CH"
                '\u0428', "SH"
                '\u0429', "SHCH"
                '\u042a', ""
                '\u042b', "Y"
                '\u042c', ""
                '\u042d', "E"
                '\u042e', "YU"
                '\u042f', "YA"
            ]

    /// Upper-case Greek letters as Latin, by ELOT 743 (ISO 843's transcription), read after the accents are off.
    let private greek =
        dict
            [
                '\u0391', "A"
                '\u0392', "V"
                '\u0393', "G"
                '\u0394', "D"
                '\u0395', "E"
                '\u0396', "Z"
                '\u0397', "I"
                '\u0398', "TH"
                '\u0399', "I"
                '\u039a', "K"
                '\u039b', "L"
                '\u039c', "M"
                '\u039d', "N"
                '\u039e', "X"
                '\u039f', "O"
                '\u03a0', "P"
                '\u03a1', "R"
                '\u03a3', "S"
                '\u03a4', "T"
                '\u03a5', "Y"
                '\u03a6', "F"
                '\u03a7', "CH"
                '\u03a8', "PS"
                '\u03a9', "O"
            ]

    /// ELOT 743's pairs: \u0391\u03a5, \u0395\u03a5 and \u0397\u03a5 end in F before a voiceless consonant or at a word's end and in V elsewhere, \u039f\u03a5 is OU, and \u0393 before \u0393, \u039e
    /// or \u03a7 is N. With the accents off, a \u03a5 that carried a diaeresis reads as part of a pair.
    let private greekPairs =
        [
            Regex(
                @"([\u0391\u0395\u0397])\u03a5(?=[\u0398\u039a\u039e\u03a0\u03a3\u03a4\u03a6\u03a7\u03a8]|[^\u0391-\u03a9]|$)",
                RegexOptions.Compiled
            ),
            "$1F"
            Regex(@"([\u0391\u0395\u0397])\u03a5", RegexOptions.Compiled), "$1V"
            Regex(@"\u039f\u03a5", RegexOptions.Compiled), "OU"
            Regex(@"\u0393(?=[\u0393\u039e\u03a7])", RegexOptions.Compiled), "N"
        ]

    /// The text with each letter the table holds spelled as the table gives it.
    let private spelled (table: Collections.Generic.IDictionary<char, string>) (text: string) =
        if text |> Seq.exists table.ContainsKey then
            (StringBuilder(text.Length * 2), text)
            ||> Seq.fold (fun built c ->
                match table.TryGetValue c with
                | true, latin -> built.Append latin
                | _ -> built.Append c)
            |> string
        else
            text

    let private greekAsLatin (text: string) =
        if text |> Seq.exists greek.ContainsKey then
            greekPairs
            |> List.fold (fun (text: string) (pair, latin) -> pair.Replace(text, latin)) text
            |> spelled greek
        else
            text

    /// Upper case, accents off, Cyrillic and Greek letters spelled in Latin, full stops and apostrophes gone so that "P.L.C." reads as PLC, and
    /// anything else but letters and digits a space. Letters of any other script stay as they are.
    let normalise (name: string) =
        if String.IsNullOrWhiteSpace name then
            ""
        elif Ascii.IsValid name then
            normaliseAscii name
        else
            // Composed first, so that \u0419 and \u0407 are met as one letter each, before the accents come off.
            let upper =
                transliterated
                |> List.fold
                    (fun (text: string) (from, into) -> text.Replace(from, into))
                    (name.ToUpperInvariant().Normalize NormalizationForm.FormC)
                |> spelled cyrillic

            let bare =
                upper.Normalize NormalizationForm.FormD
                |> Seq.filter (fun c ->
                    CharUnicodeInfo.GetUnicodeCategory c <> UnicodeCategory.NonSpacingMark
                    && c <> '.'
                    && c <> '\''
                    && c <> '\u2019')
                |> Seq.toArray
                |> String

            let plain =
                greekAsLatin bare
                |> String.map (fun c -> if Char.IsLetterOrDigit c then c else ' ')

            String.Join(' ', plain.Split(' ', StringSplitOptions.RemoveEmptyEntries))

    /// Latin letters, with their extended blocks.
    let private isLatin (c: char) =
        c <= '\u024f' || (c >= '\u1e00' && c <= '\u1eff')

    /// True when the name, after normalise, still holds a letter of a script normalise doesn't spell in Latin, such as Hebrew, Arabic or
    /// Chinese. Such a name can't be compared with a list's Latin names.
    let holdsOtherScript (name: string) =
        normalise name |> Seq.exists (fun c -> Char.IsLetter c && not (isLatin c))

    /// Legal forms, whose presence says nothing about whether two names are one party.
    let private legalForms =
        set
            [
                "LTD"
                "LIMITED"
                "LLC"
                "LLP"
                "LP"
                "INC"
                "INCORPORATED"
                "CORP"
                "CORPORATION"
                "PLC"
                "CO"
                "GMBH"
                "AG"
                "KG"
                "SA"
                "SAS"
                "SARL"
                "SL"
                "SRL"
                "SPA"
                "BV"
                "NV"
                "AB"
                "AS"
                "OY"
                "OYJ"
                "OOO"
                "ZAO"
                "OAO"
                "PAO"
                "AO"
                "NAO"
                "JSC"
                "PJSC"
                "OJSC"
                "CJSC"
                "FZE"
                "FZCO"
                "FZC"
                "DMCC"
                "SE"
                "DAC"
                "PTE"
                "PTY"
            ]

    /// Words that don't tell one company from another.
    let private genericWords =
        set
            [
                "COMPANY"
                "TRADING"
                "HOLDING"
                "HOLDINGS"
                "GROUP"
                "INTERNATIONAL"
                "BANK"
                "THE"
                "AND"
                "OF"
                "SERVICES"
                "ENTERPRISES"
                "INVESTMENT"
                "INVESTMENTS"
                "GLOBAL"
                "GENERAL"
                "NATIONAL"
                "INDUSTRIES"
            ]

    /// Courtesy titles, which say nothing about who a name is.
    let private titles =
        set
            [
                "MR"
                "MRS"
                "MS"
                "MISS"
                "MX"
                "DR"
                "PROF"
                "PROFESSOR"
                "SIR"
                "LORD"
                "LADY"
                "DAME"
                "REV"
                "REVD"
                "HON"
                "HERR"
                "FRAU"
                "MME"
                "MLLE"
            ]

    /// Titles opening or closing a name go, and never all of its words.
    let private withoutTitles (words: string list) =
        let trimmed =
            words
            |> List.skipWhile titles.Contains
            |> List.rev
            |> List.skipWhile titles.Contains
            |> List.rev

        if trimmed.IsEmpty then words else trimmed

    let tokens (name: string) =
        (normalise name).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.filter (fun token -> not (legalForms.Contains token))
        |> List.ofArray
        |> withoutTitles

    /// Jaro similarity. Written with loops because a screening runs it millions of times.
    let private jaro (first: string) (second: string) =
        if first = second then
            1.
        elif first.Length = 0 || second.Length = 0 then
            0.
        else
            let window = max 0 ((max first.Length second.Length) / 2 - 1)
            let firstHit = Array.zeroCreate<bool> first.Length
            let secondHit = Array.zeroCreate<bool> second.Length
            let mutable matches = 0

            for i in 0 .. first.Length - 1 do
                let upTo = min (second.Length - 1) (i + window)
                let mutable j = max 0 (i - window)

                while j <= upTo do
                    if not secondHit[j] && first[i] = second[j] then
                        firstHit[i] <- true
                        secondHit[j] <- true
                        matches <- matches + 1
                        j <- upTo

                    j <- j + 1

            if matches = 0 then
                0.
            else
                let mutable swapped = 0
                let mutable k = 0

                for i in 0 .. first.Length - 1 do
                    if firstHit[i] then
                        while not secondHit[k] do
                            k <- k + 1

                        if first[i] <> second[k] then
                            swapped <- swapped + 1

                        k <- k + 1

                let m = float matches

                (m / float first.Length + m / float second.Length + (m - float swapped / 2.) / m)
                / 3.

    /// Jaro-Winkler on two strings.
    let similarity (first: string) (second: string) =
        let score = jaro first second
        let longestPrefix = min 4 (min first.Length second.Length)

        let rec advancePrefix prefix =
            if prefix < longestPrefix && first[prefix] = second[prefix] then
                advancePrefix (prefix + 1)
            else
                prefix

        let prefix = advancePrefix 0

        score + float prefix * 0.1 * (1. - score)

    /// The score under which two spellings aren't raised, on words sorted so that their order doesn't count. A figure tried against
    /// the live lists of 2026-11-06, not measured for recall: see the README.
    [<Literal>]
    let SpellingThreshold = 0.92

    /// The stricter score for one distinctive word on each side, where one letter weighs more. Tried the same way.
    [<Literal>]
    let OneWordThreshold = 0.96

    /// Plain business words, as frequent in the lists as a sanctioned group's own name can be: raised alone, each would flag dozens of unrelated entries.
    let private plainBusinessWords =
        set
            [
                "AVIATION"
                "CAPITAL"
                "CONSULTING"
                "DEVELOPMENT"
                "ENERGY"
                "ENGINEERING"
                "FINANCE"
                "GAS"
                "INVEST"
                "LOGISTICS"
                "MANAGEMENT"
                "MARINE"
                "MEDIA"
                "OIL"
                "SHIPPING"
                "SOLUTIONS"
                "TECHNOLOGIES"
                "TECHNOLOGY"
            ]

    /// A word in more listed entries than this is too common to raise every entry it is in. 168 of the 36,790 words in the lists of 2026-11-06 are: ENERGY in
    /// 167 and CAPITAL in 84 are, GAZPROM in 65 and SBERBANK in 33 aren't.
    [<Literal>]
    let CommonWordEntries = 75

    /// The shortest word that can be raised alone.
    [<Literal>]
    let private ShortestLoneWord = 3

    /// A name ready to compare: normalising a whole list once, not per comparison.
    type internal Prepared =
        {
            Words: string list
            WordSet: Set<string>
            Sorted: string
            /// The words that tell one party from another, sorted: the spelling comparison reads these, so that "Trading" can't carry a match.
            Distinctive: string list
        }

    let internal prepare (name: string) =
        let words = tokens name

        {
            Words = words
            WordSet = Set.ofList words
            Sorted = words |> List.sort |> String.concat " "
            Distinctive = words |> List.filter (fun word -> not (genericWords.Contains word)) |> List.sort
        }

    let private hasDistinctiveWord (words: string list) =
        words |> List.exists (fun word -> not (genericWords.Contains word))

    /// A word that can't be raised alone, whatever the lists at hand hold.
    let internal saysLittleAlone (word: string) =
        genericWords.Contains word || plainBusinessWords.Contains word

    /// Whether one word can carry a match by being in both names: long enough, not a plain business word, and not common in the lists at hand.
    let private raisedAlone (isCommon: string -> bool) (word: string) =
        word.Length >= ShortestLoneWord
        && not (saysLittleAlone word)
        && not (isCommon word)

    /// Spellings whose lengths differ by more than this share can't reach the threshold, so aren't scored.
    let private tooDifferentInLength (first: string) (second: string) =
        float (abs (first.Length - second.Length)) > 0.35 * float (max first.Length second.Length)

    /// isCommon says which words are too common in the lists at hand to raise alone.
    let internal comparePrepared (isCommon: string -> bool) (ours: Prepared) (theirs: Prepared) =
        if ours.Words.IsEmpty || theirs.Words.IsEmpty then
            ValueNone
        elif ours.Sorted = theirs.Sorted then
            ValueSome(1., "The same words after normalising case, accents, punctuation and legal form")
        else
            let shorter, longer =
                if ours.Words.Length <= theirs.Words.Length then
                    ours, theirs
                else
                    theirs, ours

            if
                shorter.Words.Length >= 2
                && hasDistinctiveWord shorter.Words
                && shorter.WordSet.IsSubsetOf longer.WordSet
            then
                ValueSome(0.9, "Every word of the shorter name is in the longer one")
            else
                match ours.Distinctive, theirs.Distinctive with
                | [], _
                | _, [] -> ValueNone
                | [ single ], [ other ] ->
                    // Words of six letters or more are scored for spelling; a shorter word counts only when it is the same word.
                    let similar =
                        if single.Length < 6 || tooDifferentInLength single other then
                            ValueNone
                        else
                            let score = similarity single other

                            if score >= OneWordThreshold then
                                ValueSome(
                                    Math.Round(score, 3),
                                    $"Similar spelling of one distinctive word: Jaro-Winkler {score:F3}"
                                )
                            else
                                ValueNone

                    match similar with
                    | ValueNone when single = other && raisedAlone isCommon single ->
                        ValueSome(0.85, "The one distinctive word of each name is the same")
                    | found -> found
                | [ single ], _ when
                    ours.Words = [ single ]
                    && theirs.WordSet.Contains single
                    && raisedAlone isCommon single
                    ->
                    ValueSome(0.85, "The one word of the screened name is in the listed name")
                | _, [ single ] when
                    theirs.Words = [ single ]
                    && ours.WordSet.Contains single
                    && raisedAlone isCommon single
                    ->
                    ValueSome(0.85, "The one word of the listed name is in the screened name")
                | first, second ->
                    let oursText, theirsText = String.concat " " first, String.concat " " second

                    if tooDifferentInLength oursText theirsText then
                        ValueNone
                    else
                        let score = similarity oursText theirsText

                        if score >= SpellingThreshold then
                            ValueSome(
                                Math.Round(score, 3),
                                $"Similar spelling: Jaro-Winkler {score:F3} on sorted words"
                            )
                        else
                            ValueNone

    /// Whether two names resemble each other, with the score and the reason. A name of one word, screened or listed, counts when the other name
    /// holds that word, and so do two names whose one distinctive word is the same or nearly the same. A screening also holds back a word common
    /// in its lists: see CommonWordEntries.
    let compare (screened: string) (listed: string) =
        comparePrepared (fun _ -> false) (prepare screened) (prepare listed)
