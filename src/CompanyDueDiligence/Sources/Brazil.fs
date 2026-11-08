namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading

/// Receita Federal's CNPJ open data: monthly Empresas, Estabelecimentos and Socios files, each split in ten zips, semicolon-delimited and
/// quoted, in Latin-1, with no header. Read from the first rows of Empresas9, Estabelecimentos9 and Socios9 of 2026-10 and its code tables
/// (Naturezas, Qualificacoes, Motivos), which are embedded here; column order follows Receita's published layout.
module internal Brazil =

    let coverage: RegistryCoverage =
        {
            Registry = "Receita Federal CNPJ open data (Brazil)"
            Jurisdictions = [ Jurisdiction.ofCode "BR" ]
            Standing =
                Support.Partial
                    "The establishment's cadastral situation at Receita Federal, with its special situation, such as bankruptcy"
            Concerns =
                Support.Partial
                    "Insolvency from the special situation, overdue declarations and an unknown address from the reason for the situation; nothing else"
            Addresses = Support.Partial "The establishment's address, with the municipality as Receita's code"
            Search = Support.Partial "50 names containing the term, from the files passed"
            Officers = Support.Partial "Administrators in the partners' table (QSA), current ones only"
            Owners = Support.Partial "Partners in the QSA, current ones only, with no shares"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "Unverified: no licence found on the file share"
        }

    let private naturezas =
        dict
            [
                "0000", "Natureza Jurídica não informada"
                "3271", "Órgão de Direção Local de Partido Político"
                "3280", "Comitê Financeiro de Partido Político"
                "3298", "Frente Plebiscitária ou Referendária"
                "3301", "Organização Social (OS)"
                "3999", "Associação Privada"
                "4014", "Empresa Individual Imobiliária"
                "4090", "Candidato a Cargo Político Eletivo"
                "4120", "Produtor Rural (Pessoa Física)"
                "5010", "Organização Internacional"
                "5029", "Representação Diplomática Estrangeira"
                "1015", "Órgão Público do Poder Executivo Federal"
                "1023", "Órgão Público do Poder Executivo Estadual ou do Distrito Federal"
                "1031", "Órgão Público do Poder Executivo Municipal"
                "1040", "Órgão Público do Poder Legislativo Federal"
                "1058", "Órgão Público do Poder Legislativo Estadual ou do Distrito Federal"
                "1066", "Órgão Público do Poder Legislativo Municipal"
                "1074", "Órgão Público do Poder Judiciário Federal"
                "1082", "Órgão Público do Poder Judiciário Estadual"
                "1104", "Autarquia Federal"
                "1112", "Autarquia Estadual ou do Distrito Federal"
                "1120", "Autarquia Municipal"
                "1139", "Fundação Pública de Direito Público Federal"
                "1147", "Fundação Pública de Direito Público Estadual ou do Distrito Federal"
                "1155", "Fundação Pública de Direito Público Municipal"
                "1163", "Órgão Público Autônomo Federal"
                "1171", "Órgão Público Autônomo Estadual ou do Distrito Federal"
                "1180", "Órgão Público Autônomo Municipal"
                "1198", "Comissão Polinacional"
                "1210", "Consórcio Público de Direito Público (Associação Pública)"
                "1228", "Consórcio Público de Direito Privado"
                "1236", "Estado ou Distrito Federal"
                "1244", "Município"
                "1252", "Fundação Pública de Direito Privado Federal"
                "1260", "Fundação Pública de Direito Privado Estadual ou do Distrito Federal"
                "1279", "Fundação Pública de Direito Privado Municipal"
                "1287", "Fundo Público da Administração Indireta Federal"
                "1295", "Fundo Público da Administração Indireta Estadual ou do Distrito Federal"
                "1309", "Fundo Público da Administração Indireta Municipal"
                "1317", "Fundo Público da Administração Direta Federal"
                "1325", "Fundo Público da Administração Direta Estadual ou do Distrito Federal"
                "1333", "Fundo Público da Administração Direta Municipal"
                "1341", "União"
                "2011", "Empresa Pública"
                "2038", "Sociedade de Economia Mista"
                "2046", "Sociedade Anônima Aberta"
                "2054", "Sociedade Anônima Fechada"
                "2062", "Sociedade Empresária Limitada"
                "2070", "Sociedade Empresária em Nome Coletivo"
                "2089", "Sociedade Empresária em Comandita Simples"
                "2097", "Sociedade Empresária em Comandita por Ações"
                "2100", "Sociedade Mercantil de Capital e Indústria"
                "2127", "Sociedade em Conta de Participação"
                "2135", "Empresário (Individual)"
                "2143", "Cooperativa"
                "2151", "Consórcio de Sociedades"
                "2160", "Grupo de Sociedades"
                "2178", "Estabelecimento, no Brasil, de Sociedade Estrangeira"
                "2194", "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira"
                "2216", "Empresa Domiciliada no Exterior"
                "2224", "Clube/Fundo de Investimento"
                "2232", "Sociedade Simples Pura"
                "2240", "Sociedade Simples Limitada"
                "2259", "Sociedade Simples em Nome Coletivo"
                "2267", "Sociedade Simples em Comandita Simples"
                "2275", "Empresa Binacional"
                "2283", "Consórcio de Empregadores"
                "2291", "Consórcio Simples"
                "2305", "Empresa Individual de Responsabilidade Limitada (de Natureza Empresária)"
                "2313", "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)"
                "2321", "Sociedade Unipessoal de Advocacia"
                "2330", "Cooperativas de Consumo"
                "2348", "Empresa Simples de Inovação"
                "3034", "Serviço Notarial e Registral (Cartório)"
                "3069", "Fundação Privada"
                "3077", "Serviço Social Autônomo"
                "3085", "Condomínio Edilício"
                "3107", "Comissão de Conciliação Prévia"
                "3115", "Entidade de Mediação e Arbitragem"
                "3131", "Entidade Sindical"
                "3204", "Estabelecimento, no Brasil, de Fundação ou Associação Estrangeiras"
                "3212", "Fundação ou Associação Domiciliada no Exterior"
                "3220", "Organização Religiosa"
                "3239", "Comunidade Indígena"
                "3247", "Fundo Privado"
                "3255", "Órgão de Direção Nacional de Partido Político"
                "3263", "Órgão de Direção Regional de Partido Político"
                "5037", "Outras Instituições Extraterritoriais"
                "3328", "Plano de Benefícios de Previdência Complementar Fechada"
                "1350", "ENTIDADE PÚBLICA SOB REGIME ESPECIAL"
                "3310", "DEMAIS CONDOMÍNIOS"
                "8885", "Natureza Jurídica não informada"
            ]

    let private qualificacoes =
        dict
            [
                "00", "Não informada"
                "05", "Administrador"
                "08", "Conselheiro de Administração"
                "09", "Curador"
                "10", "Diretor"
                "11", "Interventor"
                "12", "Inventariante"
                "13", "Liquidante"
                "14", "Mãe"
                "15", "Pai"
                "16", "Presidente"
                "17", "Procurador"
                "18", "Secretário"
                "19", "Síndico (Condomínio)"
                "20", "Sociedade Consorciada"
                "21", "Sociedade Filiada"
                "22", "Sócio"
                "23", "Sócio Capitalista"
                "24", "Sócio Comanditado"
                "25", "Sócio Comanditário"
                "26", "Sócio de Indústria"
                "28", "Sócio-Gerente"
                "29", "Sócio Incapaz ou Relat.Incapaz (exceto menor)"
                "30", "Sócio Menor (Assistido/Representado)"
                "31", "Sócio Ostensivo"
                "32", "Tabelião"
                "33", "Tesoureiro"
                "34", "Titular de Empresa Individual Imobiliária"
                "35", "Tutor"
                "37", "Sócio Pessoa Jurídica Domiciliado no Exterior"
                "38", "Sócio Pessoa Física Residente no Exterior"
                "39", "Diplomata"
                "40", "Cônsul"
                "41", "Representante de Organização Internacional"
                "42", "Oficial de Registro"
                "43", "Responsável"
                "46", "Ministro de Estado das Relações Exteriores"
                "47", "Sócio Pessoa Física Residente no Brasil"
                "48", "Sócio Pessoa Jurídica Domiciliado no Brasil"
                "49", "Sócio-Administrador"
                "50", "Empresário"
                "51", "Candidato a cargo Político Eletivo"
                "52", "Sócio com Capital"
                "53", "Sócio sem Capital"
                "54", "Fundador"
                "55", "Sócio Comanditado Residente no Exterior"
                "56", "Sócio Comanditário Pessoa Física Residente no Exterior"
                "57", "Sócio Comanditário Pessoa Jurídica Domiciliado no Exterior"
                "58", "Sócio Comanditário Incapaz"
                "59", "Produtor Rural"
                "60", "Cônsul Honorário"
                "61", "Responsável indígena"
                "62", "Representante da Instituição Extraterritorial"
                "63", "Cotas em Tesouraria"
                "64", "Administrador Judicial"
                "65", "Titular Pessoa Física Residente ou Domiciliado no Brasil"
                "66", "Titular Pessoa Física Residente ou Domiciliado no Exterior"
                "67", "Titular Pessoa Física Incapaz ou Relativamente Incapaz (exceto menor)"
                "68", "Titular Pessoa Física Menor (Assistido/Representado)"
                "69", "Beneficiário Final"
                "70", "Administrador Residente ou Domiciliado no Exterior"
                "71", "Conselheiro de Administração Residente ou Domiciliado no Exterior"
                "72", "Diretor Residente ou Domiciliado no Exterior"
                "73", "Presidente Residente ou Domiciliado no Exterior"
                "74", "Sócio-Administrador Residente ou Domiciliado no Exterior"
                "75", "Fundador Residente ou Domiciliado no Exterior"
                "78", "Titular Pessoa Jurídica Domiciliada no Brasil"
                "79", "Titular Pessoa Jurídica Domiciliada no Exterior"
            ]

    let private motivos =
        dict
            [
                "00", "SEM MOTIVO"
                "01", "EXTINCAO POR ENCERRAMENTO LIQUIDACAO VOLUNTARIA"
                "02", "INCORPORACAO"
                "03", "FUSAO"
                "04", "CISAO TOTAL"
                "05", "ENCERRAMENTO DA FALENCIA"
                "06", "ENCERRAMENTO DA LIQUIDACAO"
                "07", "ELEVACAO A MATRIZ"
                "08", "TRANSPASSE"
                "09", "NAO INICIO DE ATIVIDADE"
                "10", "EXTINCAO PELO ENCERRAMENTO DA LIQUIDACAO JUDICIAL"
                "11", "ANULACAO POR MULTICIPLIDADE"
                "12", "ANULACAO ONLINE DE OFICIO"
                "13", "OMISSA CONTUMAZ"
                "14", "OMISSA NAO LOCALIZADA"
                "15", "INEXISTENCIA DE FATO"
                "16", "ANULACAO POR VICIOS"
                "17", "BAIXA INICIADA EM ANALISE"
                "18", "INTERRUPCAO TEMPORARIA DAS ATIVIDADES"
                "21", "PEDIDO DE BAIXA INDEFERIDA"
                "24", "POR EMISSAO CERTIDAO NEGATIVA"
                "28", "TRANSFERENCIA FILIAL CONDICAO MATRIZ"
                "31", "EXTINCAO-UNIFICACAO DA FILIAL"
                "33", "TRANSFERENCIA DO ORGAO LOCAL A CONDICAO DE FILIAL DO ORGAO REGIONAL"
                "34", "ANULACAO DE INSCRICAO INDEVIDA"
                "35", "EMPRESA ESTRANGEIRA AGUARDANDO DOCUMENTACAO"
                "36", "PRATICA IRREGULAR DE OPERACAO DE COMERCIO EXTERIOR"
                "37", "BAIXA DE PRODUTOR RURAL"
                "38", "BAIXA DEFERIDA PELA RFB AGUARDANDO ANALISE DO CONVENENTE"
                "39", "BAIXA DEFERIDA PELA RFB E INDEFERIDA PELO CONVENENTE"
                "40", "BAIXA INDEFERIDA PELA RFB E AGUARDANDO ANALISE DO CONVENENTE"
                "41", "BAIXA INDEFERIDA PELA RFB E DEFERIDA PELO CONVENENTE"
                "42", "BAIXA DEFERIDA PELA RFB E SEFIN, AGUARDANDO ANALISE SEFAZ"
                "43", "BAIXA DEFERIDA PELA RFB, AGUARDANDO ANALISE DA SEFAZ E INDEFERIDA PELA SEFIN"
                "44", "BAIXA DEFERIDA PELA RFB E SEFAZ, AGUARDANDO ANALISE SEFIN"
                "45", "BAIXA DEFERIDA PELA RFB, AGUARDANDO ANALISE DA SEFIN E INDEFERIDA PELA SEFAZ"
                "46", "BAIXA DEFERIDA PELA RFB E SEFAZ E INDEFERIDA PELA SEFIN"
                "47", "BAIXA DEFERIDA PELA RFB E SEFIN E INDEFERIDA PELA SEFAZ"
                "48", "BAIXA INDEFERIDA PELA RFB, AGARDANDO ANALISE SEFAZ E DEFERIDA PELA SEFIN"
                "49", "BAIXA INDEFERIDA PELA RFB, AGUARDANDO ANALISE DA SEFAZ E INDEFERIDA PELA SEFIN"
                "50", "BAIXA INDEFERIDA PELA RFB, DEFERIDA PELA SEFAZ E AGUARDANDO ANALISE DA SEFIN"
                "51", "BAIXA INDEFERIDA PELA RFB E SEFAZ, AGUARDANDO ANALISE DA SEFIN"
                "52", "BAIXA INDEFERIDA PELA RFB, DEFERIDA PELA SEFAZ E INDEFERIDA PELA SEFIN"
                "53", "BAIXA INDEFERIDA PELA RFB E SEFAZ E DEFERIDA PELA SEFIN"
                "54", "EXTINCAO - TRATAMENTO DIFERENCIADO DADO AS ME E EPP (LEI COMPLEMENTAR NUMERO 123/2006)"
                "55", "DEFERIDO PELO CONVENENTE, AGUARDANDO ANALISE DA RFB"
                "60", "ARTIGO 30, VI, DA IN 748/2007"
                "61", "INDICIO INTERPOS. FRAUDULENTA"
                "62", "FALTA DE PLURALIDADE DE SOCIOS"
                "63", "OMISSAO DE DECLARACOES"
                "64", "LOCALIZACAO DESCONHECIDA"
                "66", "INAPTIDAO"
                "67", "REGISTRO CANCELADO"
                "70", "ANULACAO POR NAO CONFIRMADO ATO DE REGISTRO DO MEI NA JUNTA COMERCIAL"
                "71", "INAPTIDAO (LEI 11.941/2009 ART.54)"
                "72", "DETERMINACAO JUDICIAL"
                "73", "COOMISSAO CONTUMAZ"
                "74", "INCONSISTENCIA CADASTRAL"
                "75", "OBITO DO MEI - TITULAR FALECIDO"
                "80", "BAIXA REGISTRADA NA JUNTA, INDEFERIDA NA RFB"
                "81", "SOLICITACAO DA ADMINISTRACAO TRIBUTARIA ESTADUAL/MUNICIPAL"
                "82", "SUSPENSO PERANTE A COMISSAO DE VALORES MOBILIARIOS - CVM"
                "93", "CNPJ - TITULAR BAIXADO"
            ]

    let private word (table: IDictionary<string, string>) (code: string) =
        match table.TryGetValue code with
        | true, word -> ValueSome word
        | _ -> ValueNone

    /// A CNPJ character's value for its check digits: its character code less 48, as for the alphanumeric CNPJ, which keeps digits as they are.
    let private value (c: char) = int c - 48

    let private digitFrom (weights: int list) (characters: char list) =
        let remainder =
            List.map2 (fun c weight -> value c * weight) characters weights
            |> List.sum
            |> fun total -> total % 11

        if remainder < 2 then 0 else 11 - remainder

    /// The two check digits of the first twelve characters.
    let checkDigits (first12: string) =
        let characters = List.ofSeq first12
        let first = digitFrom [ 5; 4; 3; 2; 9; 8; 7; 6; 5; 4; 3; 2 ] characters

        let second =
            digitFrom [ 6; 5; 4; 3; 2; 9; 8; 7; 6; 5; 4; 3; 2 ] (characters @ [ char (first + 48) ])

        $"{first}{second}"

    let private fullRegex = Regex @"\A[0-9A-Z]{12}[0-9]{2}\z"
    let private baseRegex = Regex @"\A[0-9A-Z]{8}\z"

    /// A CNPJ of fourteen characters whose check digits hold, or a CNPJ base of eight for the head office; dots, slash and hyphen are dropped.
    let normalCnpj (number: string) =
        let text =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace(".", "").Replace("/", "").Replace("-", "")

        if
            fullRegex.IsMatch text
            && checkDigits (text.Substring(0, 12)) = text.Substring 12
        then
            ValueSome text
        elif baseRegex.IsMatch text then
            ValueSome text
        else
            ValueNone

    [<Literal>]
    let invalidNumber =
        "A CNPJ has fourteen characters ending in two check digits, or eight for the company's base"

    let private situationWord (code: string) =
        match code with
        | "01" -> ValueSome "NULA"
        | "02" -> ValueSome "ATIVA"
        | "03" -> ValueSome "SUSPENSA"
        | "04" -> ValueSome "INAPTA"
        | "08" -> ValueSome "BAIXADA"
        | _ -> ValueNone

    /// The cadastral situation, with an active establishment's special situation, such as bankruptcy, deciding before it.
    let standing (situation: string) (special: string) =
        let special = special.Trim().ToUpperInvariant()

        match situation.Trim().PadLeft(2, '0') with
        | "02" when special = "" -> CompanyStanding.Active
        | "02" when special.Contains "FALI" -> CompanyStanding.InInsolvency special
        | "02" when special.Contains "RECUPERACAO" -> CompanyStanding.InInsolvency special
        | "02" when special.Contains "LIQUIDACAO" && special.Contains "JUDICIAL" -> CompanyStanding.InInsolvency special
        | "02" when special.Contains "LIQUIDACAO" -> CompanyStanding.WindingUp special
        | "02" -> CompanyStanding.ActiveWithConcern special
        | "01" -> CompanyStanding.Closed "Nula"
        | "03" -> CompanyStanding.Registered(ValueSome "Suspensa")
        | "04" -> CompanyStanding.Registered(ValueSome "Inapta")
        | "08" -> CompanyStanding.Closed "Baixada"
        | "00" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    /// Reasons that state an overdue declaration, and an address Receita couldn't find.
    let private overdueReasons = set [ "13"; "63"; "73" ]
    let private unknownAddressReasons = set [ "14"; "64" ]
    let private insolvencyReasons = set [ "05"; "10" ]

    let concerns (situation: string) (reason: string) (special: string) =
        let special = special.ToUpperInvariant()

        let stated =
            [
                if
                    insolvencyReasons.Contains reason
                    || special.Contains "FALI"
                    || special.Contains "RECUPERACAO"
                    || special.Contains "LIQUIDACAO"
                then
                    yield CompanyConcern.InsolvencyHistory
                if situation <> "02" && overdueReasons.Contains reason then
                    yield CompanyConcern.AnnualFilingOverdue
                if situation <> "02" && unknownAddressReasons.Contains reason then
                    yield CompanyConcern.RegisteredOfficeUndeliverable
            ]

        {
            Stated = stated
            Unconfirmed = CompanyConcern.all |> List.except stated
        }

    /// Natures of a foreign company's establishment in Brazil, or of a company domiciled abroad.
    let private foreignNatures = set [ "2178"; "2194"; "2216" ]

    /// Qualifications of a partner, owner or holder; the rest are administrators or others the QSA lists. Partner-administrators are both.
    let private ownerQualifications =
        set
            [
                "20"
                "21"
                "22"
                "23"
                "24"
                "25"
                "26"
                "28"
                "29"
                "30"
                "31"
                "34"
                "37"
                "38"
                "47"
                "48"
                "49"
                "50"
                "52"
                "53"
                "54"
                "55"
                "56"
                "57"
                "58"
                "63"
                "65"
                "66"
                "67"
                "68"
                "69"
                "74"
                "75"
                "78"
                "79"
            ]

    let private alsoOfficers = set [ "28"; "49"; "74" ]
    let private foreignEntities = set [ "37"; "57"; "79" ]

    let private foreignPeople =
        set [ "38"; "55"; "56"; "66"; "70"; "71"; "72"; "73"; "74"; "75" ]

    /// A partner's kind: 1 a legal entity, 2 a person, and 3, foreign, by the qualification where it says which.
    let partnerKind (identifier: string) (qualification: string) =
        match identifier with
        | "1" -> PartyKind.LegalEntity
        | "2" -> PartyKind.Individual
        | "3" when foreignEntities.Contains qualification -> PartyKind.LegalEntity
        | "3" when foreignPeople.Contains qualification -> PartyKind.Individual
        | "3" -> PartyKind.Unknown(ValueSome "estrangeiro")
        | "" -> PartyKind.Unknown ValueNone
        | other -> PartyKind.Unknown(ValueSome other)

    let private qualificationWord (code: string) =
        word qualificacoes code |> ValueOption.defaultValue $"qualificação {code}"

    let officerRole (qualification: string) =
        match qualification with
        | "10"
        | "72" -> OfficerRole.Director
        | "18" -> OfficerRole.Secretary
        | other -> OfficerRole.Other(qualificationWord other)

    let private field (fields: string array) (position: int) =
        if position < fields.Length then
            fields[position].Trim()
        else
            ""

    let private date (text: string) = BulkFile.dateFrom1800 "yyyyMMdd" text

    /// A Socios row: base, identifier, name, CNPJ or masked CPF, qualification, date of entry, country, representative's CPF, name and
    /// qualification, age band.
    type internal Partner =
        {
            Identifier: string
            Name: string
            Document: string
            Qualification: string
            Since: DateOnly voption
            Line: string
        }

    let partner (fields: string array) (line: string) =
        {
            Identifier = field fields 1
            Name = field fields 2
            Document = field fields 3
            Qualification = field fields 4
            Since = date (field fields 5)
            Line = line
        }

    let private asOfficer (partner: Partner) : Officer =
        {
            Name = partner.Name
            Kind = partnerKind partner.Identifier partner.Qualification
            Role = officerRole partner.Qualification
            Current = true
            AppointedOn = partner.Since
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    let private asOwner (partner: Partner) : Owner =
        let kind = partnerKind partner.Identifier partner.Qualification

        {
            Name = BulkFile.nonBlank partner.Name
            Kind = kind
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band = ValueNone
                        AsPublished = qualificationWord partner.Qualification
                    }
                ]
            Current = true
            Since = partner.Since
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                match kind, BulkFile.nonBlank partner.Document with
                | PartyKind.LegalEntity, document ->
                    ValueSome
                        {
                            LegalName = BulkFile.nonBlank partner.Name
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country =
                                if partner.Identifier = "1" then
                                    ValueSome "BR"
                                else
                                    ValueNone
                            Register =
                                if partner.Identifier = "1" then
                                    ValueSome "CNPJ"
                                else
                                    ValueNone
                            RegistrationNumber = document
                        }
                | _ -> ValueNone
            IdentityVerification = ValueNone
        }

    /// The partners in the QSA as owners. The QSA gives no shares, so details are never confirmed; a partner whose kind can't be read is a gap,
    /// and with fewer than all ten Socios parts read a partner can be in a part not passed.
    let listing (allSocios: bool) (partners: Partner list) : OwnerListing voption =
        match
            partners
            |> List.filter (fun partner -> ownerQualifications.Contains partner.Qualification)
        with
        | [] -> ValueNone
        | held ->
            let owners = held |> List.map asOwner

            ValueSome
                {
                    Owners = owners
                    Statements = []
                    Gaps =
                        [
                            OwnershipGap.OwnerDetailsNotConfirmed
                            if not allSocios then
                                OwnershipGap.OwnerNotIdentified
                            for owner in owners do
                                match owner.Kind, owner.Entity |> ValueOption.bind _.RegistrationNumber with
                                | PartyKind.LegalEntity, ValueNone -> OwnershipGap.EntityNotIdentified
                                | PartyKind.Unknown _, _ -> OwnershipGap.OwnerUnreadable
                                | _ -> ()
                        ]
                        |> List.distinct
                }

    /// An Empresas row: base, legal name, nature, the responsible person's qualification, capital, size, federal entity.
    type internal Firm =
        {
            Name: string
            Nature: string
            Line: string
        }

    /// The register's record for one establishment, with its company's name and nature, and the QSA of its company. allSocios says whether all
    /// ten Socios parts were read.
    let record
        (allSocios: bool)
        (firm: Firm)
        (partners: Partner list)
        (headOffices: IReadOnlyDictionary<string, string>)
        (fields: string array)
        (line: string)
        =
        let basis = field fields 0
        let number = basis + field fields 1 + field fields 2
        let isHeadOffice = field fields 3 = "1"
        let situation = (field fields 5).PadLeft(2, '0')
        let reason = field fields 7
        let special = field fields 28
        let foreignCity = field fields 8
        let state = field fields 19

        let street =
            [ field fields 13; field fields 14; field fields 15 ]
            |> List.filter ((<>) "")
            |> String.concat " "

        let address =
            Bulk.address
                ([
                    street
                    field fields 16
                    field fields 17
                    (if field fields 20 = "" then
                         ""
                     else
                         $"Município {field fields 20} (código da Receita)")
                 ]
                 |> List.map BulkFile.nonBlank)
                (BulkFile.nonBlank foreignCity)
                (BulkFile.nonBlank state)
                (BulkFile.nonBlank (field fields 18))
                (if state = "EX" then ValueNone else ValueSome "BRASIL")

        let officers =
            partners
            |> List.filter (fun partner ->
                not (ownerQualifications.Contains partner.Qualification)
                || alsoOfficers.Contains partner.Qualification)
            |> List.map asOfficer

        let status =
            [
                yield situationWord situation |> ValueOption.defaultValue situation
                yield!
                    word motivos reason
                    |> ValueOption.filter (fun _ -> reason <> "00")
                    |> ValueOption.toList
                if special <> "" then
                    yield special
            ]
            |> String.concat "; "

        {
            Company =
                {
                    Name = firm.Name
                    OtherNames =
                        BulkFile.nonBlank (field fields 4)
                        |> ValueOption.filter ((<>) firm.Name)
                        |> ValueOption.toList
                    Number = number
                    Jurisdiction = Jurisdiction.ofCode "BR"
                    JurisdictionAsPublished = ValueNone
                    Standing = standing situation special
                    Concerns = concerns situation reason special
                    StatusAsPublished = BulkFile.nonBlank status
                    LegalForm = word naturezas firm.Nature |> ValueOption.orElse (BulkFile.nonBlank firm.Nature)
                    IncorporatedOn = date (field fields 10)
                    CeasedOn =
                        if situation = "08" || situation = "01" then
                            date (field fields 6)
                        else
                            ValueNone
                    Addresses =
                        match address with
                        | ValueSome address ->
                            Available
                                [
                                    {
                                        Kind =
                                            (if isHeadOffice then
                                                 AddressKind.PrincipalOffice
                                             else
                                                 AddressKind.Other "Estabelecimento filial")
                                        Address = address
                                    }
                                ]
                        | ValueNone -> NotAvailable "The establishment's row holds no address"
                    Role =
                        if not isHeadOffice then
                            RecordRole.Establishment
                        elif foreignNatures.Contains firm.Nature then
                            RecordRole.ForeignRegistration
                        else
                            RecordRole.Home
                    HomeRegistration = ValueNone
                    EstablishmentOf =
                        if isHeadOffice then
                            ValueNone
                        else
                            match headOffices.TryGetValue basis with
                            | true, head -> ValueSome head
                            | _ -> ValueNone
                }
            Officers =
                if List.isEmpty officers then
                    ValueNone
                else
                    ValueSome officers
            Owners = listing allSocios partners
            Raw = String.concat "\n" (line :: firm.Line :: (partners |> List.map _.Line))
        }

    /// The rows of each file in the streams, one file at a time: ValueNone as each file opens, then each row's fields and line. A row of
    /// another width than the layout's fails the file.
    let private files (ct: CancellationToken) (streams: Stream list) (width: int) =
        streams
        |> Seq.collect (fun file ->
            BulkFile.each file (fun _ openFile ->
                seq {
                    yield ValueNone

                    yield!
                        BulkFile.lines Encoding.Latin1 (openFile ())
                        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                        |> Seq.map (fun line ->
                            ct.ThrowIfCancellationRequested()
                            let fields = BulkFile.split ';' true line

                            if fields.Length <> width then
                                BulkFile.fail $"A row has {fields.Length} fields where the layout has {width}"

                            ValueSome(struct (fields, line)))
                }))

    /// The rows of every file in the streams, as files reads them.
    let private rows ct streams width =
        files ct streams width |> Seq.choose ValueOption.toOption

    /// How many parts Receita splits each file into.
    [<Literal>]
    let Parts = 10

    /// The register over the files the host picks: any of the ten parts of Empresas, Estabelecimentos and Socios of one month. An establishment
    /// whose company isn't in the Empresas passed is left out, having no name. A CNPJ's base finds its head office. A base on two Empresas rows
    /// answers NotAvailable, with every CNPJ under it. Partners are gathered in a dictionary filled here and then only read, and establishments
    /// are held once, as both passes over them need them: the full files run to tens of millions of rows.
    let build
        (empresas: Stream list)
        (estabelecimentos: Stream list)
        (socios: Stream list)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : ICompanyRegistry =
        let firms, repeatedBases =
            rows ct empresas 7
            |> Seq.map (fun struct (fields, line) ->
                match field fields 0, field fields 1 with
                | "", _
                | _, "" -> BulkFile.fail "A company row has no CNPJ base or name"
                | basis, name ->
                    {
                        Name = name
                        Nature = field fields 2
                        Line = line
                    },
                    basis)
            |> Bulk.index (snd >> ValueSome)

        let partners = Dictionary<string, List<Partner>>()

        let socioFiles =
            files ct socios 11
            |> Seq.fold
                (fun count row ->
                    match row with
                    | ValueNone -> count + 1
                    | ValueSome(struct (fields, line)) ->
                        let basis = field fields 0

                        if basis = "" then
                            BulkFile.fail "A partner row has no CNPJ base"

                        match partners.TryGetValue basis with
                        | true, held -> held.Add(partner fields line)
                        | _ -> partners[basis] <- List [ partner fields line ]

                        count)
                0

        let allSocios = socioFiles >= Parts

        let establishments =
            rows ct estabelecimentos 30
            |> Seq.map (fun struct (fields, line) ->
                if field fields 0 = "" || field fields 1 = "" || field fields 2 = "" then
                    BulkFile.fail "An establishment row has no CNPJ"

                struct (fields, line))
            |> Array.ofSeq

        let headOffices =
            establishments
            |> Seq.filter (fun struct (fields, _) -> field fields 3 = "1")
            |> Seq.map (fun struct (fields, _) -> field fields 0, field fields 0 + field fields 1 + field fields 2)
            |> readOnlyDict

        let records =
            establishments
            |> Array.choose (fun struct (fields, line) ->
                let basis = field fields 0

                match firms.TryGetValue basis with
                | true, (firm, _) ->
                    let held =
                        match partners.TryGetValue basis with
                        | true, held -> List.ofSeq held
                        | _ -> []

                    Some(record allSocios firm held headOffices fields line)
                | _ -> None)

        let index = BulkIndex<BulkRecord>()

        records
        |> Array.iter (fun record ->
            normalCnpj record.Company.Number
            |> ValueOption.iter (fun key -> index.Add(key, record)))

        // A base on two company rows can't name the company of any establishment under it.
        repeatedBases |> Seq.iter index.Repeat

        establishments
        |> Array.iter (fun struct (fields, _) ->
            if repeatedBases.Contains(field fields 0) then
                index.Repeat(field fields 0 + field fields 1 + field fields 2))

        // A base finds its head office, alongside the head office's own CNPJ.
        records
        |> Array.filter (fun record -> record.Company.Role <> RecordRole.Establishment)
        |> Array.iter (fun record -> index.Add(record.Company.Number.Substring(0, 8), record))

        BulkRegistry(
            coverage,
            normalCnpj,
            invalidNumber,
            index.Held,
            date,
            BulkAbsence.CantTell
                "The host passes some of the month's files, so a CNPJ in a file not passed, or of a company whose Empresas row wasn't passed, isn't in the index",
            "The QSA in the files passed lists no administrator for this company",
            (fun _ -> NotAvailable "The QSA in the files passed lists no partner for this company"),
            index.Repeated
        )
