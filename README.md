# CompanyDueDiligence

An F# library that connects evidence about a business client into one case file for a compliance officer. Unofficial: not affiliated with or endorsed by any
register, list or vendor it reads. It targets `net10.0` and depends only on FSharp.Core and the .NET base libraries.

**The officer decides. The library never decides and never says "clear".** It gathers, connects, flags and states gaps. A case file has no overall result, and
`CaseFile.reviewNeeded` lists what to look at. An empty list wouldn't mean anything was cleared.

Vendor checks (Experian KYB, Plaid Monitor) run first and are the preferred source. The host adds them as [`VendorCheck`](#vendor-checks). A vendor's result stays that
vendor's, on that date.

The rule throughout is fail closed: an unreadable answer, a parse failure, a failed source or anything that can't be followed becomes a gap. It never reads as
nothing found.

Companies House was run live on 2026-11-08: profiles, officers, owners, filings, insolvency, UK establishments, search, person search and full cases, on companies of each type. **The other sources behind a key have not been run against their live APIs**: their answers are shaped from the published specs. See
[what to check live](#what-to-check-live). The keyless sources were called live on 2026-11-06 or 2026-11-08, except the Netherlands' KVK, which answered 429. Their recorded answers are in `tests/CompanyDueDiligence.Tests/Fixtures`, with people's names redacted.
[Sources](#sources) lists the 58 registers read here, and GLEIF, and [docs/candidate-sources.md](https://github.com/Thorium/CompanyDueDiligence/blob/HEAD/docs/candidate-sources.md) covers every other country.

## The case file

| Part | Holds |
|---|---|
| `Entities` | Each party, with an identity of register and identifier, never a bare number: `EntityIdentity.OnRegister`, `GermanCourtRegister` (court, section, number), `Lei`, or `NameOnly`. A UK record, a Delaware home record and a Texas registration of one business are three entities, each with its own `Standing`. |
| `Links` | `HomeRecord`, `ForeignRegistration`, `UkEstablishment`, `Owner`, `Officer`, `Parent`, `Creditor` or `StatedRelationship`, with `Provenance`: `Register`, `FilingDocument`, `Vendor`, `CrossCheck` or `OperatorDeclared`. `CaseLink.isEvidenced` is false for a declared link, and every view says so. |
| `Evidence` | `Source`, `Origin` (register, vendor, cross-check, list, filing document or operator), `RetrievedAt`, `AsOf` where the source gives one, `Raw` as received, and `Facts` to compare by. |
| `Flags` | Neutral and sourced: status, overdue filings, insolvency, registered office, shared or virtual address, nominal capital, past-due identity verification, disagreement, not on the register, possible sanctions match, vendor hit, an officer's own. |
| `Gaps` | `NotChecked`, `NoRoute`, `NeedsPaidContract`, `NeedsEntitlement`, `SourceFailed`, `NotAvailableForCountry`, `NotPublished`, each with `WouldClose`. `Reach` is `WholeCase` for what leaves the picture incomplete, such as an unread parent. |
| `Documents` | Filings to read, with link, kind and page count where known. `CaseDocument.isHighlighted` marks insolvency and constitutional ones, and each of those is a gap until read. |
| `Notes`, `VendorChecks`, `Screenings` | As entered, with who, when and the source. |

Rules that follow from it:

- `CaseFile.flagsSeenFrom` shows a flag on the entities linked to it, in either direction, with the links it came through. `describeSeen` says it is about the other entity and shows only
  that a link exists. A forfeited branch shows on the home record that way, and sets no flag there.
- `Discrepancies.forEntity` compares what sources say of one entity: name, number, status, address, legal form, capital, officers. It keeps every value and picks none. A `Disagreement` is flagged.
  `DifferentKinds` is labelled and not flagged: a registered agent's address against a principal office, or authorised against issued capital in different figures.
- Capital alone is compared across one business's records, joined by home record, foreign registration or establishment links, so that a UK form can be set against a home certificate. The same figure
  under another word is a disagreement: 10,000,000 "issued" against 10,000,000 "authorised".
- Status isn't compared across records.
- What a pointer says of the far record is evidence about that record, so a UK record's stated legal form and home number are compared with what the home record says.
- Each part of a register's `Coverage` that is `Partial` (standing, concerns, addresses, owners) is a gap carrying its caveat, so a tax register's "Registered" is read as a tax registration only.
  A register with no status word is a `standing` gap: its "Registered" confirms no standing.
- An answer whose data is older than the day it is read, by its `AsOf` or, for a downloaded file, its `RetrievedAt`, is a `data date` gap: later changes aren't seen. So is any answer from a downloaded file that states no date of its own, whenever it was downloaded. "Not held" from such data says
  the date in its flag, and is a gap on the whole case, since the company may have been formed since.
- `Evidence.Raw` can hold personal data. So can a case's JSON and its export with `includeRaw`.

## Working a case

```fsharp
open CompanyDueDiligence

let connection = SourceConnection.Create()
let companiesHouse = Sources.companiesHouse connection (SourceKey apiKeyFromConfiguration)

let sources =
    { CaseSources.ofRegistries
          [
              companiesHouse
              Sources.malta connection
              Sources.france connection
              Sources.finland connection
              Sources.texas connection
          ] with
        CrossCheck = ValueSome(Sources.gleif connection) }

task {
    let! case =
        Investigation.openAsync
            sources
            InvestigationOptions.defaults
            (CaseStart.create "CASE-1" "00000006" (Jurisdiction.ofCode "GB") "As the customer gave it")
            cancellationToken

    let screened = Investigation.screen listTexts InvestigationOptions.defaults case
    return CaseFile.reviewNeeded screened, CaseJson.toJson screened, CaseExport.toText false screened
}
```

- **Following links.** `openAsync` starts from one company and follows corporate owners, UK establishments, home records and GLEIF parents into whichever source covers that country. `MaxDepth` (4) and
  `MaxEntities` (25) are choices made without data. One record is read once, so a loop ends. What it can't follow becomes a gap: no number, wording it can't place, no source. Placing reads a
  wording whole and never guesses: `New South Wales` isn't Wales, and `Georgia` alone places nothing. A stated country it can't place stays a gap, whatever a register's wording says.
- **Several registers for one place.** Every register covering the jurisdiction is asked, those naming the exact code (`CA-BC`) before those covering the country (`CA`), each in `Registries` order.
  The first that holds the company is the record followed. The others' answers are kept as evidence on the same entity, so `Discrepancies` compares them, and their status and stated concerns are flagged.
  An answer of outside coverage or "can't tell" doesn't hide the next register. When every register answers outside coverage, as Corporations Canada does for a province code, the record is a route or
  `NoRoute` gap, not a failure.
- **GLEIF in every outcome.** The LEI cross-check is read whether a register held the company, said it doesn't, couldn't tell, failed, or no source covers the country, and its parents are followed. When the number as asked finds nothing, the number as the register wrote it is asked too, and an answer holding no record is kept as evidence.
- **Places without numbers.** A home record or registration that names a place and no number is placed: the gap says where, and is that place's declared route (Delaware's paid contract) when there is one.
  The home entity is keyed by name and the place, so Colorado, Connecticut and New York registrations of one Delaware company reach one home entity. A bare two-letter code on a record held in the US or
  Canada is that country's state or province code (`DE` from Colorado is Delaware, `AB` from British Columbia is Alberta). Anywhere else it is a country code, and it gives way to a US state another
  wording names.
- **Owners and officers.** An owner's natures of control, kind and band as published, are the owner link's `Note`. Current owners with no band at all are a `shareholdings` gap. An officer whose name is
  withheld or not given is an entity of its own, told apart by role and place in the list, and an `officers` gap of its own.
- **Following by hand.** `Investigation.followAsync` reads one more entity into a case, such as one an officer declared.
- **Filings and readers.** `IDocumentReader` is pluggable and no OCR ships, because scanned filings are normal at Companies House. `Investigation.readDocumentAsync` fetches through an `IFilingRegister` and
  a reader. A failure leaves the document unread and adds why.
- **Manual evidence.** `CaseFile.addFinding`, `addNote`, `raiseFlag`, `attachReading`, `declareLink` and `closeGap` need an `Attribution`: who, when and the source. `attachReading` closes that
  document's not-read gap only. `linkFromDocument` needs the document and its reading, and gives a link with `FilingDocument` provenance. `closeGap` needs evidence.
- **Storage.** The library stores nothing. `CaseJson.toJson` and `ofJson` round-trip a case. Reading is strict: another `SchemaVersion`, an unknown field or case, or a missing field refuses the whole file, because a
  host re-saving what it partly read would lose the rest.
- **Audit file.** `CaseExport.toText includeRaw case`.
- **Addresses.** No source read here says an address is a virtual office. `SharedAddress` comes from two entities in a case giving one address. `VirtualAddress` comes from a `KnownAddressList` the host holds, or a
  vendor's hit shown as one. None ships.
- **Nominal capital.** `NominalCapitalAtMost` (100, in any currency) is a choice made without data, and `description_values.capital` on a Companies House filing is the only figure read automatically. Set your own.

## Sources

`RegistryCoverage` states each source's cost, pace and licence. Sources are in three groups:
- **Lookups with no key** were called live on 2026-11-06 or 2026-11-08 with a few read-only calls, except the Netherlands' KVK, which answered 429.
- **Lookups behind a free key** were written from the official specs. Companies House was run live on 2026-11-08; the others haven't been, for want of keys.
- **Bulk sources** read files the host downloads.

A row says what was run live and what is unverified. Recorded answers are in `tests/CompanyDueDiligence.Tests/Fixtures`, with people's names redacted.

A source answering for a country also answers for its subdivisions (`Jurisdiction.covers`), and `Registries.forJurisdiction` takes the first register naming the exact code (`CA-BC`), else the first covering the country, so list order matters only among registers at the same level. Corporations Canada refuses province codes itself.

`Registries.searchByNameAsync registry name matching jurisdiction ct` searches by name as `NameMatch.Exact` (the whole name after `NameMatch.normalise`), `StartsWith` or `Contains`. `NameMatch.normalise` spells Cyrillic and Greek letters in Latin, as [sanctions matching](#sanctions) does, so Exact and StartsWith compare a name in those scripts with its Latin spelling. A part of a Greek name asked in Greek (`Αυ` for `Αύρα`) also matches letter by letter, as the Latin spelling of `ΑΥ` and `ΓΓ` depends on the letters around them. Colorado, Connecticut, New York (both lists), Oregon, Pennsylvania, Texas, Colombia and DC match in their own query (`INameMatchSearch`): each asks for the normalised words in order, with anything around and between them, then keeps the names `NameMatch.matcher` accepts. So all three modes find a name differing in case, separators, and full stops or apostrophes after a word's first letter, before a final S or inside a word of up to three letters, but compare accents as held. Bulk sources and their indexes match all three on the names they hold, with Contains as `SearchAsync`; their Contains compares upper case only, so `PLC` doesn't find `P.L.C.`. These queries were run live on 2026-11-08. Every other register answers Contains with its own search and Exact or StartsWith by narrowing that search's page, which is never `Complete`; that includes Taiwan, whose `eq` on a name also contains, and the CKAN full-text searches (Israel, Singapore, Latvia, Ireland).

### Lookups with no key

| Source | Answers | Live answer | Not offered | Pace | Licence |
|---|---|---|---|---|---|
| Malta Business Registry, `Sources.malta` | `MT`, by number such as `C 1`, which `C1`, `C-1` and `C/1` also name | Name, type, one of 14 state words, address, status date. A 404 is not held only in the registry's own error shape, and the answer must carry the number asked. The `address` field can hold a note such as `STRUCK OFF WEF 23/04/81`, read as no address. | Search, officers, owners, concerns | None published; paced 2 a second, a choice | CC BY 4.0, per data.gov.mt's Basic Company Information record |
| Recherche d'entreprises, `Sources.france` | `FR`, by SIREN | `A`/`C`, head office, officers including auditors, SIREN of corporate officers. **It answers any nine digits with a stub with no name and a date, which is not a company.** Not found is `NotAvailable`, because non-diffusible companies are left out. | Owners, concerns | 7 a second, published, and 30 a second per ASN | Licence Ouverte 2.0, per data.gouv.fr; the API's own record states none |
| PRH/YTJ, `Sources.finland` | `FI`, by Business ID | Names, form, addresses, trade register status (`Registered`, not active), Business ID status, situations (bankruptcy, liquidation, re-organisation), last modified. **It answers a value of the wrong shape with an empty list**, so one isn't sent. **The open data holds Trade Register companies, ended ones among them, and leaves out sole traders and entities not on the Trade Register**, so an empty answer is `NotAvailable` and a search is never complete; YTJ's own web service holds them. An ended company is read by its last name and form, with `CeasedOn` from its `endDate`; a search leaves out a company it can't read. | Officers, owners | 429 documented, no figure; paced 2 a second, a choice | CC BY 4.0, per its schema |
| Brønnøysund Enhetsregisteret, `Sources.norway` | `NO`, by 9-digit organisation number (mod-11) | Bankruptcy, winding-up and forced-dissolution markers; no status word, so a live entity is `Registered`. Brønnøysund's error object for both the entity and the sub-unit is not held; two empty 404s are `NotAvailable`, as an entity removed for legal reasons answers them too; a sub-unit is `NotAvailable`, naming its parent. A struck-off record is `Closed` with its date. **A 410 (removed for legal reasons) is `Closed` with no name.** Roles: CEO and board as directors; resigned roles and deceased people are former. NUF is a foreign registration. Search is never complete: it leaves out deleted entities. | Owners, overdue filings | None published; paced 2 a second | NLOD 2.0 |
| ARES, `Sources.czechia` | `CZ`, by 8-digit IČO | State from the public register, else the trade register; "v likvidaci" in the name is `WindingUp`, whatever punctuation stands around it; an insolvency-register listing qualifies `Active`. 404 `NENALEZENO` is `NotAvailable`: ARES leaves out entities deleted from the register, so a search is never complete either. Officers with dates. **Owners: partners with % share and a sole shareholder**, legal and not beneficial. | Overdue filings | 500 a minute, per the ARES terms | None stated; MF terms limit automated use (500 a minute, no spreading over IPs) |
| RPO, `Sources.slovakia` | `SK`, by 8-digit IČO | A search, then the record (2 calls). No result is `NotAvailable`, as RPO is a nightly replica; several records for one IČO are `NotAvailable`. Termination is `Closed`, code 2 `WindingUp`, code 3 `InInsolvency`. Officers: statutory bodies and prokurists. Owners: partners and shareholders. Lags up to 24 h. | Beneficial owners (withdrawn without a legitimate interest), overdue filings | None published; paced 2 a second | CC BY 4.0, per each answer |
| CRO open data, `Sources.ireland` | `IE`, by company number | Daily snapshot of 828,842 records, dissolved ones included. All 33 status codes mapped; "Liquidation stayed", a winding up the court has stayed, is `WindingUp`. Absence is `NotAvailable`, as a daily copy. Types 1128 and 1139 are foreign registrations. | Officers, owners, concerns | None published; paced 2 a second | CC BY 4.0 |
| KRS, `Sources.poland` | `PL`, by KRS number | Status from section 6: liquidation `WindingUp`, bankruptcy and restructuring `InInsolvency`, none `Registered`. **A deleted entity answers 204**, so the full extract is read for its deletion date. 404 in both registers is `NotAvailable`. **The register masks people's names**, so officers and owners are `Withheld`. KRS lists LLC partners only at 10% of capital or more, so a gap stands unless one partner holds every share. | Search, overdue filings | None published; paced 1 a second | None named; reuse under the 2021 open data act, art. 24, per the KRS API page |
| UID register, `Sources.switzerland` | `CH`, by UID (mod-11) | SOAP. UID status with the commercial-register entry status; liquidation wording in the name is `WindingUp`: "in Liquidation", "in Nachlassliquidation", "in Liq.", "en liquidation", "en liq.", "in liquidazione" and "en liquidaziun", as words of their own. **A UID with two seats is `NotAvailable`**, and so is an empty answer, as non-public entries are withheld. | Officers, owners | 20 a minute, published by BFS | None stated; Public Services are for unregistered users. OPEN-BY covers the BFS spec only |
| Latvian Register of Enterprises, `Sources.latvia` | `LV`, by registration number | CKAN on data.gov.lv: register, officers, beneficial owners. Liquidated is `Dissolved`; blank is `Registered`. Legal address, form, dates, current officers, **registered beneficial owners** (no nature of control). Daily copy, so absence is `NotAvailable`. | Concerns | None published; paced 2 a second | CC0 1.0 |
| KVK open dataset, `Sources.netherlands` | `NL`, by 8-digit KVK number | **Not run live: 429 to every call on 2026-11-08**; from `api_opendata_basis.yml` v1.1.0. `actief`, insolvency code, legal form. **No name is published.** 404 is `NotAvailable`, since only BVs and NVs are held. | Search, addresses, officers, owners | 1 a minute per IP and 200 per 5 minutes overall, published | CC BY 4.0 |
| Texas Comptroller, `Sources.texas` | `US-TX`, by SOS file or taxpayer number | Name, address, type, SOS status, right to transact. **Despite its name it holds forfeited and ended records.** **Some file numbers have two rows**, read as `NotAvailable`. Absence proves nothing, and a search is never complete, as it holds almost no dissolved companies. Not the Secretary of State's register. | Officers, owners, concerns | None published; paced 2 a second | None stated; the portal links only Tyler's terms |
| Colorado Department of State, `Sources.colorado` | `US-CO`, by entity ID | 3.1 million entities since 1864. All 15 status words; formation place gives home or foreign; principal, mailing and agent addresses. Daily copy, so absence is `NotAvailable`. | Officers (not collected), owners, concerns | None published; paced 2 a second | Public Domain |
| Connecticut Secretary of the State, `Sources.connecticut` | `US-CT`, by business ID | Status and sub-status, 74 pairs mapped; a past-due report is a stated `AnnualFilingOverdue`. Citizenship gives the role. Officers from the Principals dataset (two calls). Absence is `NotAvailable`. | Owners, agents | None published; paced 2 a second | Public Domain (Business Master); Principals states none |
| NY Department of State, `Sources.newYork` | `US-NY`, by DOS ID | Active Corporations (monthly, active only) and the newer status history, which decides when it isn't Active; the filings for an inactive ID. Search reads the active list and All Filings (63wc-4exh), which holds dissolved entities, with no status for one only there; never complete. Officers: the CEO only. | Owners, concerns, addresses of an inactive entity | None published; paced 2 a second | None stated; OPEN-NY terms: revocable licence, no attribution, a no-bots clause |
| Oregon SOS, `Sources.oregon` | `US-OR`, by registry number | Active only, weekly. Rows for one number collapse to one company with its addresses. Assumed and reserved names are refused. Absence is `NotAvailable`, and a search is never complete. | Officers, owners, concerns | None published; paced 2 a second | Public domain, per ORS 276A.368 |
| Pennsylvania DOS, `Sources.pennsylvania` | `US-PA`, by filing number | **No status column**: standing is `Registered` with no word, and the state warns the data overstates active businesses. Officers read. Searches are never complete: the data holds active registrations only. | Owners, concerns | None published; paced 2 a second | Public Domain |
| DC Corporations Division, `Sources.districtOfColumbia` | `US-DC`, by file number | ArcGIS: 12 status words, locale for the role, addresses. **Every trade name filed** is another name, read in a second call that fails the lookup if it fails. A biennial report past its due date is a stated `AnnualFilingOverdue`. **Beneficial owners** by name and address, with `OwnerDetailsNotConfirmed`. | Officers, disputes, insolvency | None published; paced 2 a second | CC BY 4.0, per each layer's ArcGIS item |
| OrgBook BC, `Sources.britishColumbia` | `CA-BC`, by number such as `BC0772006` | `ACT` is `Active`, `HIS` is `Closed` with its reason; home jurisdiction not BC is a foreign registration. Only the exact `source_id` is taken. Absence is `NotAvailable`. | Addresses, officers, owners, concerns | None published; paced 2 a second | "Access Only License - British Columbia": reproduction needs written permission |
| RUES on datos.gov.co, `Sources.colombia` | `CO`, by NIT or chamber and matrícula | All 14 matrícula states; a name ending "en liquidación" is `WindingUp`, and "liquidación judicial" or "reorganización" `InInsolvency`; an unrenewed matrícula is `ActiveWithConcern` with `AnnualFilingOverdue`. Legal representative. Absence and duplicates are `NotAvailable`, except two rows of which exactly one is code 09, cancelled by a move of domicile: the other row is read, the move noted in `StatusAsPublished`. | Addresses, owners, insolvency | None published; paced 2 a second | CC BY-SA 4.0 |
| Hacienda, `Sources.costaRica` | `CR`, by cédula jurídica | **Tax registration only**, not the Registro Nacional: `Inscrito` is `Registered`; arrears or a missed return is `ActiveWithConcern`; `Desinscrito`, as in "Desinscrito oficio", is `Closed` with the word. 404 is `NotAvailable`. | Addresses, search, officers, owners, concerns | Published: 10 a second, 10-minute IP block; paced 2 a second, no retry after a 429 | None found; the docs answered 403 on 2026-11-08 |
| Israeli Companies Registrar, `Sources.israel` | `IL`, by 9-digit company number | CKAN on data.gov.il. Hebrew status and sub-status, all seen mapped; a violating company is `ActiveWithConcern`. The corporation type gives Israeli (home) or foreign. Daily copy, so absence is `NotAvailable`. | Officers, owners | None published; paced 2 a second | "Other (Open)" |
| ACRA on data.gov.sg, `Sources.singapore` | `SG`, by UEN | `Registered` / `Deregistered`, type, street, postcode; a foreign company branch is a foreign registration. Monthly copy, so absence is `NotAvailable`. | Officers, owners, concerns | 4 per 10 s, published for keyless use | Singapore Open Data Licence |
| MOEA GCIS open data, `Sources.taiwan` | `TW`, by UBN (check digit) | All 33 status words and 廢止登記 (code 10, `Closed`), suspension, ROC-calendar dates, directors and supervisors (a juristic person holding a seat is a `LegalEntity` officer). **A UBN not held answers an empty 200**, read as `NotAvailable`. Search asks status 01 and the ended 04, 05, 07, 08, 09, 10, 11 and 13, one call each under the pacing; never complete. | Owners, concerns beyond insolvency | Daily cap, no figure; paced 2 a second | Open Government Data License v1, attribution required. **The API guide requires an emailed usage notice and IP registration before use**; not in the demo |
| GLEIF, `Sources.gleif` | Any firm with an LEI | Legal name, status, addresses, reported number and jurisdiction, parents, reporting exceptions. Kept as `CrossCheck`, never as a register. Only a record in the jurisdiction asked is attached. A number's list is read a page of 100 at a time up to 200; records past that are `Unread`, a gap, and none is attached. | | 60 a minute; unverified | CC0, as GLEIF states |

### Lookups behind a free key

None of these has been run live. Each takes its key as a `SourceKey`, or as `ClientCredentials` for a user and secret. Printing either shows nothing, and a key travelling in a query string is taken out of failures. [docs/keys.md](https://github.com/Thorium/CompanyDueDiligence/blob/HEAD/docs/keys.md) says where to get each key, and `KeyedSources.fromEnvironment` builds every source whose `CDD_*` variables are set.

| Source | Answers | Written from | Not offered | Pace | Licence |
|---|---|---|---|---|---|
| Companies House | `GB`, with a key | Run live 2026-11-08; see [what to check live](#what-to-check-live) | | 600 per 5 minutes, per its guide | No rules on use, per Companies House; bans for exceeding 600 per 5 minutes |
| Corporations Canada, `Sources.canada` | `CA` only, by corporation number or BN9 | `corporations-en.json`. Name history, status, registered office, directors. An unknown number answers 200 "could not find corporation", read as not held. | Search, owners, concerns | 60 a minute, published | OGL-Canada for the dataset |
| NZBN API, `Sources.newZealand` | `NZ`, by NZBN or company number | The portal's OpenAPI. 17 status codes, addresses, directors, **shareholdings as legal owners**, insolvencies. A 404 is not held only with `errorCode` NZBN-404. | | Paced 2 a second | NZ Crown copyright, with credit |
| Bolagsverket, `Sources.sweden` | `SE`, by organisation number | The devportal's OpenAPI and code lists. Deregistration reasons, proceedings, active flag. `ORGANISATION_FINNS_EJ` is `NotAvailable`, as Bolagsverket says the organisation may still exist. A fault in a block deciding standing leaves it `Unknown`. OAuth2 credentials go in a Basic header, unverified against the gateway. | Search, officers, owners | 60 a minute per user; not re-read | None stated; free, no agreement, per bolagsverket.se |
| NTA corporate number, `Sources.japan` | `JP`, by 13-digit corporate number | The v4 specification. Process codes, close causes, head office. A 404 means the app ID is invalid. | Officers, owners, concerns | Paced 2 a second | NTA terms; the attribution sentence is in `Coverage.Licence` |
| Zefix PublicREST, `Sources.zefix` | `CH`, by UID | `/v3/api-docs`. ACTIVE / BEING_CANCELLED / CANCELLED, legal form, domicile, deletion date. Basic credentials from the EHRA. | Officers, owners, concerns | Paced 2 a second | OGD: open use, source cited |
| California SOS, `Sources.california` | `US-CA`, by entity number | Guide v1.0.4 (draft). Only two status words documented; a 400 is `NotAvailable`, as the guide gives it for both no entity and a bad request. **The portal says government agencies only.** | Officers, owners, concerns | Paced 2 a second | None stated; the portal says government agencies only, approval required |
| ABN Lookup, `Sources.australia` | `AU`, by ABN or ACN | The ABR's JSON samples. An active ABN is `Registered`, not `Active`: it isn't ASIC registration. No record for an ACN is `NotAvailable`. Names, state, postcode. A suppressed record is `NotAvailable`. | Officers, owners, concerns | Paced 2 a second | ABR Web Services Agreement |
| NTS business status, `Sources.korea` | `KR`, by business number | The data.go.kr swagger. Continuing is `Registered` (tax registration), suspended `Unknown`, closed `Closed`; the check digit is checked. **No name or address.** | Search, officers, owners, addresses, concerns | 1,000,000 a day, published | No restriction on use |
| Sudski registar, `Sources.croatia` | `HR`, by OIB or MBS | The OpenAPI v3.0.4. Name, seat, legal form, founding and deletion dates, bankruptcy and liquidation flags. OAuth2. | Search, officers, owners | 6 a minute, a third party's figure | Otvorena dozvola (NN 67/2017), per data.gov.hr; registration required |
| GEMI open data, `Sources.greece` | `GR`, by GEMI number or AFM | The swagger. Names, address, legal form, persons as officers; status by the codebook's active flag, with liquidation and bankruptcy words deciding over it. Nine bare digits passing the AFM check are refused as ambiguous; a 404 counts only in GEMI's error-list shape. | Owners, concerns | 8 a minute, a third party's figure | ODC-BY-1.0 |
| Firmenbuch HVD, `Sources.austria` | `AT`, by Firmenbuchnummer | `WebService-HVD.zip` v1.3. Name, address, legal form; `Closed` when deleted. **A SOAP fault sent with HTTP 500 reads as `Unavailable`, and nothing reads as not held.** | Officers, owners, legal facts | 429, no figure; paced 2 a second | CC BY 4.0, per data.gv.at's HVD record |
| CVR, `Sources.denmark` | `DK`, by CVR number | "Kom godt igang med Elasticsearch". Name, address, composite status, life cycle. A timed-out or shard-incomplete answer is `Unavailable`. **The only endpoint is plain http, so the credentials travel unencrypted.** | Officers, owners, legal form | Paced 2 a second | CC BY 4.0, per data.gov.dk; advertising-protected firms not for direct marketing |
| KRA PIN checker, `Sources.kenya` | `KE`, by company PIN | developer.go.ke. **Tax PIN status only**, not the business register. OAuth key and secret. | Addresses, search, officers, owners, concerns | Paced 2 a second | None found; free to approved developers under KRA's developer guidelines, not read |

### Bulk sources

The host downloads the published files on its own schedule and passes them as streams with a `BulkFileDate` and a `CancellationToken`, through `BulkSources`. Files are read as published, zips included, with UTF-8 read strictly: a file with an invalid byte is refused rather than read with replacement characters. Nothing is called. Answers are as of the file's date, and a number held twice in one file is `NotAvailable`. A delimited file that ends without a line end, in a record with fewer fields than its header, is refused as cut short (Belgium, Cyprus, Romania, Slovenia, Buenos Aires, ASIC, Alaska, Chile's register and Peru); a cut that leaves the last record its field count can't be told, and NUL padding or a Ctrl-Z after the last record isn't a record. Building the larger ones takes time and memory: Ukraine's whole uo.zip takes about 8.7 GB, Romania holds 4.2 million firms in about 52 s and 2.25 GB, and Estonia takes about 1.9 GB. Build each once per download and save it with `BulkIndexes`; `BulkDownloads` writes Ukraine's index streamed, at a 1.2 GB peak.

Most users need few of these, so nothing is downloaded until asked. `BulkCatalogue.all` lists every bulk source with its publisher's page, a size for those with fixed links or a resolver, how often the file is replaced and its licence: a settings screen with a download button per source. `BulkDownloads` fetches sixteen of them into a folder the host chooses: seven at fixed links (`BulkDelivery.Links`), and nine whose file names change (`BulkDelivery.Resolved`), found at download time through the portal's official listing:

```fsharp
use downloads = BulkDownloads.Create("C:/data/registers")
let estonia = (BulkCatalogue.tryFind "EE").Value
let! saved = downloads.DownloadAsync(estonia, ValueNone, ct)      // on the user's request
let! registry = downloads.LoadAsync(estonia, ct)                 // add it to CaseSources.Registries
```

- `LoadAsync` builds the registry once per download and saves an index beside the files; every later load opens the index. `IndexStateOf` says whether one is built.
- `StateOf` says whether a source is saved, when, its size, and whether it is older than its publisher's cadence.
- Every file of a download goes to a part file first, and they replace the saved copy together, so a failure leaves the last complete copy. A file under a twentieth of its stated size, such as a page served in its place, is refused, and so is one that sends nothing for two minutes.
- A download's data date (`BulkFileDate.AsOf`) is the oldest date its publisher states: the listing's, else the file's Last-Modified.
- An index records the library version that built it; another version's index is rebuilt.
- A file over four times its catalogued size is refused. A resolved file's size is the one its listing states, or the source's stated size when the listing states none; never the download's own Content-Length. A listing whose sizes add up to over four times the source's stated size is refused before anything is fetched.
- `ResolveAsync` reads a resolved source's listing and returns the files a download would fetch, with their names on the portal and dates, without downloading. `ResolvedFilesOf` gives the files of the download saved now.
- When a listing can't be read, or names none of the files, the download fails and its message says how to get them by hand (`BulkResolver.ByHand`); pass them to `BulkSources`.
- Four stay `BulkDelivery.ByHand`: Belgium needs a registration, Florida SFTP, Moldova's page has no API or machine listing, and the SII nómina needs SII's approval, as its site terms allow personal non-commercial use only.

| Resolved source | Listing | Files picked | Stated size |
|---|---|---|---|
| `CY` Cyprus | data.gov.cy dataset 1026, its DCAT-AP 2.0 RDF/XML | organisations_N.csv, registered_office_N.csv, organisation_officials_N.csv | 242 MB |
| `RO` Romania | data.gov.ro CKAN `package_search`, organisation onrc | OD_FIRME, OD_STARE_FIRMA and OD_REPREZENTANTI_LEGALI of the newest `firme-` dataset; N_STARE_FIRMA of the newest `nomenclatoare-` one | 1.15 GB |
| `SI` Slovenia | podatki.gov.si CKAN `package_show` poslovni-register-slovenije | opsiprs.csv | 126 MB |
| `UA` Ukraine | data.gov.ua CKAN `package_show` 03cc1239-… | UO.zip | 328 MB |
| `RU-SME` Russia | nalog.gov.ru meta.csv (Russian open data standard 3.0) | the newest data-DDMMYYYY-structure-….zip it lists; meta.csv can trail the dataset page by a month | 2.3 GB, a ceiling, as meta.csv states no size |
| `BR` Brazil | WebDAV PROPFIND on the public share arquivos.receitafederal.gov.br redirects to | Empresas, Estabelecimentos and Socios 0 to 9 of the newer of the two newest month folders that holds all thirty | 7.5 GB |
| `AU-ASIC` ASIC | data.gov.au CKAN `package_show` asic-companies | the newest ZIP of "Company Dataset - Current" | 79 MB |
| `CL-RES` Chile | datos.gob.cl CKAN `package_show` registro-de-empresas-y-sociedades | every yearly CSV, 2013 to now, since a formation of any year can be asked for | 250 MB |
| `AR-C` Buenos Aires | datos.jus.gob.ar CKAN `package_show` | the newest "IGJ - YYYY semestre N" zip, by the year and half its name states | 162 MB, a ceiling, as CKAN states no size |

`BulkIndexes.saveAsync` writes a built registry to a folder, and `BulkIndexes.openAsync` opens it in milliseconds as an `IBulkRegistry` that answers exactly as the built one, holding file handles rather than records; dispose it to close them. It refuses an index another version of the library wrote, whose rules for numbers and names can differ: save it again. Lookups are positional reads, safe in parallel. A search reads the whole names file. `BulkIndexes.saveUkraineAsync` writes Ukraine's index as uo.zip streams, never holding every record. On the real files:

| | Build in memory | Save | Index on disk | Open | Heap after open | Lookup | Search, whole file |
|---|---|---|---|---|---|---|---|
| ЄДР uo.zip of 2026-11-03 | 58 s, 8.7 GB heap | 34 s | 2.3 GB | 20 ms | under 0.1 MB | 54 µs | 170 ms |
| ЄДР, streamed | | 88 s, 1.2 GB peak | 2.4 GB | | | | |
| SII nómina | 15 s, 1.6 GB heap | 34 s | 1.1 GB | 20 ms | under 0.1 MB | 51 µs | 170 ms |

| Source | Answers | Read from | Not offered | Licence |
|---|---|---|---|---|
| e-Business Register, `BulkSources.estoniaAsync` | `EE`, by registry code | The real zips of 2026-11-07. Status, foreign branches, registry-card persons, shareholders with their percentages, **beneficial owners**, with gaps for owners given only as board members and for a company with no shareholder rows. **Current entities only**, so absence is `NotAvailable`. | Former officers, overdue filings | CC BY 4.0 |
| Registrar of Companies, `BulkSources.cyprusAsync` | `CY`, by type and number (`HE 1`) | The real CSVs of 2026-11-02. 18 status words; officials without dates. **Numbers repeat across types**, so a bare number isn't looked up. Struck-off records are included, so absence is not held unless the file has malformed rows. | Owners (`NeedsEntitlement`), concerns beyond insolvency | CC BY 4.0 |
| ONRC, `BulkSources.romaniaAsync` | `RO`, by registration number or CUI | The real CSVs of 2026-11-04. All 197 state codes placed; legal representatives; a foreign parent's branch is an `Establishment`. Removed firms are included, so a CUI the file lacks is not held. A registration number it lacks is `NotAvailable`: the file writes each firm's number in one of two forms, `J40/123/2020` or `J2020000123400`, and nothing in it proves how one maps to the other. 95,158 CUIs repeat. | Owners | CC BY 4.0 |
| AJPES PRS list, `BulkSources.sloveniaAsync` | `SI`, by registration number | **Not run on the real file.** The portal answered curl with 407 on 2026-11-08 but answered BulkDownloads' client, whose listing named opsiprs.csv of 126 MB. Number, name, legal form, address; `Registered` with no word. | Officers, owners, concerns | CC BY 4.0 |
| CBE/KBO, `BulkSources.belgiumAsync` | `BE`, by enterprise number (mod-97) | **Not run on the real file: it needs a registration.** Cookbook R018.00 and the official code table. **Active entities only.** Name and address type codes unverified. | Officers, owners | CBE licence: reuse only for the purpose registered, no direct marketing |
| ЄДР, `BulkSources.ukraineAsync` | `UA`, by EDRPOU | The real uo.zip of 2026-11-03 and its XSD. 6 states, founders (with a gap, as their stakes are in hryvnias), **beneficial owners with percentages**, managers. | Addresses (withheld in wartime) | CC BY |
| Division of Corporations, `BulkSources.floridaAsync` | `US-FL`, by document number | **Not run on the real file** (SFTP); the fixed-width layout. Active / inactive, filing type for the role, up to 6 officers, agent. Pass cordata.zip as one stream; a daily file replaces quarterly records, and a number in two quarterly parts is repeated. A record flagged for more than six officers answers `NotAvailable` for officers. Date format assumed. | Owners, concerns | None stated; free downloads for informational purposes |
| Corporations downloads, `BulkSources.alaskaAsync` | `US-AK`, by entity number | The real downloads. Officials split into officers and owners; only holders of 5% or more are named, so a gap is stated. The index page is behind a captcha; terms unread. | Concerns, owner shares | None found; the download page is behind a captcha |
| ASIC company dataset, `BulkSources.asicCompaniesAsync` | `AU`, by ACN or ARBN | The real weekly zip, 4.5 million rows, built in about 18 s. 11 statuses, former names. Deregistered over a year ago are dropped, so absence is `NotAvailable`. | Addresses, officers, owners, concerns | CC BY 3.0 AU |
| DNIT RUC list, `BulkSources.paraguayAsync` | `PY`, by RUC | The real ruc0.zip. 5 statuses; active is `Registered` as a tax registration. **Individuals included.** A malformed row withholds its RUC; absence is not held only with all ten files and every row tied to its RUC, which the real files of 2026-11-01 aren't, so absence reads `NotAvailable`. | Addresses, officers, owners, concerns | None stated; the site says all rights reserved |
| Receita Federal CNPJ, `BulkSources.brazilAsync` | `BR`, by CNPJ or its base | Parts of the real files and the code tables. Situação and special situations, head office or branch, **administrators as officers, partners as owners**, with gaps for their shares and for Socios parts not passed. | Owner shares | Unverified: dados.gov.br couldn't be read |
| Registro de Empresas y Sociedades, `BulkSources.chileAsync` | `CL`, by RUT | The real yearly CSVs. Simplified-regime formations only, so `Registered` with no word and absence `NotAvailable`; an act other than a formation is `Unknown`. | Officers, owners, concerns, status | CC BY |
| State Register of Legal Units, `BulkSources.moldovaAsync` | `MD`, by IDNO (check digit) | The real company.xlsx of 2026-11-02, 304,013 rows in 6 s. No status word: a liquidation date is `Dissolved`, an insolvency administrator `InInsolvency`, a liquidator `WindingUp`. Leaders with roles; **founders with percentages**, not said to be people or companies, so a gap. Enterprises only, so absence is `NotAvailable`. | Concerns beyond insolvency | None stated; the site terms allow reuse under the law, source named |
| APR, `BulkSources.serbiaAsync` | `RS`, by matični broj | The real 57.7 MB answer of 2026-10-31, 133,793 companies. 4 status words; foreign branches are establishments; municipality only. Deleted companies are left out, so absence is `NotAvailable`. | Officers, owners | Serbian Open Data Licence, per data.gov.rs |
| e-Registar Brčko, `BulkSources.bosniaBrckoAsync` | `BA-BRC`, by MBS or JIB | The real export, 6,690 subjects. 4 status words. Current subjects only. | Officers, owners, concerns | None stated; the footer says all rights reserved |
| IGJ, `BulkSources.argentinaCabaAsync` | `AR-C`, by IGJ number or CUIT | The real half-yearly zip, 1,076,718 entities in 12 s. Struck off is `Closed` with its reason; authorities as officers; **partners as owners** without shares. 10,927 repeated CUITs are `NotAvailable`. | Concerns | CC BY 4.0 |
| SII nómina, `BulkSources.chileSiiAsync` | `CL`, by RUT (check digit) | The real zips of 2026-08-03, 3.4 million RUTs in 15 s and about 1.6 GB. An end date is `Closed`, else `Registered`; a line with fewer or more fields than its header is an unread line. **Fetched by hand:** `BulkDownloads` doesn't download it; with SII's express approval, pass the zips to `BulkSources.chileSiiAsync`. Tax registration, not a company register. | Officers, owners, concerns | None stated; site terms: personal non-commercial use, reproduction needs SII's approval |
| SUNAT padrón reducido, `BulkSources.peruAsync` | `PE`, by company RUC (20…) | The real file of 2026-11-07, 2.4 million companies. 13 status words; active is `Registered` as a tax registration, suspended or pending `Unknown`; NO HABIDO is an undeliverable address. Ended RUCs are included, so absence is not held. Tax registration. | Officers, owners | None stated |
| Supercias directorio, `BulkSources.ecuadorAsync` | `EC`, by RUC or expediente | The real xlsx of 2026-11-06, 229,616 rows. 15 legal situations; legal representative; foreign branches with their country. Cancelled companies left out, so absence is `NotAvailable`. | Owners, concerns | None stated; on datosabiertos.gob.ec with no licence |
| FNS SME register, `BulkSources.russiaSmeAsync` | `RU`, by OGRN or INN | **Not run on the full file** (2.25 GB): the XSD and its first entries. Micro, small or medium enterprise; region only. **Small and medium businesses only**, so absence proves nothing. | Officers, owners, concerns | FNS open data terms, attribution required |

### Coverage gaps

No code calls Delaware's public search, the German portal or any register whose terms forbid automated use. `CoverageRoutes` declares them, and the routes aren't contract-checked:

| Register | Gap | Route |
|---|---|---|
| Delaware | `NeedsPaidContract` | A paid vendor or registered agent's report |
| Germany | `NeedsEntitlement` | Approval from the court at Hagen, or a paid vendor |
| Spain, Italy, Luxembourg | `NeedsPaidContract` | A paid extract from the register's own channel, or a paid vendor |

[docs/candidate-sources.md](https://github.com/Thorium/CompanyDueDiligence/blob/HEAD/docs/candidate-sources.md) lists, for every country, the registers with no free automated route and why.

OpenCorporates is an option as a paid API only. `StandingWords.delaware` places `Good Standing`, `AR delinquent` and `Void` for a host with such a source, and any other word is `Unknown`. Utah, Missouri and any other
jurisdiction with no route are `NoRoute`.

## Person search

"Is this person a director, officer or owner of a company?" A register that publishes people and lets them be searched also implements `IPersonSearch`; one that doesn't, doesn't. `Registries.personSearches` picks them out of a list.

- `SearchPeopleAsync(name, jurisdiction, ct)` answers `Evidence<Availability<PersonSearchResult>>`: `Matches`, `TotalMatches` (records the register's own search found, in its own unit) and `Complete`. `PersonSearch` is a `Support` saying how far it reaches.
- A `PersonMatch` holds the name and role as published, `Current` only when the register says so, the company's name, number and status on that register, and `AppointedOn` and `EndedOn` where given. It says the register holds the name, not that it is the person meant.
- A search never says "not a director". `Complete` is true only when the register says it returned every record its search found, so an empty list is no clear.
- The name is `Firstname Lastname` or `Lastname, Firstname` (`PersonName.tryParse`). The register's own search goes first, as contains or full text as each supports; then `PersonName.matches` keeps a role only when every word asked, after `NameMatch.normalise`, is a whole word of the published name, in any order. A masked name, holding an asterisk, never matches, and Poland's KRS, which masks every name, has no person search.
- A name with no letter or digit is `InvalidRequest`, and a jurisdiction outside coverage is `OutsideCoverage`; neither sends anything.

| Register | Searches | Calls | Complete | Run live |
|---|---|---|---|---|
| France, `Sources.france` | `/search?nom_personne=&prenoms_personne=`: officers who are people, among the 25 companies ranked first | 1 | Never: non-diffusible companies are left out | 2026-11-08 |
| Connecticut, `Sources.connecticut` | Principals (ka36-64k6) by `name__c`, each word contained; businesses joined from n7gp-d28j by `business_id` | 1 or 2 | Under 100 rows | 2026-11-08 |
| New York, `Sources.newYork` | `chairman_name`, the CEO, on the active corporations list (n9v6-gdp6) | 1 | Never: active corporations only | 2026-11-08 |
| District of Columbia, `Sources.districtOfColumbia` | Beneficial owners (layer 2) by `UPPER(NAME) LIKE`; the layer carries the business name, file number and status | 1 | Under 100 and not cut short | 2026-11-08 |
| Pennsylvania, `Sources.pennsylvania` | Governors and officers in xvd7-5r2c, each word in a name part | 1 | Never: active registrations only | 2026-11-08 |
| Latvia, `Sources.latvia` | CKAN full text on the officers and beneficial owners resources; companies joined from the register | 2 or 3 | Both totals read | 2026-11-08 |
| Companies House, `CompaniesHouseRegistry` | `/search/officers?q=`, then the first 50 appointments of up to 20 officers named as asked | 1 to 21 | Every officer and appointment read | 2026-11-08 |

A `PersonMatch` carries no date of birth, so two people of one name aren't told apart; the raw answer keeps what the register gave. The other adapters don't implement `IPersonSearch`.

## Sanctions

The host downloads the lists and passes their text with its date, as `ListText`, so the library fetches and stores nothing. Treasury's and the UN's URLs answer with redirects to signed URLs on other hosts, so a downloader has to follow those.

| List | URL | Read |
|---|---|---|
| OFAC SDN | `treasury.gov/ofac/downloads/sdn.csv`, `alt.csv`, `add.csv` | 19,555 entries, 39,769 names with aliases |
| OFAC consolidated | `.../consolidated/cons_prim.csv`, with `cons_alt.csv` and `cons_add.csv` if supplied | 486 entries, without those two |
| UK | `sanctionslist.fcdo.gov.uk/docs/UK-Sanctions-List.csv` | 6,420 entries, 15,758 names; about 50 MB; states its report date |
| UN | `scsanctions.un.org/resources/xml/en/consolidated.xml` | 1,007 entries, 3,760 names; states its date |
| EU | | Stated as not used: the file URL answered 403 without a token on 2026-11-06 |

The EU's open data catalogue lists file URLs that carry a token in the query. They weren't called, and no EU parser is here: whether to use them is the host's decision.

- A result is `PossibleMatch`, for a person to review, with `ScoreReason`, and `BirthYear` and `Country` compared when both sides have them. The output has no "not sanctioned".
- Every `Screening` states `ListsRead` with their dates, `ListsNotUsed` with what would close them, `NamesScreened` and `NamesNotScreened`, such as owners nobody has identified. So a clean name result leaves those gaps open.
- A file with the wrong shape is unreadable as a whole, because part of a list must not pass for all of it: a row with the wrong column count, a missing header column, a quoted field left open, no entries, an OFAC file without its closing Ctrl-Z, or fewer entries than `ListMinimums`.
- `ListMinimums.defaults` is 80% of each list's entries above, set through `InvestigationOptions.ListMinimums` or `SanctionsScreening.load`. Lower it if a list really shrinks that far. The UK and UN files have no count to check against, and the UK's no end marker, so a UK file cut off but still above its minimum can't be told from a whole one.
- Blank alias or address text counts as not supplied, and the list's notes say so.
- A file that states another date than the one it was passed with is a gap, and the export prints both.
- `Investigation.screen` screens every name held for an entity: the register's, the name given before the register answered, the name an owner's record states, `Company.OtherNames` such as former names, and LEI names. A party with no name, a failed read of officers, owners, an LEI or a parent, and a name with nothing to compare appear in `NamesNotScreened`. A name or entity added after a screening is a gap until the next one.
- Matching: upper case, accents off, Cyrillic and Greek letters spelled in Latin, full stops gone, legal forms dropped (the joint-stock forms `PJSC`, `OJSC`, `CJSC`, `AO` and `NAO` among them), words sorted. `AO` and `NAO` are also names of people (Ao, Nao), so a name holding one is compared both without it and with it as a word, and the higher score counts. The same words score 1. Courtesy titles opening or closing a name go. Every word of a shorter name inside a longer one scores 0.9, if one is distinctive. A screened name of one distinctive word inside a listed name scores 0.85, unless the word is under three letters, is a plain business word such as `SOLUTIONS`, or is in more than `CommonWordEntries` (75) listed entries: `ALI` is in 540 and `GAZPROM` in 65. Such a name is compared only as a whole, and is named in `NamesNotScreened`. A one-word name below that limit can still raise dozens of entries. Under the same three limits, a listed name of one word inside a longer screened name scores 0.85 where Jaro-Winkler on the whole names doesn't raise it, and so do two names whose one distinctive word is the same with other generic words beside it, such as `VTB Bank` and `VTB`. Otherwise Jaro-Winkler on
  the distinctive words, at 0.92, or 0.96 for one word each of six letters or more. A person isn't compared with a listed company, nor a company with a listed person; a party whose kind isn't known is compared with both. Those thresholds were tried on 14 names against the live lists, which took 1.6 seconds and
  raised 55 matches, 52 of them for three common personal names. Recall hasn't been measured, and neither have the matches the one-word listed name and same-word rules add: a listed vessel or alias of one word now meets every screened name holding it.
- Scripts: Cyrillic is spelled by BGN/PCGN for Russian without its rules of position, so `Е` is `E` everywhere, `Ё` is `E`, and the hard and soft signs are dropped; the letters only Ukrainian has follow BGN/PCGN for Ukrainian (`Є` `YE`, `Ї` `YI`, `І` `I`, `Ґ` `G`). A letter the languages read differently is read as Russian: `Г` is `G` and `И` is `I`, where Ukrainian has `H` and `Y`, and `Щ` is `SHCH` and `Ъ` dropped, where Bulgarian has `SHT` and `A`. Greek is spelled by ELOT 743, with `ΑΥ`, `ΕΥ` and `ΗΥ` as `AF`, `EF` and `IF` before a voiceless consonant or a word's end and `AV`, `EV` and `IV` elsewhere. A list's other spelling of the same name, such as `SERGEI` for `SERGEY`, is left to Jaro-Winkler. A name holding letters of any other script, such as Hebrew, Arabic or Chinese, is compared by its other words only and is named in `NamesNotScreened`, never in `NamesScreened`. Latin modifier letters (`Oʻzbekiston`), ligatures and full-width letters are Latin. The lists' own original-script names aren't read.

## Vendor checks

The host fills in a `VendorCheck` from its own client: `Vendor`, `Product`, `Reference`, `CheckedAt`, `OutcomeWords` unchanged, `Hits`, `Gaps`, `Facts` and `Raw`. This package references no vendor client, because ExperianClient pulls in SwaggerProvider.
`CaseFile.addVendorCheck` adds evidence, a flag per hit and a gap per thing not checked. It closes only the "no vendor's check yet" gap. A `Clear` closes nothing else.

Experian, from `KybOutcome` in `ExperianClient/Reports.fs`, typechecked against that client:

```fsharp
let private subject (who: KybSubject) =
    match who with
    | KybSubject.Company -> "the company"
    | KybSubject.Person -> "the person"
    | KybSubject.KeyParty party -> party.Name |> ValueOption.defaultValue "a key party"
    | KybSubject.Unlinked name -> name |> ValueOption.defaultValue "an unlinked party"

let private showAs (kind: KybHitKind) =
    match kind with
    | KybHitKind.Dissolved -> ValueSome FlagKind.StatusConcern
    | KybHitKind.AdverseMarker("MailboxAddress" | "CHDefaultAddress") -> ValueSome FlagKind.VirtualAddress
    | _ -> ValueNone

let ofKyb reference raw at (outcome: KybOutcome) : VendorCheck =
    let word, hits, gaps =
        match outcome with
        | KybOutcome.Clear -> "Clear", [], []
        | KybOutcome.NeedsReview(hits, gaps) -> "NeedsReview", hits, gaps
        | KybOutcome.NotScreened gaps -> "NotScreened", [], gaps

    {
        Vendor = "Experian"
        Product = "KYB"
        Reference = reference
        CheckedAt = at
        OutcomeWords = word
        Hits = hits |> List.map (fun hit -> { Subject = subject hit.Subject; Words = $"%A{hit.Kind}"; Detail = ValueNone; ShowAs = showAs hit.Kind })
        Gaps = gaps |> List.map (fun gap -> { Subject = "the check"; Words = $"%A{gap}"; WouldClose = ValueNone })
        Facts = []
        Raw = ValueSome raw
    }
```

Plaid, from `EntityScreeningOutcome` in `PlaidClient/Monitor.fs` (PlaidClient 1.3.0), typechecked against that client and run on answers shaped from Plaid's OpenAPI spec, `2020-09-14_1.799.0`. It hasn't run on a live answer.

Plaid's `cleared` is the customer's own decision state, not Plaid's finding: no list hit when screened, a reviewer on the customer's team dismissing a hit, or the customer's own API call. So `OutcomeWords` keeps the program and the audit trail, and a dismissed hit stays as a flag. Plaid's words are unchanged inside them.

```fsharp
open Plaid
open Plaid.Monitor

let private hitOf (legalName: string) (hit: EntityHit) : VendorHit =
    {
        Subject = legalName
        Words = $"{EntityListCode.toString hit.ListCode}: {HitStatus.toString hit.ReviewStatus}"
        Detail = hit.Names |> List.map _.Full |> function [] -> ValueNone | names -> ValueSome(String.concat "; " names)
        ShowAs =
            match hit.ReviewStatus, hit.ListCode with
            | HitStatus.Dismissed, _ -> ValueNone
            // These four aren't sanctions lists: state-owned enterprises, World Bank debarment, US federal exclusions and the US unverified list.
            | _, (EntityListCode.IzSoe | EntityListCode.IzWbk | EntityListCode.UsSam | EntityListCode.UsUvl | EntityListCode.Other _) -> ValueNone
            | _ -> ValueSome FlagKind.PossibleSanctionsMatch
    }

let ofMonitor (screeningId: string) at (screening: Result<EntityScreening, PlaidFailure>) (hits: Result<EntityHitList, PlaidFailure>) : VendorCheck =
    let legalName, status =
        match screening with
        | Ok read -> read.SearchTerms.LegalName, ScreeningStatus.toString read.Status
        | Error _ -> "the company", "not read"

    let words, found, gaps =
        match EntityScreeningOutcome.ofResults screening hits with
        | EntityScreeningOutcome.Clear clear ->
            let trail = clear.AuditTrail
            let who = trail.DashboardUserId |> ValueOption.map (fun user -> $" {user}") |> ValueOption.defaultValue ""
            $"{status}, program {clear.ProgramId}, last changed {trail.Timestamp:``yyyy-MM-dd``} by {AuditSource.toString trail.Source}{who}", clear.DismissedHits, []
        | EntityScreeningOutcome.NeedsReview(hits, gaps) -> $"NeedsReview, Plaid's status {status}", hits, gaps
        | EntityScreeningOutcome.NotScreened gaps -> $"NotScreened, Plaid's status {status}", [], gaps

    let raw =
        [
            match screening with
            | Ok read -> read.Raw
            | Error _ -> ()

            match hits with
            | Ok list ->
                for hit in list.Hits do
                    hit.Raw

                for unparsed in list.Unparsed do
                    unparsed.Raw
            | Error _ -> ()
        ]

    {
        Vendor = "Plaid"
        Product = "Monitor entity screening"
        Reference = screeningId
        CheckedAt = at
        OutcomeWords = words
        Hits = found |> List.map (hitOf legalName)
        Gaps = gaps |> List.map (fun gap -> { Subject = "the check"; Words = $"%A{gap}"; WouldClose = ValueNone })
        Facts = []
        Raw = if raw.IsEmpty then ValueNone else ValueSome(String.concat "\n" raw)
    }
```

A `Clear` covers only the lists in its program: `Monitor.getEntityProgramAsync` gives its `ListsEnabled`, and a UK check needs `UK_HMC` among them.

## The register contract

`ICompanyRegistry` is what each source implements, and an adapter of your own can join a `CaseSources`:

| Member | Answers |
|---|---|
| `Coverage` | `RegistryCoverage`: name, ISO 3166 jurisdictions, `Support` for standing, concerns, addresses, search, officers and owners, `Cost`, `RateLimit` and `Licence` |
| `TryFindAsync(number, jurisdiction, ct)` | `Evidence<Availability<Company voption>>` |
| `SearchAsync(name, jurisdiction, ct)` | `Evidence<Availability<SearchResult>>` |
| `OfficersAsync(number, jurisdiction, ct)` | `Evidence<Availability<Officer list>>` |
| `OwnersAsync(number, jurisdiction, purpose, ct)` | `Evidence<Availability<OwnerListing>>` |

Each returns `Task<Result<_, RegistryFailure>>`.

- `Evidence` holds `Source`, `RetrievedAt`, `AsOf`, `Raw` and `Value`. It wraps every answer, because "not found" and "not available" are evidence too.
- `Available ValueNone` from `TryFindAsync` means the register says it doesn't hold the number. `NotAvailable` means this source can't tell, as with France's left-out companies. A jurisdiction outside coverage is `RegistryFailure.OutsideCoverage`.
- `Available []` means the source says there are none. `NotAvailable reason` says nothing either way. `NeedsEntitlement` says who may read it and how to apply.
- `OwnerPurpose` is `CustomerDueDiligence` or `PublicRead`. A register that restricts owners answers `NeedsEntitlement` for the first and `NotAvailable` for the second.
- `Company.Standing` is legal standing on that register alone, and `CompanyStanding.isActive` is true for `Active` only. `Registered` holds a register's word, or none for a register with no status word. `WindingUp` is a dissolution that doesn't say whether it is insolvent. A status an adapter can't place is `Unknown`.
- `Company.Role` is `Home`, `ForeignRegistration`, `Establishment` or `NotStated`. `HomeRegistration` is what a record says of the home record, and `EstablishmentOf` the overseas company's number on that register.
- `Company.Concerns` holds the red flags apart from standing. `Stated` is what the register states. `Unconfirmed` is what its answer doesn't settle, which isn't clear.
- `OwnerListing.Gaps` says what keeps a listing from showing the whole of the direct ownership, and `OwnerListing.isComplete` is true when it is empty. `Statements` are classified by `Meaning`, with `AsPublished` kept. A listing can hold owners and still have gaps.
- `Owner.Entity` and `Officer.Entity` hold a legal entity's own registration.
- `Owner.IdentityVerification` and `Officer.IdentityVerification` report what the register publishes, with no status worked out from it. `ValueNone` says nothing about whether the person is verified. A statement due in the past with none supplied shows as a flag.
- A `CompanyMatch` has no standing: read the company before relying on a match. `SearchResult.Complete` is true only when `Matches` holds every match the register says there is.
- `RegistryFailure.RequestRefused` means the request or the access needs fixing, and `RegistryFailure.isTemporary` says whether the same call can work later as it stands.
- `Jurisdiction` holds a code such as `GB`, `GB-SCT` or `US-DE`. A register for a country answers for its subdivisions.

### Adding a source

An adapter implements `ICompanyRegistry`, and has to:

- declare `Coverage`, with `Support.NotOffered` for what the register doesn't publish, and its cost, pace and licence;
- answer `RegistryFailure.OutsideCoverage` for a jurisdiction `Coverage` doesn't list;
- give `TryFindAsync` `Available ValueNone` only when the register says the number isn't held, and `NotAvailable` when its silence can mean something else;
- answer `NotAvailable`, not an empty list, for data the register doesn't publish or can't confirm;
- map its statuses to `CompanyStanding`, with anything it doesn't recognise as `Unknown`;
- fill `Concerns`, with `Unconfirmed` for what the answer doesn't settle, and `CompanyConcerns.notPublished` if the register publishes none;
- classify its ownership statements, with anything it doesn't recognise as a gap, and list a gap for each current owner it can't identify;
- answer `RequestRefused` for what only fixing the request or access will change, and `Unavailable` only for what can work later;
- set `Source` to `Coverage.Registry`, `RetrievedAt` to when it answered and `Raw` to what it answered.

Code outside this repository implements `ICompanyRegistry`, and optionally `INameMatchSearch` and `IPersonSearch`. The rest applies to sources added inside it, since `BulkRegistry`, `BulkSourceShape` and `OpenData` are internal. A source behind a key takes a `SourceKey` or `ClientCredentials` and keeps it out of failures. A bulk source builds a `BulkRegistry` from the host's streams
and a `BulkFileDate`, and says by `BulkAbsence` whether its file lists every company. Its normaliser goes in `BulkSourceShape` under its catalogue id, so that an index of it can be opened. `OpenData` holds the Socrata, CKAN and ArcGIS queries several registers share.

Never scrape a register whose terms forbid it: declare a `CoverageRoute`. `RegistryTests.fs` holds a register for a made-up jurisdiction, and one set of contract checks that runs against it and Companies House.

## How Companies House fills the contract

It reads the [Companies House API](https://developer-specs.company-information.service.gov.uk/). It comes from the bundled specs and the enumerations, checked against live answers on 2026-11-08.

- **Standing** reads `company_status` and `company_status_detail` as the [enumerations](https://github.com/companieshouse/api-enumerations) spell them.
  Active with `active-proposal-to-strike-off` is `ActiveWithConcern`. Open and registered are `Registered`.
- **Concerns** read the profile's flags. Overdue accounts is `next_accounts.overdue` or the deprecated `accounts.overdue`. Annual filing is
  `confirmation_statement.overdue` or `annual_return.overdue`. Insolvency history is `has_insolvency_history`, `has_been_liquidated` or a `links.insolvency`; it is clear when
  `has_insolvency_history` is false, there is no link, and `has_been_liquidated` is false or missing. Any other flag the profile lacks is `Unconfirmed`, except annual filing on an
  `oversea-company`, which files no confirmation statement. A `uk-establishment`'s flags stay `Unconfirmed` on its record, as they are its overseas
  company's; a case reads them there and adds no gap for them on the establishment. A flag that isn't a boolean fails the answer.
- **Role** reads `type`: `oversea-company` and `registered-overseas-entity` are `ForeignRegistration`, `uk-establishment` is `Establishment`, and any other is `Home`. `HomeRegistration` reads
  `foreign_company_details`, with `external_registration_number` for a number it lacks. `EstablishmentOf` reads `branch_company_details.parent_company_number`.
- **Filings** read filing history. `description_values.capital` gives a capital figure as `Issued`, and one that isn't a number stays in the raw answer. A document is read as `application/pdf`.
- **Officers** are a `LegalEntity` with a `corporate-` role or an `identification` block, and an `Individual` with a date of birth or a role the specification lists a `corporate-` form beside, such as `director`. Any other role, such as a partner in a limited partnership, is held by either and is `Unknown`, which a screening compares with listed entities and individuals alike.
- **Partial records** are read from the profile and the filing list, never as whole. `super_secure_managing_officer_count` adds that many withheld officers to the list, each a gap in a case. `partial_data_available` says Companies House isn't the primary source: an empty officer list is `NotAvailable`, a list with officers ends with one not given, an owner listing carries a gap, and a filing history isn't `Complete`. A `filing_history_status` other than `filing-history-available` isn't `Complete` either, and a case records filings the register doesn't hold. The officer and owner gaps name the marker; `FilingHistory` has no field for one.
- **Jurisdiction** is `GB`, or a subdivision for `england`, `wales`, `scotland` and `northern-ireland`. `england-wales` has no ISO 3166 code
  of its own and stays `GB`, as do foreign companies on the register; `JurisdictionAsPublished` keeps the wording.
- **Owners** are PSCs and their statements, two lists. No current owner is only `Available` when a current statement says there is no one to list,
  in the two codes that say so. Anything else is `NotAvailable`, naming the statements: exempt companies, unidentified owners and only
  former owners. With a current owner it is `Available`, and `Gaps` holds what the statements leave open and any current owner who is withheld, can't be read, or is
  a legal entity with no registration number. Statements are classified from
  [`psc_descriptions.yml`](https://github.com/companieshouse/api-enumerations/blob/HEAD/psc_descriptions.yml), and a code not listed there is a gap.
- **Search** returns one page of up to 50, with `TotalMatches` and `Complete`. A 404 is `NotAvailable`.
- **Failures**: 401 is `AccessRefused`. Any other 4xx and a redirect are `RequestRefused`. 429 is `RateLimited`. 5xx, no answer, and a list that came back incomplete or
  inconsistent are `Unavailable`. An answer that can't be read, or a list past the page cap, is `UnreadableAnswer`.
- A number that can't be one, including one that has lost its leading zeros, is `InvalidRequest` and isn't sent.

### How its client reads

- **Pacing.** Calls carrying the key keep to the guide's 600 in any five minutes, then wait for the oldest to leave the window. An answer saying none are left
  holds the client until its reset, cut to the window. A 429 holds it for what the answer says, or the guide's period, or at least a second, and is asked again
  twice, so a list doesn't lose the pages read; a wait past five minutes isn't made and the call is `RateLimited`. The limit belongs to the key, so make one
  registry per key and keep it. Waits don't count against the 20-second request limit. A signed document URL isn't paced.
- **Paging.** A list is read from `start_index` 0, 100 to a page, moving on by the items received. An answer must state a readable total. A list ends only on an
  empty answer or a 404 at its total, one more call that brings an understated total to light. Part of a list is never read as all of it: ending below the
  total, a page seen before, a total or counts that change between pages, an item that moves to a later page, more items than the total, or more than 100 calls
  fail the read. Officers, PSCs and PSC statements must also state active and ended counts that add up. A search reads up to 1000 items, and says whether that was all.
- **Documents.** A filing is fetched from the Document API with the key, which answers with a redirect to a signed URL; that URL is read without the key, follows
  no further redirect, and is refused when weaker than https. A document id comes from the filing link's path; the link's host isn't used.
- **Not found.** A company profile is not held only on Companies House's own not-found error. A 404 on a list or sub-resource says nothing is held at that address.

## What to check live

### Companies House

Run live on 2026-11-08 on Tesco PLC, BP P.L.C., companies in liquidation, administration and dissolved, overseas companies (Norway, California, New York),
an overseas entity, a UK establishment, an LLP and an unknown number, and on the sandbox with companies from its test data generator. A Developer Hub
test key reaches only the sandbox; a live key answers 401 there. What the answers showed:

- The rate limit headers: `X-Ratelimit-Limit` (600), `-Remain`, `-Reset` in Unix seconds, and `-Window` (`5m`).
- At `start_index` equal to its total, the officer list answers no items and a total of 0, and the PSC list no items and its usual counts. Both end the list.
- For a number it doesn't hold, a profile answers 404 with a `message` holding `Resource not found for company profile`, and the officer list, PSC list and
  filing history answer 200 with empty lists. An officer list, owner listing and filing history are each read against the profile, unless the registry has read it.
- Officer and PSC counts add up to the total and match the items; the officer list carries `inactive_count`.
- `has_been_liquidated` is missing from most profiles made since about 2015, every LLP and recent overseas company in a sample of 54.
  `has_insolvency_history` was on every one, so with no `links.insolvency` it settles insolvency history. `registered_office_is_in_dispute` was missing on 2.
- Overseas companies carry no confirmation statement, so annual filing doesn't apply to them. A UK establishment's profile carries no filing flags: they are its overseas company's.
- Filing history pages hold at most 100. BP had 15,553 filings and Tesco 8,372; `FilingsAsync` reads the newest 1,000, and its `FilingHistory` says how many there are and that it stopped; a case records that as a gap. Capital filings (allotments, buy-backs) aren't highlighted: they were 824 of Tesco's newest 1,000.
- Search snaps `start_index` down to a multiple of `items_per_page`; the company lists don't. A 416 at `start_index` 1000 on search.
- `uk-establishments` answers 200 with an empty list for a company with none and states no total; for an unknown number it answers 404.
- `foreign_company_details` holds `legal_form`, `governed_by` and `originating_registry` with `country` and sometimes `name`, such as "Secretary Of State,
  State Of California". `registration_number` can be "N/A", which is no number.
- A corporate PSC's `place_registered` and `country_registered` hold places. Address parts can be empty strings; they are left out.
- An insolvency record can hold `"cases": []` with a `status` such as `liquidation`.
- A document's content answers 302 to a signed S3 URL on `s3.eu-west-2.amazonaws.com`.
- Officers' `identity_verification_details` are present and read.

Still unverified:

- Disqualified officer ids not linking to officer ids, which is why a search goes by name (forum posts).
- Whether `search.json`'s `total_results`, described as further results, is the whole total. `Complete` assumes it is.
- The classification of change-reporting beneficial owner statements as `ChangesOnly`, which is no gap.
- The shape of charges answers.
- `description_values.capital` on a filing, which the bundled spec doesn't describe.
- `partial_data_available`, `super_secure_managing_officer_count` and a `filing_history_status` other than `filing-history-available`: read as the bundled specs describe them, and not met live.
- An officer under a role with no `corporate-` form, such as a limited partnership's partner, with or without an `identification` block: not met live.

Choices made without data: the cap of 100 calls a list, the 1,000 filings read, the two retries and longest wait of five minutes after a 429, and the 32 MB an answer and 256 MB a document may hold before the read fails.

### The other sources

Each row of the [source tables](#sources) says what was run live, what was written from a spec, and which status words or fields are unverified. A source
behind a key other than Companies House hasn't been run, and its first live answers decide whether its mapping holds. Earlier notes on the first sources:

- **Malta:** the licence, and a beneficial owner route. A rate limit: none is published and none was met. An `Overseas` company's record holds no pointer to its home record, so it reads as a gap.
- **France:** the data licence. A company under partial diffusion wasn't met live: the rule that it hides the address comes from the spec. Category `31xx` as a foreign company comes from INSEE's table. The beneficial owner route.
- **Finland:** the open data holds ended Trade Register companies (0357299-5, `tradeRegisterStatus` 4, on 2026-11-08) and leaves out sole traders and entities not on the Trade Register (1952100-9 answers nothing), so an empty answer reads as `NotAvailable`. Forms `19` and `7` as foreign, and a company in a running situation, weren't met live. The beneficial owner route.
- **Texas:** the rate limit, and any terms beyond the portal's. The two rows on one file number weren't told apart. `F` and the other status words are from the layout attachment, not from the Secretary of State.
- **GLEIF:** the 60 a minute, which couldn't be confirmed in its docs. The licence wasn't re-read. No API key is needed.
- **Romania:** whether ONRC's two forms of a registration number map one to one, as year, sequence, county and a check digit; until the file or ONRC's layout shows it, a number in the form the file lacks is `NotAvailable`.
- **Switzerland:** "i.L." as liquidation wording wasn't seen, and isn't read.
- **Sanctions:** how the Cyrillic and Greek spellings compare with the lists' own, and how many matches the one-word listed name and same-word rules add: neither was run on the lists.
- **Sanctions:** whether OFAC's `cons_alt.csv` and `cons_add.csv` have the shapes of `alt.csv` and `add.csv`: they weren't downloaded. Dates of birth in OFAC's remarks can run past the file's field limit into `sdn_comments.csv`, which isn't read.
- **Vendors:** neither mapping has run on a live answer. Experian's is typechecked. Plaid's is typechecked and run on answers shaped from its OpenAPI spec.
- **Routes:** `CoverageRoutes` are research notes, and Delaware's three status words haven't been seen from a paid source.
- **Name match queries:** the `INameMatchSearch` patterns were run live on 2026-11-08 against Colorado, Connecticut, New York, Texas, DC, Oregon, Pennsylvania and Colombia. New York's Exact search timed out once at 30 s and answered on a retry.

## Secrets and redirects

The sources other than Companies House keep no cookies and don't follow redirects: a 3xx is `RequestRefused`. `BulkDownloads` sends no key, keeps no cookies and follows up to five redirects; a file a listing names is fetched only over https from the listing's own host, or one its resolver names. A source behind a key takes it as a `SourceKey` or
`ClientCredentials`, and printing either shows nothing. Each key goes only to its own source's host, in the place its docs name. A key that travels in a
query string (ABN Lookup, NTS, NTA) is taken out of failures, and out of `Raw` where the answer could echo it. Denmark's CVR has only a plain http endpoint,
so its credentials travel unencrypted: weigh that before using it. A 401 is `AccessRefused`, and a source behind a token asks for a new one once first. A handler passed to `SourceConnection` that follows redirects raises `ArgumentException`, except in a browser, where only keyless sources run.

`Sources.companiesHouse` sends its key only to Companies House's API and Document API hosts, over https, through the connection's handler, which doesn't follow redirects. Each HTTP request is limited to 20 seconds.

Companies House asks that keys stay out of source code. Getting one means registering an application of type "API Key" on the
[Developer Hub](https://developer.company-information.service.gov.uk/).

## Versioning

From 1.0.0 the package follows semantic versioning, with these rules for what a minor version may add:
- **Union cases.** Cases may be added to any union except `Availability`, `GapReach`, `LeiParentLevel`, `DiscrepancyLabel` and `NameMatch`. Match the others with a wildcard, or use their helpers (`RegistryFailure.describe`, `isTemporary`, `CompanyStanding.isActive`).
- **Record fields.** Fields may be added to records. Build the records a host or adapter fills from `Company.create`, `RegistryCoverage.create`, `Evidence.create`, `CaseSources.ofRegistries`, `CaseStart.create`, `VendorCheck.create` and `InvestigationOptions.defaults` with `{ X with ... }`, so a new field doesn't break the source. Code compiled against an older version needs rebuilding.
- **Interfaces.** `ICompanyRegistry`, `INameMatchSearch`, `IPersonSearch`, `IFilingRegister`, `IDocumentReader`, `ILeiCrossCheck` and `IBulkRegistry` don't gain members: a new capability is a new interface a source opts into.
- **Stored cases.** A case's JSON uses the type and case names. A change to them raises `CaseFile.CurrentSchemaVersion`, and `CaseJson.ofJson` upgrades an older file step by step; a file from a newer version is refused.

## Left out

**PSC discrepancy reports.** The API needs OAuth 2.0, not the API key. Adding it would take an OAuth sign-in for a real user, the two scopes
in `Schemas/discrepancies/pscDiscrepancies.json`, and that user holding the PSC Discrepancy permission, which obliged entities request from Companies
House by email. Then three calls: create a report, add discrepancies, and `PUT` a status of `COMPLETE`, after which the report can't change.

**Streaming API.** It needs a separate key, from an application of type "Streaming API", on `stream.companieshouse.gov.uk`. Adding it would take a
long-lived request read line by line through an unbuffered client, ignoring blank keep-alive lines, storing each event's timepoint to reconnect from,
handling 416 when the timepoint is too old, and waiting a minute after a 429. An account has two connections, and a third closes the oldest.

## Specs

`Schemas/` holds the Companies House specs, unedited, with their sources in `Schemas/SOURCES.md`. Nothing is generated from them: they are split
across files with references to `127.0.0.1`, some of those files are missing, and their closed enums disagree. The client reads what it needs by hand and
tolerates new members, as the Developer guidelines ask.

## Browser demo

`samples/CompanyDueDiligence.Web` is a Bolero (F# on Blazor WebAssembly) page that runs this library in the browser.
- It opens cases against the 15 keyless registers whose servers allow calls from a web page.
- Its Companies page finds a company by name and a person by name across the registers switched on, at once. Results are grouped by register, five to a register until "Show all", with the registers that gave none on one line; a register picker beside each search box asks all of them, the default, or one. A person's match opens the company with "Look up". PEP status comes from a vendor's screening, not a register.
- Its name search has a Match select: Exact name, Starts with (the default) or Contains.
- Deep links: `#/company/<code>/<number>` (such as `#/company/FI/2990791-7`), `#/search/<term>` with `?match=exact` or `?match=contains`, and `#/person/<name>`, each with `?register=<code>` for one register, URL-encoded, open the page at that lookup or search, on load and on a hash change. Hash routes need no server rewrite, so they work under the GitHub Pages sub-path. A lookup or search run from the page puts its link in the address bar, and "Copy link" copies it; where the browser refuses the clipboard, the link is left in the address bar. A link to a register switched off in Settings is still looked up, with a note; an unknown code is an error.
- Companies House (`GB`) needs a JavaScript key set in Settings, with a "Get a key" link to the Developer Hub, kept in this browser and sent only to Companies House. With one, GB is listed first, in the picker, the switches, both searches and cases; a refused key reads "Companies House refused the key". Its JavaScript key route hasn't been run live; the API key route has.
- Its settings screen switches each register, and the GLEIF cross-check, on or off. The choice is kept in the browser. A case follows owners only into registers switched on; anything else becomes a gap.
- Its bulk sources screen lists the catalogue without downloading: building an index of millions of rows on a browser's one thread freezes the page for minutes.
- Registers behind a key, and bulk files, need a desktop or server host, where keys stay private and files are kept in a folder.

`.github/workflows/ci.yml` builds, runs both test projects on Linux and Windows, and packs, on every push to main and pull request. `.github/workflows/pages.yml` runs the library's tests, publishes the page and deploys it to GitHub Pages. It needs Pages set to "GitHub Actions" in the repository settings. To run it locally:

```bash
dotnet run --project samples/CompanyDueDiligence.Web --urls http://localhost:5180
```

### Local server mode

`samples/CompanyDueDiligence.Server` is an ASP.NET Core app that hosts the same page with a JSON API, so every register runs, not only those a browser may call:

```bash
dotnet run --project samples/CompanyDueDiligence.Server
```

Then open http://localhost:5190. On a computer shared with other accounts, which can all reach localhost, start it with `--token`: every API call but `api/mode` then needs the access token, and the address to open is the one it prints, http://localhost:5190/?token=…. The page keeps the token in the browser and takes it out of the address bar. The token is in `token` in the data folder and stays the same across runs. The page asks `api/mode` at start: the server answers it, GitHub Pages answers 404, and the page then stays the browser demo above.

- Cases, name searches and person searches run on the server, over every keyless source, each keyed source whose settings are given, and each bulk source whose index is built. The pickers, the switches, grouping, flags and deep links work as in the browser.
- Settings → Keys has a field per setting in `KeyedSources.settingNames`, with Save and Remove, and shows whether each is set, never its value. "Get a key" opens each issuer's page, with what getting the key takes. A register is listed once all its settings are given. An environment variable of the same name also counts; a saved key overrides it. Companies House takes a key of type API Key here.
- Settings → Bulk sources has Download for each source with fixed links or a resolver, with a progress bar, then Remove and the index size. A download builds the index; then that source's lookups and name search answer like any register's.
- A resolved source shows the listing it is found through. "Show the current files" reads the listing and lists the files with their names on the portal, dates and sizes, without downloading. After a download, the saved files are listed the same way. When the listing can't be read, the error says how to get the files by hand.
- "or use files I already have" opens, for a resolved source, the form a source fetched by hand has: the paths of its files on this computer, and "Build index from these files". Each path is a full path on this computer's own drives, not a network share or device path, however its separators are written. Once the index is built, those files replace any download; a failed build leaves the download and index there were.
- It listens on http://localhost:5190 only; `--port <n>` sets another port, still at localhost. It refuses a request whose Host header isn't `localhost` or `127.0.0.1`, against DNS rebinding, and a change sent from another site's page.
- Its data folder is `%LOCALAPPDATA%\CompanyDueDiligence` on Windows, and `$XDG_DATA_HOME/CompanyDueDiligence` or `~/.local/share/CompanyDueDiligence` elsewhere; `--data <dir>` sets another. It holds `keys.json`, `token` with `--token`, `protection/`, and `bulk/<id>/` with each source's files and index.
- `keys.json` holds each key protected with ASP.NET Core data protection. On Windows the protection keys in `protection/` are encrypted with DPAPI for your account. Elsewhere they are plain files, so the keys are only as private as the data folder, which is made readable by its owner alone.

The API: `GET api/mode`, `api/registers`, `api/lookup/{code}/{number}?codes=&gleif=`, `api/search?term=&match=&codes=` (`match` is `exact` or `contains`, else starts-with), `api/people?name=&codes=`, `api/keys` and `api/bulk`; `PUT` and `DELETE api/keys/{settingName}`; `GET api/bulk/{id}/current` (a resolved source's files as its listing names them now); `POST api/bulk/{id}/download`, `POST api/bulk/{id}/files` and `DELETE api/bulk/{id}`. Every answer is JSON. The Pages workflow publishes only the page.

## Tests

`dotnet test` runs offline, against a fake handler and local sockets. Scenario tests in `InvestigationTests.fs` use made-up names and numbers: a UK overseas record with a Delaware home record, a dissolved UK company and a same-named US company with forfeited branches, creditors named only in a filing, a virtual address with nominal capital and a past-due verification, and sanctions screening with unknown owners. The sanctions tests use made-up entries in the files' shapes.
Recorded answers are in `Fixtures`, with people's names and birth dates redacted.
`tests/CompanyDueDiligence.Server.Tests` runs the local server's API on an in-memory test host.

## Licences

The code is under the licence in `LICENSE`, except:
- the demo's flags (`samples/CompanyDueDiligence.Web/wwwroot/flags`), from flag-icons, MIT, with their licence beside them;
- the Companies House specs in `Schemas/`, Crown copyright, as `Schemas/SOURCES.md` says;
- the recorded answers in `tests/CompanyDueDiligence.Tests/Fixtures`, under their sources' licences, given in the source tables; OrgBook BC's is not an open licence.
