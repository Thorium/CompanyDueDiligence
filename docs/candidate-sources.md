# Coverage by country

Every UN member and observer state, plus the US states, was researched on 2026-11-08 for an official, free, machine-readable company register: an API or a bulk file. The adapters built from that research are in the README's [Sources](../README.md#sources). This file lists the rest:
- free sources not adapted, and why;
- jurisdictions with no free automated route.

Many "none found" lines rest on searches and secondary guides rather than a direct look at each register, so read them as likely, not proven. Confirm the terms on the publisher's own page before relying on any route.

## Free sources not adapted

| Jurisdiction | Source | Why not |
|---|---|---|
| LT | JAR on data.gov.lt | Its firewall refused every request from here. |
| JO | Companies Control on opendata.gov.jo | The datastore holds no rows; the full snapshot is from 2019. |
| KZ | gbd_ul on data.egov.kz | Needs a key from an egov.kz account (foreigners unverified); field names undocumented. |
| TH | DBD open API | Every DBD host blocked automated requests; no official documentation found. |
| DO | DGII RNC list | The file answered 403 and has no published layout. |
| IN | MCA master data on data.gov.in | Only third-party descriptions found. |
| BG | Commercial Register on data.egov.bg | The portal refused requests; the Registry Agency sells extracts. |
| BY | EGR API | Seen only on archived official pages; the live host timed out. |
| MN | GASR open data | No documentation; the host refused connections. |
| XK | ARBK open data | The export needs a key built by the site's JavaScript. |
| TJ | Tax Committee register | HTML only. |
| KR | FSC corporate basic info | No status field, and keyed by another number. |
| US-IA | Active Iowa Business Entities | Download only, with no documented file URL or query API. |
| US-VA | SCC datasets on data.virginia.gov | Stopped in November 2024, and two tables are cut at the Excel row limit. |
| US-IL | Statutory open data (805 ILCS 5/1.25) | The files couldn't be found; the SOS site refuses automated requests. |
| US-MN, US-KY | SOS bulk data | Free for non-commercial use only. |
| US-VT | SOS weekly bulk file | URL unverified; the state has proposed fees. |
| US-WA, US-MI, US-MO, US-AZ, US-MA, US-UT, US-WI | SOS search pages | Free, but their terms say nothing about automated use and no API is documented. Ask the state first. |
| US-RI, US-OH | New-filing exports | New filings only, so they can't answer a lookup. |
| CA-QC | REQ open data | CC BY-NC-SA: non-commercial only. |
| UY | RUPE supplier register | Not a company register. |
| AR | Registro Nacional de Sociedades; ARCA padrón | No status; tax registration only. |
| PK | FBR Active Taxpayer List | Tax filers only, and the file host timed out. |

Canada's federal open data CSVs and Japan's bulk files repeat what `Sources.canada` and `Sources.japan` read. Latvia's bulk files repeat `Sources.latvia`.

## No free automated route

### Europe

| Jurisdiction | Why |
|---|---|
| DE | Web only; terms forbid automation without approval from the court at Hagen. `CoverageRoutes.germany`. |
| ES | No register route; BORME searches by date only. `CoverageRoutes.spain`. |
| IT | The InfoCamere high-value dataset site has restrictive access criteria. `CoverageRoutes.italy`. |
| LU | The HVD API holds annual accounts only; the LBR API is paid. `CoverageRoutes.luxembourg`. |
| HU | The XML service is paid; the free view excludes bulk. |
| PT | Only aggregate statistics are open; the certidão is paid. |
| IS | Web search only. |
| LI, GG, IM, GI, MC, SM, AD, VA | Search page, paid, login or none. |
| JE | API limited to service providers for their own entities. |
| AL | Web only, with free PDF extracts. |
| MK | No datasets; web services appear paid. |
| ME | None found. |
| BA (Federation, Republika Srpska) | Web only. |
| GE | Captcha. |
| AM | Web only; extracts paid. |
| AZ | Web only; fuller data needs a login. |
| RU (EGRUL) | A free per-company PDF behind a captcha; bulk is paid. |

### United States

| Jurisdiction | Why |
|---|---|
| US-DE | Terms forbid automated tools. `CoverageRoutes.delaware`. |
| US-UT, US-MO, US-OH, US-TX (SOS), US-IL, US-IN, US-KY, US-MI, US-MN, US-WI, US-VA, US-NC, US-GA, US-SC, US-ME, US-MD, US-NE, US-WV, US-WY, US-AR, US-HI, US-LA, US-NJ, US-NV, US-ND, US-SD, US-MT, US-TN | Paid bulk or subscription. North Carolina's search also forbids scripts. |
| US-AL, US-ID, US-KS, US-MS, US-NH, US-NM, US-OK | None found. Only a catalogue sweep was made for some of these. |

### Americas

| Jurisdiction | Why |
|---|---|
| CA-ON, CA-AB, CA-NB, CA-MB, CA-SK, CA-NS | Paid or portal only. |
| MX, BO, GT, HN, SV, NI, PA | Web or paid only. |
| VE, CU, HT | None found. |
| PE (SUNARP), CO (full RUES), CR (Registro Nacional), UY (DGI) | Paid certificates or none. |
| JM, BB, BZ, GY, SR, TT, AG, DM, GD, KN, LC, VC, BS | Limited web search, paid extracts, or offline. |
| KY, VG, BM | Paid searches; Cayman's CORIS is for licensed providers only. |

### Asia and the Middle East

| Jurisdiction | Why |
|---|---|
| CN | Web only, with a slider captcha; bulk only through licensed partners. |
| HK | Paid per search. |
| VN | Web lookup; data products are paid. |
| ID | Web only; profiles paid; API under government agreements only. |
| MY | Paid profiles. |
| PH | Web only. |
| KH, LA, MM, BN, MV, NP, BT, TL | Web search only, or none. |
| KP, TM, AF, IQ, SY, YE | None found. |
| KG, UZ | Web search only. |
| IR, LB, PS | Web lookups only. |
| TR | Captcha and login. |
| SA | Paid; a short free trial only. |
| AE (federal, DIFC, ADGM), QA, BH, OM, KW | Web registers only. |
| EG | Web only. |
| PK (SECP), BD, LK | Web only or captcha. |

### Africa

| Jurisdiction | Why |
|---|---|
| ZA | Login-gated; disclosure certificates paid. |
| NG | Search behind a Cloudflare challenge; the API is paid and for selected partners. |
| KE (BRS) | Paid search. `Sources.kenya` reads tax PIN status only. |
| TN | Paid web services with a prepaid balance; the public search has a captcha. |
| MA | Paid. |
| GH, RW, ZM, BW, NA, TZ, LS, LR, MW, SZ, BJ, CI, SL, ET | Free web search, at best, with no API or bulk file. |
| MU, SC | Web or paid; Mauritius keeps directors and shareholders confidential. |
| DZ, AO, BF, BI, CV, CM, CF, TD, KM, CD, CG, DJ, GQ, ER, GA, GM, GN, GW, LY, MG, ML, MR, MZ, NE, ST, SN, SO, SS, SD, TG, ZW, UG | None found. The shared company register software of OHADA's member states has no public search or API. |

### Oceania

| Jurisdiction | Why |
|---|---|
| PG, FJ, SB, TO, WS, VU, PW | Web search only. Several run the same registry platform; none publishes an API. |
| KI, MH, FM, NR, TV | None found. |

## Cross-border

GLEIF is the only official cross-border source. The Open Ownership Register (an NGO) closed on 2024-11-29.
