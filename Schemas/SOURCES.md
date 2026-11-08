# Bundled specifications

Swagger 2.0 files published by Companies House, unedited, downloaded on 2026-11-06. They are reference material: nothing is generated
from them, the library doesn't load them and they aren't in the package. The tests read `public-data/swagger.json` and
`document/swagger.json` to check that each call's path is one the spec lists.

Each base is `https://developer-specs.company-information.service.gov.uk`, the site's footer marks it Crown copyright, and these files
aren't covered by this repository's licence.

| File | Source path | SHA-256 |
|---|---|---|
| `public-data/swagger.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/swagger.json` | `f9f566b30896ebb568a236d2498487b27b9f71edcdeb39d738390f6acb72b3a0` |
| `public-data/companyProfile.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/companyProfile.json` | `9283fde8578115207cfeca747bb917d6060bff2b4ff7721307dc62f911ed11af` |
| `public-data/search.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/search.json` | `c59dc2f9abf08053444528d050668d3df27a7af070a310df0f30a81bdb3097a0` |
| `public-data/companyOfficerList.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/companyOfficerList.json` | `d5c5607205eb3d950e630b6322b4a5855f5eac27dbf01ed2139e8a1ed29394c8` |
| `public-data/psc.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/psc.json` | `a2ac315713c1041782b56c5fac1f70fefde356bbfa18786ab983b386b6c27f67` |
| `public-data/pscModels.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/pscModels.json` | `56b9f8ad379a969a07a50abd621dc74a62fa3e9cb1a8a09db5cc32bf62d20fb9` |
| `public-data/charges.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/charges.json` | `4cfb674c032fc04b7b56ff13699878818e4c5faf7283438ccce486e66d2270fa` |
| `public-data/insolvency.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/insolvency.json` | `f01ca2e69e68cced71cc349d472102da136cd4992cca1692a33e68d932d5748e` |
| `public-data/filingHistory.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/filingHistory.json` | `d6a5a5d169ba8982592140b78dfb4d902d9c8844e477a67461c8d4689a977bb9` |
| `public-data/officerAppointmentList.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/officerAppointmentList.json` | `63a3724e94a337debfbf16c212c05cbf0edaeb337d7e0ff9f5852b0a4f41389c` |
| `public-data/companyUKEstablishments.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/companyUKEstablishments.json` | `2ccc75fc10c2ddf2103752f3f6dee0f6cc58c8904b697ebf807df53ede9d36a1` |
| `public-data/disqualifications.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/disqualifications.json` | `c7ffabf31fb96530055361c03758c1472f078c927219894190a12582d5069752` |
| `document/swagger.json` | `/document.api.ch.gov.uk-specifications/swagger-2.0/spec/swagger.json` | `ee3ccf15d32c22ec12e02f92ffe43847927971fa95222940db877c7071800730` |
| `discrepancies/pscDiscrepancies.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/pscDiscrepancies.json` | `f4135c8c445df9ab55e9280eddbaaaf4b8b22ca9ca5d58e8d274fe1b8ef45a08` |
| `streaming/streaming.json` | `/api.ch.gov.uk-specifications/swagger-2.0/spec/streaming.json` | `a316ac907ea8eca577b59be9b8d66478acb7ed1cadf8525d399169c9484dbf9f` |

`public-data/swagger.json` refers to further parts under the same source path, such as `exemptions.json` and `companyRegisters.json`, which
aren't bundled because no typed call here uses them.

## Where the files need care

- Their references point at `http://127.0.0.1:10000/...`, and `errors.json`, `officerChanges.json` and `registeredOfficeAddress.json` answer 404.
  `officerChanges.json` is where an officer's `identity_verification_details` is defined, so the client reads it by the names in `pscModels.json`.
- `companyUKEstablishments.json` lists no paging parameters and no total.
- `search.json` describes `total_results` as the number of further results, and marks it required only on some searches.
- Definition names clash between parts, such as `address` in four of them.
- Closed enums disagree between parts: `company_status` has 12 values in `companyProfile.json` and 10 in `search.json`.
- `securityDefinitions` names an `api_key` header. The Authorisation guide's HTTP basic is what the client sends.
- `charges.json` types `classification`, `particulars` and `links` as lists, and `insolvency.json` a practitioner's `address`. The client reads
  `classification`, `particulars` and `address` as one object or a list.
- `psc.json` marks `register_view` as required and also says it defaults to false, so the client sends false.
- `schemes` lists http, and the Developer guidelines say the API is TLS only.
- `document/swagger.json` lists only 302 and 401 for content. Its text also names a 406 for an unsupported type.
