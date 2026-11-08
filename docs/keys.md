# Getting and setting free keys

Fourteen sources need a free key or a user and secret. This file says, for each:
- where to get it;
- what approval it takes;
- which setting it goes in.

The steps come from each issuer's own pages as read on 2026-11-08. Only the Companies House key has been used live, on 2026-11-08; see the README's [Sources](https://github.com/Thorium/CompanyDueDiligence/blob/HEAD/README.md#lookups-behind-a-free-key). Where a step is marked unverified, the issuer's page couldn't be read or didn't say.

## Setting them

Keep keys out of source code and out of the repository. Put them in environment variables or a secret store, and build the sources from them:

```fsharp
let connection = SourceConnection.Create()
let keyed = KeyedSources.fromEnvironment connection

for problem in keyed.Incomplete do
    log.LogWarning("Key settings incomplete: {Problem}", problem)

let registries = keyed.Registries @ [ Sources.norway connection; Sources.czechia connection ]
```

- `KeyedSources.fromSettings connection read` takes any `string -> string voption`, such as a lookup in your secret store.
- A source with none of its settings is listed in `NotConfigured`.
- A source with only half of a pair is listed in `Incomplete`, by setting name, never by value, and no registry is made for it.
- A blank value counts as not given.
- Printing a `SourceKey` or `ClientCredentials` shows nothing.

To build one source yourself, pass the key directly: `Sources.canada connection (SourceKey key)`.

## The keys

| Source | Setting(s) | Where to get it | Approval | Notes |
|---|---|---|---|---|
| Companies House (`GB`) | `CDD_COMPANIES_HOUSE_KEY` | Register on the [Developer Hub](https://developer.company-information.service.gov.uk/) and create an application of type "API Key". The browser demo takes a key of type JavaScript instead, restricted to the page's address. | None: issued at once. | 600 calls per 5 minutes per key. |
| Corporations Canada (`CA`) | `CDD_CANADA_KEY` | Sign up on the [ISED API Store](https://api.ised-isde.canada.ca/), then subscribe to the Corporations API's Public Plan. | Unverified. | Sent in the `user-key` header. 60 calls a minute. |
| NZBN (`NZ`) | `CDD_NEW_ZEALAND_KEY` | Sign in to the [business.govt.nz API portal](https://portal.api.business.govt.nz/) and subscribe to the NZBN API. | The portal says subscriptions may be reviewed. | Sent as `Ocp-Apim-Subscription-Key`. |
| Bolagsverket (`SE`) | `CDD_SWEDEN_CLIENT_ID`, `CDD_SWEDEN_CLIENT_SECRET` | Send the free "kundanmälan" (customer notification) form on [Bolagsverket's API page](https://bolagsverket.se/apierochoppnadata/hamtaforetagsinformation/vardefulladatamangder/apiforvardefulladatamangder.5513.html). | None stated; no contract. The client ID arrives by email and the secret by SMS. | OAuth2 client credentials, scope `vardefulla-datamangder:read`. 60 calls a minute per user. Whether the gateway takes the credentials in a Basic header, as sent here, is unverified. |
| NTA corporate number (`JP`) | `CDD_JAPAN_APPLICATION_ID` | Apply on the [Corporate Number Web-API page](https://www.houjin-bangou.nta.go.jp/webapi/). | The NTA issues the application ID in about 2 to 4 weeks. | Sent in the query string; this package keeps it out of failures. Every display of the data needs the NTA's attribution sentence, which is in `Coverage.Licence`. |
| Zefix (`CH`) | `CDD_ZEFIX_USER`, `CDD_ZEFIX_PASSWORD` | Email `zefix@bj.admin.ch` asking for PublicREST access. | By the Federal Office of the Commercial Register; cost not stated. | HTTP Basic. The keyless UID register (`Sources.switzerland`) needs nothing. |
| California SOS (`US-CA`) | `CDD_CALIFORNIA_KEY` | Create an account on calicodev.sos.ca.gov and subscribe to the Business Entity product. | **The portal says government agencies only**, and the product needs approval. | Sent as `Ocp-Apim-Subscription-Key`. |
| ABN Lookup (`AU`) | `CDD_AUSTRALIA_GUID` | Register for a GUID on the [ABR web services page](https://abr.business.gov.au/Tools/WebServices); it comes by email. | You must accept the Web Services Agreement. | Sent in the query string; kept out of failures and raw answers. ASIC's company dataset (`BulkSources.asicCompaniesAsync`) needs no key. |
| NTS business status (`KR`) | `CDD_KOREA_SERVICE_KEY` | Create an account on [data.go.kr](https://www.data.go.kr/) and apply to use dataset 15081808. | Usually automatic. Whether a foreign account needs Korean phone verification is unverified. | Sent in the query string. 1,000,000 calls a day. |
| Sudski registar (`HR`) | `CDD_CROATIA_CLIENT_ID`, `CDD_CROATIA_CLIENT_SECRET` | Register on [sudreg-data.gov.hr](https://sudreg-data.gov.hr/). | Unverified. | OAuth2 client credentials; a token lasts 6 hours (a third party's figure). |
| GEMI (`GR`) | `CDD_GREECE_KEY` | Fill in the form at [opendata.businessportal.gr/register](https://opendata.businessportal.gr/register). | GEMI approves each application. | Sent as `api_key`. About 8 calls a minute (a third party's figure). |
| Firmenbuch HVD (`AT`) | `CDD_AUSTRIA_KEY` | Apply under the IWG at [justizonline.gv.at](https://justizonline.gv.at/jop/web/iwg/register). | The Ministry of Justice approves each application. ID Austria may be needed (unverified). | Sent as `X-API-KEY`. |
| CVR (`DK`) | `CDD_DENMARK_USER`, `CDD_DENMARK_PASSWORD` | Apply for system-to-system access through [datacvr.virk.dk](https://datacvr.virk.dk/), or write to `cvrselvbetjening@erst.dk`. | By Erhvervsstyrelsen; cost unverified. | HTTP Basic over **plain http**, the only endpoint documented, so the credentials travel unencrypted. |
| KRA PIN checker (`KE`) | `CDD_KENYA_CONSUMER_KEY`, `CDD_KENYA_CONSUMER_SECRET` | Sign up on the [GavaConnect developer portal](https://developer.go.ke/) and create an app with the PIN checker. | KRA approves each app before it goes live. | Tax PIN status only, not the business register. |

## After getting a key

The first live answers decide whether each mapping holds, since none of these has been run. For each new key:
1. Look up two or three companies you know, one of them dissolved.
2. Compare what comes back with the register's own website.
3. Report a status word that reads as `Unknown`, so its mapping can be added.
