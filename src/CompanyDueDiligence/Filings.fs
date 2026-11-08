namespace CompanyDueDiligence

open System
open System.Threading
open System.Threading.Tasks

/// What a statement of capital says the figure is. Authorised and issued capital are different things that forms and certificates can mix up.
[<RequireQualifiedAccess>]
type CapitalKind =
    | Authorised
    | Issued
    | PaidUp
    /// The source's own word, or none.
    | Unlabelled of word: string voption

/// A capital figure as a source states it.
type CapitalStatement =
    {
        Kind: CapitalKind
        /// How many shares, when stated.
        Shares: decimal voption
        /// The money figure, when stated.
        Amount: decimal voption
        Currency: string voption
    }

/// A filing on a register, to be read by a person. A filing can be scanned images only, so nothing here says what it contains.
type FiledDocument =
    {
        /// The id to read the document's content by. ValueNone when the register holds no image of it.
        DocumentId: string voption
        TransactionId: string
        /// The register's filing category, such as "insolvency" or "resolution".
        Category: string voption
        /// The form, such as "AM10".
        FormType: string voption
        /// The register's description key.
        Description: string voption
        FiledOn: DateOnly voption
        /// The page count, when the register gives it.
        Pages: int voption
        /// Where the document's record is, on the register.
        Link: string voption
        /// Capital figures the filing's metadata holds.
        Capital: CapitalStatement list
    }

/// One insolvency case as a register records it.
type InsolvencyProceeding =
    {
        /// The register's own word for the case type.
        Kind: string voption
        CaseNumber: string voption
        /// The register's date types with their dates.
        Dates: (string * DateOnly) list
        Practitioners: string list
    }

/// A document's content as read from a register.
type DocumentFile =
    { ContentType: string; Content: byte[] }

/// A company's filings, newest first, as far as the register's read went.
type FilingHistory =
    {
        Filings: FiledDocument list
        /// The count the register states, when it states one.
        Total: int voption
        /// True when every filing was read. False when the read stopped at the register's limit: the older ones weren't read.
        Complete: bool
    }

/// A register's filings beyond the common contract. Same rules as ICompanyRegistry: Available with no filings and Complete means the register
/// says there are none.
type IFilingRegister =
    /// What the register covers, as ICompanyRegistry.Coverage: its Registry is the name each answer gives as its Source.
    abstract Coverage: RegistryCoverage

    /// The company's filings, newest first, up to the register's read limit; Complete says whether that was all of them.
    abstract FilingsAsync:
        number: string * ct: CancellationToken -> Task<Result<Evidence<Availability<FilingHistory>>, RegistryFailure>>

    /// The insolvency cases on the company's record.
    abstract InsolvencyAsync:
        number: string * ct: CancellationToken ->
            Task<Result<Evidence<Availability<InsolvencyProceeding list>>, RegistryFailure>>

    /// The establishments, on this register, of an overseas company.
    abstract EstablishmentsAsync:
        number: string * ct: CancellationToken ->
            Task<Result<Evidence<Availability<CompanyMatch list>>, RegistryFailure>>

    /// A document's content. ValueNone when the register has no such document.
    abstract DocumentAsync:
        documentId: string * ct: CancellationToken -> Task<Result<DocumentFile voption, RegistryFailure>>
