//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Liquidation for Purchase' //- Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_DOC_LIQ
  as select from    I_JournalEntry
    left outer join zdt_fi_doc_liq             as LiquidationPurchase
            on LiquidationPurchase.accountingdocument     = I_JournalEntry.AccountingDocument
           and LiquidationPurchase.accountingdocumenttype = I_JournalEntry.AccountingDocumentType
    inner join    zdt_ec_001                   as ElectronicDocuments       
            on ElectronicDocuments.companycode           = I_JournalEntry.CompanyCode
           and ElectronicDocuments.documenttype          = I_JournalEntry.AccountingDocumentType
           and ElectronicDocuments.documentsri           = '03'
           and ElectronicDocuments.sequence              = '01'
      
    inner join    I_JournalEntryItem           as I_JournalEntryItem
            on I_JournalEntryItem.CompanyCode            = I_JournalEntry.CompanyCode
           and I_JournalEntryItem.FiscalYear             = I_JournalEntry.FiscalYear
           and I_JournalEntryItem.AccountingDocument     = I_JournalEntry.AccountingDocument
           and I_JournalEntryItem.Supplier               is not initial
           and I_JournalEntryItem.FinancialAccountType   = 'K'
           and I_JournalEntryItem.Ledger                 = '0L'
           and I_JournalEntryItem.IsReversal is initial    
           and I_JournalEntryItem.IsReversed is initial
    
    inner join    I_BusinessPartner            as I_BusinessPartner
            on I_BusinessPartner.BusinessPartner         = I_JournalEntryItem.Supplier
            
    inner join    I_BusinessUserVH             as I_BusinessUser
            on I_BusinessUser.UserID                     = I_JournalEntryItem.AccountingDocCreatedByUser
    inner join    I_CompanyCode                as I_Company
            on I_Company.CompanyCode                    = I_JournalEntry.CompanyCode
            
    inner join    I_AccountingDocumentTypeText as I_AccountingDocumentTypeText 
            on I_AccountingDocumentTypeText.AccountingDocumentType = I_JournalEntry.AccountingDocumentType
           and I_AccountingDocumentTypeText.Language               = $session.system_language
    left outer join ZSH_STATUS                 as I_Status                  
            on I_Status.value_low =   LiquidationPurchase.documentstatus
{

  key
        case
            when LiquidationPurchase.companycode is not initial
            then LiquidationPurchase.companycode
          else   I_JournalEntry.CompanyCode
          end                             as Companycode,

  key
        case
            when LiquidationPurchase.fiscalyear is not initial
            then LiquidationPurchase.fiscalyear
          else   I_JournalEntry.FiscalYear
          end                             as Fiscalyear,

  key   case
        when LiquidationPurchase.accountingdocument is not initial
        then LiquidationPurchase.accountingdocument
      else   I_JournalEntry.AccountingDocument
      end                                 as Accountingdocument,


  key   case
        when LiquidationPurchase.accountingdocumenttype is not initial
        then LiquidationPurchase.accountingdocumenttype
      else   I_JournalEntry.AccountingDocumentType
      end                                 as Accountingdocumenttype,


        case
           when LiquidationPurchase.supplier is not initial
           then LiquidationPurchase.supplier
         else   I_JournalEntryItem.Supplier
         end                              as Supplier,

        case
            when LiquidationPurchase.businessname is not initial
            then LiquidationPurchase.businessname
          else   I_BusinessPartner.BusinessPartnerFullName
          end                             as Businessname,
          
        LiquidationPurchase.typeid        as Typeid,
        LiquidationPurchase.idnumber      as Idnumber,

        case
            when LiquidationPurchase.establishment is not initial
            then LiquidationPurchase.establishment
          else   '000'
          end                             as Establishment,

        case
          when LiquidationPurchase.emissionpoint is not initial
          then LiquidationPurchase.emissionpoint
        else   '000'
        end                               as Emissionpoint,

        case
          when LiquidationPurchase.sequential is not initial
          then LiquidationPurchase.sequential
        else   '000000000'
        end                               as Sequential,

        LiquidationPurchase.accesskey        as Accesskey,

        case
            when LiquidationPurchase.documenttype is not initial
            then LiquidationPurchase.documenttype
          else   ElectronicDocuments.documentsri
          end                             as Documenttype,

        LiquidationPurchase.issuedate        as Issuedate,
        
        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when LiquidationPurchase.documentstatus is not initial
                then LiquidationPurchase.documentstatus
                else 'PENDING'
          end                             as Documentstatus,


        case LiquidationPurchase.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                            as criticality,
        
        LiquidationPurchase.messagedocument   as Messagedocument,
        LiquidationPurchase.authorizationdate as Authorizationdate,
        LiquidationPurchase.xml               as Xml,
        LiquidationPurchase.mimetype          as Mimetype,
        LiquidationPurchase.filename          as Filename,
        LiquidationPurchase.documentsupplier  as Documentsupplier,

        I_JournalEntry.AccountingDocCreatedByUser,
        I_JournalEntry.AccountingDocumentCreationDate,
        I_JournalEntry.ReverseDocument,
        I_JournalEntry.PostingDate,
        I_JournalEntry.AccountingDocumentHeaderText,
        I_JournalEntryItem.DocumentItemText,
        I_JournalEntry.NetAmountIsPosted,
        I_JournalEntry.CompanyCodeCurrency,
        I_JournalEntry.TaxBaseAmountIsNetAmount,
        I_JournalEntry.DocumentReferenceID,
        

        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Company.CompanyCodeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_BusinessUser.PersonFullName     as UserFullName,
        
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_AccountingDocumentTypeText.AccountingDocumentTypeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Status.Description
      
}
