//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Liquidation Supports' //'- Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_012
  as select from I_JournalEntry
    left outer join zdt_ec_012 as SupportAttachment   
            on SupportAttachment.companycode            = I_JournalEntry.CompanyCode
           and SupportAttachment.fiscalyear             = I_JournalEntry.FiscalYear
           and SupportAttachment.accountingdocument     = I_JournalEntry.AccountingDocument
           and SupportAttachment.accountingdocumenttype = I_JournalEntry.AccountingDocumentType
           
    inner join zdt_ec_001    as ElectronicDocuments 
            on ElectronicDocuments.companycode  = I_JournalEntry.CompanyCode
           and ElectronicDocuments.documenttype = I_JournalEntry.AccountingDocumentType
           and ElectronicDocuments.documentsri  = '03'
           and ElectronicDocuments.sequence     = '01'
           and ElectronicDocuments.refunds      is not initial 
           
    inner join I_JournalEntryItem as I_JournalEntryItem   
            on I_JournalEntryItem.CompanyCode            = I_JournalEntry.CompanyCode
           and I_JournalEntryItem.FiscalYear             = I_JournalEntry.FiscalYear
           and I_JournalEntryItem.AccountingDocument     = I_JournalEntry.AccountingDocument
           and I_JournalEntryItem.Supplier               is not initial
           and I_JournalEntryItem.FinancialAccountType   = 'K'
           and I_JournalEntryItem.Ledger                 = '0L'
           and I_JournalEntryItem.IsReversal is initial    
           and I_JournalEntryItem.IsReversed is initial
           
    inner join I_BusinessPartner  as I_BusinessPartner         
            on I_BusinessPartner.BusinessPartner = I_JournalEntryItem.Supplier
            
    inner join I_BusinessUserVH   as I_BusinessUser            
            on I_BusinessUser.UserID             = I_JournalEntryItem.AccountingDocCreatedByUser
    inner join I_CompanyCode as I_Company           
            on I_Company.CompanyCode             = I_JournalEntry.CompanyCode
                
    inner join I_AccountingDocumentTypeText as I_AccountingDocumentTypeText 
            on I_AccountingDocumentTypeText.AccountingDocumentType = I_JournalEntry.AccountingDocumentType
           and I_AccountingDocumentTypeText.Language               = $session.system_language
    composition [0..*] of ZCDS_RV_EC_013 as _SupportDetails
{
  
  @ObjectModel.text.association: '_SupportDetails'
  key
        case
            when SupportAttachment.companycode is not initial
            then SupportAttachment.companycode
          else   I_JournalEntry.CompanyCode
          end                        as Companycode,
  @ObjectModel.text.association: '_SupportDetails'
  key
        case
            when SupportAttachment.fiscalyear is not initial
            then SupportAttachment.fiscalyear
          else   I_JournalEntry.FiscalYear
          end                        as Fiscalyear,
  
  @ObjectModel.text.association: '_SupportDetails'
  key
        case
            when SupportAttachment.accountingdocument is not initial
            then SupportAttachment.accountingdocument
          else   I_JournalEntry.AccountingDocument
          end                        as Accountingdocument,
  
  @ObjectModel.text.association: '_SupportDetails'
  key
        case
            when SupportAttachment.accountingdocumenttype is not initial
            then SupportAttachment.accountingdocumenttype
          else   I_JournalEntry.AccountingDocumentType
          end                        as Accountingdocumenttype,
          
   case 
    when SupportAttachment.filestatus is not initial 
    then SupportAttachment.filestatus
    else 'PENDING'
    end  as Filestatus,

        SupportAttachment.attachment as Attachment,
        SupportAttachment.mimetype   as Mimetype,
        SupportAttachment.filename   as Filename,

  case SupportAttachment.filestatus
    when 'PENDING' then 2
    when 'COMPLETE' then 3
    else 2
    end  as Criticality,
  
  @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
  abs(I_JournalEntryItem.AmountInCompanyCodeCurrency) as AmountInCompanyCodeCurrency,
     
  I_JournalEntryItem.Supplier                    as Supplier,
  I_JournalEntry.CompanyCodeCurrency             as CompanyCodeCurrency,
  
  I_JournalEntryItem.AccountingDocCreatedByUser as AccountingDocCreatedByUser,
  
  @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
  @ObjectModel.text.element: [ 'BusinessPartnerFullName' ]
  I_BusinessPartner.BusinessPartnerFullName as BusinessPartnerFullName,
  
  @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
  @ObjectModel.text.element: [ 'UserFullName' ]
  I_BusinessUser.PersonFullName     as UserFullName,
        
  @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
  @ObjectModel.text.element: [ 'CompanyCodeName' ] 
  I_Company.CompanyCodeName as CompanyCodeName,
  
  @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
  @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
  I_AccountingDocumentTypeText.AccountingDocumentTypeName as AccountingDocumentTypeName,
  
  SupportAttachment.last_changed_by as Last_changed_by,
  
  _SupportDetails
  
}
