//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Withholdings' //- Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_DOC_RET
  as select from    I_JournalEntry
    left outer join zdt_fi_doc_ret             as Withholdings
            on Withholdings.accountingdocument            = I_JournalEntry.AccountingDocument
           and Withholdings.accountingdocumenttype        = I_JournalEntry.AccountingDocumentType

    inner join ZCDS_P_EMI_RET         as ElectronicDocuments
            on ElectronicDocuments.Companycode            = I_JournalEntry.CompanyCode
           and ElectronicDocuments.Accountingdocument     = I_JournalEntry.AccountingDocument
           and ElectronicDocuments.Accountingdocumenttype = I_JournalEntry.AccountingDocumentType
           and ElectronicDocuments.Documenttype           = '07'
           and ElectronicDocuments.Documentstatus        = 'PENDING'
    inner join    I_JournalEntryItem          as I_JournalEntryItem
            on I_JournalEntryItem.CompanyCode             = I_JournalEntry.CompanyCode
           and I_JournalEntryItem.FiscalYear              = I_JournalEntry.FiscalYear
           and I_JournalEntryItem.AccountingDocument      = I_JournalEntry.AccountingDocument
           and I_JournalEntryItem.Supplier                is not initial
           and I_JournalEntryItem.FinancialAccountType    = 'K'
           and I_JournalEntryItem.Ledger                  = '0L'
           and I_JournalEntryItem.IsReversal is initial    
           and I_JournalEntryItem.IsReversed is initial
    
    inner join    I_BusinessPartner            as I_BusinessPartner
            on I_BusinessPartner.BusinessPartner          = I_JournalEntryItem.Supplier
            
    inner join    I_BusinessUserVH             as I_BusinessUser
            on I_BusinessUser.UserID                      = I_JournalEntryItem.AccountingDocCreatedByUser
    inner join    I_CompanyCode                as I_Company
            on I_Company.CompanyCode                      = I_JournalEntry.CompanyCode
            
    inner join    I_AccountingDocumentTypeText as I_AccountingDocumentTypeText 
            on I_AccountingDocumentTypeText.AccountingDocumentType = I_JournalEntry.AccountingDocumentType
           and I_AccountingDocumentTypeText.Language               = $session.system_language
    left outer join ZSH_STATUS                 as I_Status                  
            on I_Status.value_low =   Withholdings.documentstatus
{

  key
        case
            when Withholdings.companycode is not initial
            then Withholdings.companycode
          else   I_JournalEntry.CompanyCode
          end                             as Companycode,

  key
        case
            when Withholdings.fiscalyear is not initial
            then Withholdings.fiscalyear
          else   I_JournalEntry.FiscalYear
          end                             as Fiscalyear,

  key   case
        when Withholdings.accountingdocument is not initial
        then Withholdings.accountingdocument
      else   I_JournalEntry.AccountingDocument
      end                                 as Accountingdocument,


  key   case
        when Withholdings.accountingdocumenttype is not initial
        then Withholdings.accountingdocumenttype
      else   I_JournalEntry.AccountingDocumentType
      end                                 as Accountingdocumenttype,


        case
           when Withholdings.supplier is not initial
           then Withholdings.supplier
         else   I_JournalEntryItem.Supplier
         end                              as Supplier,

        case
            when Withholdings.businessname is not initial
            then Withholdings.businessname
          else   I_BusinessPartner.BusinessPartnerFullName
          end                             as Businessname,
          
        Withholdings.typeid        as Typeid,
        Withholdings.idnumber      as Idnumber,

        case
            when Withholdings.establishment is not initial
            then Withholdings.establishment
          else   '000'
          end                             as Establishment,

        case
          when Withholdings.emissionpoint is not initial
          then Withholdings.emissionpoint
        else   '000'
        end                               as Emissionpoint,

        case
          when Withholdings.sequential is not initial
          then Withholdings.sequential
        else   '000000000'
        end                               as Sequential,

        Withholdings.accesskey        as Accesskey,

        case
            when Withholdings.documenttype is not initial
            then Withholdings.documenttype
//          else   ElectronicDocuments.documentsri
          else '07'
          end                             as Documenttype,

        Withholdings.issuedate        as Issuedate,
        
        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when Withholdings.documentstatus is not initial
                then Withholdings.documentstatus
                else 'PENDING'
          end                             as Documentstatus,


        case Withholdings.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                            as criticality,
        
        Withholdings.messagedocument   as Messagedocument,
        Withholdings.authorizationdate as Authorizationdate,
        Withholdings.xml               as Xml,
        Withholdings.mimetype          as Mimetype,
        Withholdings.filename          as Filename,
        Withholdings.documentsupplier  as Documentsupplier,

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
