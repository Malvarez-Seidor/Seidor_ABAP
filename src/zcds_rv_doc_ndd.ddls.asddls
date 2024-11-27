//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Debit Notes - Roow View Interface'


define root view entity ZCDS_RV_DOC_NDD
  as select from    I_BillingDocumentBasic
    left outer join zdt_sd_doc_ndd            as DebitNotes                on  DebitNotes.billingdocument        = I_BillingDocumentBasic.BillingDocument
    inner join      zdt_ec_001                as ElectronicDocuments       on  ElectronicDocuments.companycode   = I_BillingDocumentBasic.CompanyCode
                                                                          and  ElectronicDocuments.documenttype  = I_BillingDocumentBasic.BillingDocumentType
                                                                          and  ElectronicDocuments.documentsri   = '05'
                                                                          and  ElectronicDocuments.sequence      = '01' 
    inner join      I_JournalEntry            as I_JournalEntry            on  I_JournalEntry.FiscalYear         = I_BillingDocumentBasic.FiscalYear
                                                                          and  I_JournalEntry.CompanyCode        = I_BillingDocumentBasic.CompanyCode
                                                                          and  I_JournalEntry.AccountingDocument = I_BillingDocumentBasic.AccountingDocument
    inner join      I_BusinessPartner         as I_BusinessPartner         on  I_BusinessPartner.BusinessPartner = I_BillingDocumentBasic.SoldToParty
    inner join      I_BusinessUserVH          as I_BusinessUser            on  I_BusinessUser.UserID             = I_BillingDocumentBasic.CreatedByUser
    inner join      I_CompanyCode             as I_Company                 on  I_Company.CompanyCode             = I_BillingDocumentBasic.CompanyCode
    inner join      I_BillingDocumentTypeText as I_BillingDocumentTypeText on  I_BillingDocumentTypeText.BillingDocumentType = I_BillingDocumentBasic.BillingDocumentType
                                                                          and  I_BillingDocumentTypeText.Language            = $session.system_language
    inner join      I_SalesOrganizationText   as I_SalesOrganizationText   on  I_SalesOrganizationText.SalesOrganization     = I_BillingDocumentBasic.SalesOrganization
                                                                          and  I_SalesOrganizationText.Language              = $session.system_language
    inner join      I_AccountingDocumentTypeText as I_AccountingDocumentTypeText on I_AccountingDocumentTypeText.AccountingDocumentType = I_JournalEntry.AccountingDocumentType
                                                                          and  I_AccountingDocumentTypeText.Language              = $session.system_language
    left outer join ZSH_STATUS                 as I_Status                  
            on I_Status.value_low =   DebitNotes.documentstatus
{

  key
        case
            when DebitNotes.companycode is not initial
            then DebitNotes.companycode
          else   I_BillingDocumentBasic.CompanyCode
          end                             as Companycode,

  key
        case
            when DebitNotes.fiscalyear is not initial
            then DebitNotes.fiscalyear
          else   I_BillingDocumentBasic.FiscalYear
          end                             as Fiscalyear,

  key   case
        when DebitNotes.accountingdocument is not initial
        then DebitNotes.accountingdocument
      else   I_BillingDocumentBasic.AccountingDocument
      end                                 as Accountingdocument,


  key   case
        when DebitNotes.accountingdocumenttype is not initial
        then DebitNotes.accountingdocumenttype
      else   I_JournalEntry.AccountingDocumentType
      end                                 as Accountingdocumenttype,

  key   case
        when DebitNotes.billingdocument is not initial
        then DebitNotes.billingdocument
      else   I_BillingDocumentBasic.BillingDocument
      end                                 as Billingdocument,

        case
            when DebitNotes.billingdocumenttype is not initial
            then DebitNotes.billingdocumenttype
          else   I_BillingDocumentBasic.BillingDocumentType
          end                             as Billingdocumenttype,

        case
           when DebitNotes.soldtoparty is not initial
           then DebitNotes.soldtoparty
         else   I_BillingDocumentBasic.SoldToParty
         end                              as Soldtoparty,

        case
            when DebitNotes.businessname is not initial
            then DebitNotes.businessname
          else   I_BusinessPartner.BusinessPartnerFullName
          end                             as Businessname,
          
        DebitNotes.typeid        as Typeid,
        DebitNotes.idnumber      as Idnumber,

        case
            when DebitNotes.establishment is not initial
            then DebitNotes.establishment
          else   '000'
          end                             as Establishment,

        case
          when DebitNotes.emissionpoint is not initial
          then DebitNotes.emissionpoint
        else   '000'
        end                               as Emissionpoint,

        case
          when DebitNotes.sequential is not initial
          then DebitNotes.sequential
        else   '000000000'
        end                               as Sequential,

        DebitNotes.accesskey        as Accesskey,

        case
            when DebitNotes.documenttype is not initial
            then DebitNotes.documenttype
          else   ElectronicDocuments.documentsri
          end                             as Documenttype,

        DebitNotes.issuedate        as Issuedate,
        
        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when DebitNotes.documentstatus is not initial
                then DebitNotes.documentstatus
                else 'PENDING'
          end                             as Documentstatus,


        case DebitNotes.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                            as criticality,

        DebitNotes.messagedocument as Messagedocument,
        DebitNotes.authorizationdate as Authorizationdate,
        DebitNotes.xml              as Xml,
        DebitNotes.mimetype         as Mimetype,
        DebitNotes.filename         as Filename,
        DebitNotes.documentsupplier as Documentsupplier,

        I_BillingDocumentBasic.CreatedByUser,
        I_BillingDocumentBasic.CreationDate,
        I_BillingDocumentBasic.SalesOrganization,
        I_BillingDocumentBasic.BillingDocumentDate,
        I_BillingDocumentBasic.BillingDocumentIsCancelled,
        @Semantics.amount.currencyCode: 'TransactionCurrency'
        I_BillingDocumentBasic.TotalNetAmount,
        I_BillingDocumentBasic.TransactionCurrency,
        @Semantics.amount.currencyCode: 'TransactionCurrency'
        I_BillingDocumentBasic.TotalTaxAmount,
        I_BillingDocumentBasic.DocumentReferenceID,
        

        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Company.CompanyCodeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_BillingDocumentTypeText.BillingDocumentTypeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_BusinessUser.PersonFullName     as UserFullName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_SalesOrganizationText.SalesOrganizationName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_AccountingDocumentTypeText.AccountingDocumentTypeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Status.Description

}
