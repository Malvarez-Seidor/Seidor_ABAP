//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Credit Notes - Roow View Interface'

define root view entity ZCDS_RV_DOC_NDC
  as select from    I_BillingDocumentBasic
    left outer join zdt_sd_doc_ndc            as CreditNotes               on CreditNotes.billingdocument = I_BillingDocumentBasic.BillingDocument
    
    inner join      zdt_ec_001                as ElectronicDocuments       on  ElectronicDocuments.companycode   = I_BillingDocumentBasic.CompanyCode
                                                                          and  ElectronicDocuments.documenttype  = I_BillingDocumentBasic.BillingDocumentType
                                                                          and  ElectronicDocuments.documentsri   = '04'
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
    left outer join ZSH_STATUS                 as I_Status                 on  I_Status.value_low =   CreditNotes.documentstatus
{

  key
        case
            when CreditNotes.companycode is not initial
            then CreditNotes.companycode
          else   I_BillingDocumentBasic.CompanyCode
          end                             as Companycode,

  key
        case
            when CreditNotes.fiscalyear is not initial
            then CreditNotes.fiscalyear
          else   I_BillingDocumentBasic.FiscalYear
          end                             as Fiscalyear,

  key   case
        when CreditNotes.accountingdocument is not initial
        then CreditNotes.accountingdocument
      else   I_BillingDocumentBasic.AccountingDocument
      end                                 as Accountingdocument,


  key   case
        when CreditNotes.accountingdocumenttype is not initial
        then CreditNotes.accountingdocumenttype
      else   I_JournalEntry.AccountingDocumentType
      end                                 as Accountingdocumenttype,

  key   case
        when CreditNotes.billingdocument is not initial
        then CreditNotes.billingdocument
      else   I_BillingDocumentBasic.BillingDocument
      end                                 as Billingdocument,

        case
            when CreditNotes.billingdocumenttype is not initial
            then CreditNotes.billingdocumenttype
          else   I_BillingDocumentBasic.BillingDocumentType
          end                             as Billingdocumenttype,

        case
           when CreditNotes.soldtoparty is not initial
           then CreditNotes.soldtoparty
         else   I_BillingDocumentBasic.SoldToParty
         end                              as Soldtoparty,

        case
            when CreditNotes.businessname is not initial
            then CreditNotes.businessname
          else   I_BusinessPartner.BusinessPartnerFullName
          end                             as Businessname,
          
        CreditNotes.typeid        as Typeid,
        CreditNotes.idnumber      as Idnumber,

        case
            when CreditNotes.establishment is not initial
            then CreditNotes.establishment
          else   '000'
          end                             as Establishment,

        case
          when CreditNotes.emissionpoint is not initial
          then CreditNotes.emissionpoint
        else   '000'
        end                               as Emissionpoint,

        case
          when CreditNotes.sequential is not initial
          then CreditNotes.sequential
        else   '000000000'
        end                               as Sequential,

        CreditNotes.accesskey        as Accesskey,

        case
            when CreditNotes.documenttype is not initial
            then CreditNotes.documenttype
          else   ElectronicDocuments.documentsri
          end                             as Documenttype,

        CreditNotes.issuedate        as Issuedate,
        
        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when CreditNotes.documentstatus is not initial
                then CreditNotes.documentstatus
                else 'PENDING'
          end                             as Documentstatus,


        case CreditNotes.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                            as criticality,

        CreditNotes.messagedocument as Messagedocument,
        CreditNotes.authorizationdate as Authorizationdate,
        CreditNotes.xml              as Xml,
        CreditNotes.mimetype         as Mimetype,
        CreditNotes.filename         as Filename,
        CreditNotes.documentsupplier as Documentsupplier,

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
