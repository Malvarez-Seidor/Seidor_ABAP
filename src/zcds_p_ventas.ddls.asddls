@AbapCatalog.sqlViewName: 'ZCDS_V_VENTAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales report - View'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_P_VENTAS
  as select distinct from    I_BillingDocument // Documento de Ventas

    inner join      I_BusinessPartner         as I_BusinessPartner // Cliente        
            on I_BusinessPartner.BusinessPartner             = I_BillingDocument.SoldToParty

    inner join I_Businesspartnertaxnumber as I_Businesspartnertaxnumber // Identificacion Cliente
           on  I_Businesspartnertaxnumber.BusinessPartner    = I_BillingDocument.SoldToParty

    inner join      I_BusinessUserVH          as I_BusinessUser // Usuario Creador Documento de Ventas
            on I_BusinessUser.UserID                         = I_BillingDocument.CreatedByUser

    inner join      zdt_ec_001                as ElectronicDocuments // Configuracion de Documentos Electonicos       
            on ElectronicDocuments.companycode               = I_BillingDocument.CompanyCode
           and ElectronicDocuments.documenttype              = I_BillingDocument.BillingDocumentType
           and ( ElectronicDocuments.documentsri             = '01'
            or   ElectronicDocuments.documentsri             = '04'
            or   ElectronicDocuments.documentsri             = '05' )
           and ElectronicDocuments.sequence                  = '01'

    inner join      I_JournalEntry            as I_JournalEntry  // Documento Contable del Documento de Ventas 
           on  I_JournalEntry.FiscalYear                     = I_BillingDocument.FiscalYear
          and  I_JournalEntry.CompanyCode                    = I_BillingDocument.CompanyCode
          and  I_JournalEntry.AccountingDocument             = I_BillingDocument.AccountingDocument
                    
    inner join      I_BusinessUserVH          as I_BusinessUserEntry  // Usuario Creador Documento Contable
            on I_BusinessUserEntry.UserID                    = I_JournalEntry.AccountingDocCreatedByUser

    inner join      I_CompanyCode             as I_Company  // Datos de Sociedad
           on  I_Company.CompanyCode                         = I_BillingDocument.CompanyCode

    inner join      I_BillingDocumentTypeText as I_BillingDocumentTypeText // Clase de Documento de Ventas
           on  I_BillingDocumentTypeText.BillingDocumentType = I_BillingDocument.BillingDocumentType
          and  I_BillingDocumentTypeText.Language            = $session.system_language

    inner join      I_AccountingDocumentTypeText as I_AccountingDocumentTypeText  // Clase de Documento Contable por Venta
           on  I_AccountingDocumentTypeText.AccountingDocumentType = I_JournalEntry.AccountingDocumentType
          and  I_AccountingDocumentTypeText.Language         = $session.system_language
    
    left outer join I_JournalEntryItem        as I_JournalEntryItem  // Retencion Asociada a la Venta
           on  I_JournalEntryItem.InvoiceReferenceFiscalYear = I_JournalEntry.FiscalYear
          and  I_JournalEntryItem.CompanyCode                = I_JournalEntry.CompanyCode
          and  I_JournalEntryItem.InvoiceReference           = I_JournalEntry.AccountingDocument
          and  I_JournalEntryItem.AccountingDocumentType    <> I_JournalEntry.AccountingDocumentType
          and  I_JournalEntryItem.Ledger                     = '0L'
          and  I_JournalEntryItem.IsReversal is initial    
          and  I_JournalEntryItem.IsReversed is initial 

//inner join    I_OperationalAcctgDocItem           as I_JournalEntryItem
//            on I_JournalEntryItem.CompanyCode            = I_JournalEntry.CompanyCode
//           and I_JournalEntryItem.FiscalYear             = I_JournalEntry.FiscalYear
//           and I_JournalEntryItem.AccountingDocument     = I_JournalEntry.AccountingDocument
//           and I_JournalEntryItem.Supplier               is not initial
//           and I_JournalEntryItem.FinancialAccountType   = 'K'
//           and I_JournalEntryItem.Ledger                 = '0L'
//           and I_JournalEntryItem.IsReversal is initial    
//           and I_JournalEntryItem.IsReversed is initial   
          
    left outer join I_JournalEntry            as I_Withholding  // Documento Contable de la Retencion 
           on  I_Withholding.FiscalYear                      = I_JournalEntryItem.FiscalYear
          and  I_Withholding.CompanyCode                     = I_JournalEntryItem.CompanyCode
          and  I_Withholding.AccountingDocument              = I_JournalEntryItem.AccountingDocument
          and  I_Withholding.IsReversal is initial    
          and  I_Withholding.IsReversed is initial
     
    left outer join I_PaymentTermsConditionsText as I_PaymentTermsConditionsText
           on  I_PaymentTermsConditionsText.PaymentTerms     =  I_BillingDocument.CustomerPaymentTerms
          and  I_PaymentTermsConditionsText.Language         =  $session.system_language
    
    left outer join I_BusinessUserVH          as I_BusinessUserWithholding  // Usuario Creador Documento de Retencion
            on I_BusinessUserWithholding.UserID              = I_JournalEntry.AccountingDocCreatedByUser
    
    left outer join I_AccountingDocumentTypeText as I_WithholdingTypeText // Clase de Documento de Retencion
           on  I_WithholdingTypeText.AccountingDocumentType  = I_Withholding.AccountingDocumentType
          and  I_WithholdingTypeText.Language                = $session.system_language

    left outer join zdt_sd_doc_fac            as InvoiceDocuments  // Fatura Documento Electronico 
           on  InvoiceDocuments.companycode                  = I_BillingDocument.CompanyCode
          and  InvoiceDocuments.billingdocument              = I_BillingDocument.BillingDocument
          and  InvoiceDocuments.billingdocumenttype          = I_BillingDocument.BillingDocumentType
          and  InvoiceDocuments.accountingdocument           = I_BillingDocument.AccountingDocument
          and  InvoiceDocuments.accountingdocumenttype       = I_JournalEntry.AccountingDocumentType
          and  InvoiceDocuments.documenttype                 = ElectronicDocuments.documentsri

    left outer join zdt_sd_doc_ndc            as CreditNotes  // Nota de Credito Documento Electronico
           on  CreditNotes.companycode                       = I_BillingDocument.CompanyCode
          and  CreditNotes.billingdocument                   = I_BillingDocument.BillingDocument
          and  CreditNotes.billingdocumenttype               = I_BillingDocument.BillingDocumentType
          and  CreditNotes.accountingdocument                = I_BillingDocument.AccountingDocument
          and  CreditNotes.accountingdocumenttype            = I_JournalEntry.AccountingDocumentType
          and  CreditNotes.documenttype                      = ElectronicDocuments.documentsri
          
    left outer join zdt_sd_doc_ndd            as DebitNotes  // Nota de Debito Documento Electronico
           on  DebitNotes.companycode                        = I_BillingDocument.CompanyCode
          and  DebitNotes.billingdocument                    = I_BillingDocument.BillingDocument
          and  DebitNotes.billingdocumenttype                = I_BillingDocument.BillingDocumentType
          and  DebitNotes.accountingdocument                 = I_BillingDocument.AccountingDocument
          and  DebitNotes.accountingdocumenttype             = I_JournalEntry.AccountingDocumentType
          and  DebitNotes.documenttype                       = ElectronicDocuments.documentsri
          
    left outer join zdt_ec_004                as I_TypeIdentification  // Typo Identificacion Cliente del SRI
            on I_TypeIdentification.bptaxtype                = I_Businesspartnertaxnumber.BPTaxType
           and I_TypeIdentification.typedoccument            = '02' 
           and I_TypeIdentification.companycode              = I_BillingDocument.CompanyCode
           
    left outer join ZSH_STATUS                as I_Status                  
            on I_Status.value_low                            = InvoiceDocuments.documentstatus
            or I_Status.value_low                            = CreditNotes.documentstatus
            or I_Status.value_low                            = DebitNotes.documentstatus

{

  @ObjectModel.text.element: [ 'CompanyCodeName' ]
  key I_BillingDocument.CompanyCode                           as CompanyCode,

  key I_BillingDocument.FiscalYear                            as FiscalYear,

  key I_BillingDocument.BillingDocument                       as BillingDocument,
  
  @ObjectModel.text.element: [ 'BillingDocumentTypeName' ]
  key I_BillingDocument.BillingDocumentType                   as BillingDocumentType,

  key I_BillingDocument.AccountingDocument                    as AccountingDocument,
  
  @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
  key I_JournalEntry.AccountingDocumentType                   as AccountingDocumentType,

      case
        when InvoiceDocuments.soldtoparty is not initial
        then InvoiceDocuments.soldtoparty
        when CreditNotes.soldtoparty      is not initial
        then CreditNotes.soldtoparty
        when DebitNotes.soldtoparty       is not initial
        then DebitNotes.soldtoparty
        else   I_BillingDocument.SoldToParty
        end                                                   as Customer,

      case
        when InvoiceDocuments.businessname is not initial
        then InvoiceDocuments.businessname
        when CreditNotes.businessname      is not initial
        then CreditNotes.businessname
        when DebitNotes.businessname       is not initial
        then DebitNotes.businessname
        else I_BusinessPartner.BusinessPartnerFullName
        end                                                   as Businessname,
        
      case
        when InvoiceDocuments.typeid is not initial
        then InvoiceDocuments.typeid
        when CreditNotes.typeid      is not initial
        then CreditNotes.typeid
        when DebitNotes.typeid       is not initial
        then DebitNotes.typeid
        else I_TypeIdentification.typedi
        end                                                   as Typeid,
        
      case
        when InvoiceDocuments.idnumber                  is not initial
        then InvoiceDocuments.idnumber
        when CreditNotes.idnumber                       is not initial
        then CreditNotes.idnumber
        when DebitNotes.idnumber                        is not initial
        then DebitNotes.idnumber
        when I_Businesspartnertaxnumber.BPTaxLongNumber is not initial
        then I_Businesspartnertaxnumber.BPTaxLongNumber
        else I_Businesspartnertaxnumber.BPTaxNumber
        end                                                   as Idnumber,
        
      case
        when InvoiceDocuments.establishment is not initial
        then InvoiceDocuments.establishment
        when CreditNotes.establishment      is not initial
        then CreditNotes.establishment
        when DebitNotes.establishment       is not initial
        then DebitNotes.establishment
        else   '000'
        end                                                   as Establishment,

      case
        when InvoiceDocuments.emissionpoint is not initial
        then InvoiceDocuments.emissionpoint
        when CreditNotes.emissionpoint      is not initial
        then CreditNotes.emissionpoint
        when DebitNotes.emissionpoint       is not initial
        then DebitNotes.emissionpoint
        else   '000'
        end                                                   as Emissionpoint,

      case
        when InvoiceDocuments.sequential is not initial
        then InvoiceDocuments.sequential
        when CreditNotes.sequential      is not initial
        then CreditNotes.sequential
        when DebitNotes.sequential       is not initial
        then DebitNotes.sequential
        else   '000000000'
        end                                                   as Sequential,
      
      case
        when InvoiceDocuments.accesskey is not initial
        then InvoiceDocuments.accesskey
        when CreditNotes.accesskey      is not initial
        then CreditNotes.accesskey
        when DebitNotes.accesskey       is not initial
        then DebitNotes.accesskey
        else   ''
        end                                                   as Accesskey,
        
      case
//        when InvoiceDocuments.documenttype is not initial
//        then '18'
//        when CreditNotes.documenttype      is not initial
//        then CreditNotes.documenttype
//        when DebitNotes.documenttype       is not initial
//        then DebitNotes.documenttype
        when ElectronicDocuments.documentsri = '01'
        then '18'
        else ElectronicDocuments.documentsri
        end                                                   as Documenttype,

      case
        when InvoiceDocuments.issuedate is not initial
        then InvoiceDocuments.issuedate
        when CreditNotes.issuedate      is not initial
        then CreditNotes.issuedate
        when DebitNotes.issuedate       is not initial
        then DebitNotes.issuedate
        else '00000000'
        end                                                   as Issuedate,

      @ObjectModel.text.element: [ 'Documentstatus' ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      case
        when InvoiceDocuments.documentstatus is not initial
        then InvoiceDocuments.documentstatus
        when CreditNotes.documentstatus      is not initial
        then CreditNotes.documentstatus
        when DebitNotes.documentstatus       is not initial
        then DebitNotes.documentstatus
        else 'PENDING'
        end                                                   as Documentstatus,
      
      I_BillingDocument.BillingDocumentIsCancelled            as BillingDocumentIsCancelled,
      
      I_BillingDocument.CustomerPaymentTerms                  as CustomerPaymentTerms,
      
      I_BillingDocument.PaymentMethod                         as PaymentMethod,
      
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      I_BillingDocument.TotalNetAmount                        as TotalNetAmount,
      
      I_BillingDocument.TransactionCurrency                   as TransactionCurrency,
      
      I_JournalEntry.FiscalPeriod                             as FiscalPeriod,
      
      I_JournalEntry.PostingDate                              as PostingDate,
      
      I_JournalEntry.AccountingDocumentHeaderText             as AccountingDocumentHeaderText,
      
      I_JournalEntry.DocumentReferenceID                      as DocumentReferenceID,
      
      I_JournalEntry.ReverseDocument                          as ReverseDocument,
      
      I_JournalEntry.AccountingDocCreatedByUser               as AccountingDocCreatedByUser,
      
      I_JournalEntry.Reference1InDocumentHeader               as Reference1InDocumentHeader,
      
      I_Withholding.AccountingDocument                        as AccountingDocumentWith,
      
      I_Withholding.FiscalYear                                as FiscalYearWith,
      
      @ObjectModel.text.element: [ 'DocumentTypeNameWith' ]
      I_Withholding.AccountingDocumentType                    as AccountingDocumentTypeWith,
      
      I_Withholding.PostingDate                               as PostingDateRWith,
      
      I_Withholding.AccountingDocumentHeaderText              as DocumentHeaderTextWith,
      
      I_Withholding.DocumentReferenceID                       as DocumentReferenceIDWith,
      
      I_Withholding.ReverseDocument                           as ReverseDocumentWith,
      
      I_Withholding.AccountingDocCreatedByUser                as AccountingDocCreatedByUserWith,
      
      I_Withholding.Reference1InDocumentHeader                as Reference1InDocumentHeaderWith,
      
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      I_BillingDocument.TotalTaxAmount                        as TotalTaxAmount,
      
      I_BillingDocument.CreatedByUser                         as CreatedByUser,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_BusinessUser.PersonFullName                           as UserFullName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_BusinessUserEntry.PersonFullName                      as PersonFullName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_BusinessUserWithholding.PersonFullName                as PersonFullNameWith,
      
      I_BusinessPartner.BusinessPartnerCategory               as BusinessPartnerCategory,
      
      I_BusinessPartner.BusinessPartnerGrouping               as BusinessPartnerGrouping,
      
      I_BusinessPartner.IsNaturalPerson                       as IsNaturalPerson,

      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_Company.CompanyCodeName                               as CompanyCodeName,

      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      
      I_BillingDocumentTypeText.BillingDocumentTypeName       as BillingDocumentTypeName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_AccountingDocumentTypeText.AccountingDocumentTypeName as AccountingDocumentTypeName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_WithholdingTypeText.AccountingDocumentTypeName        as DocumentTypeNameWith,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_PaymentTermsConditionsText.PaymentTermsConditionDesc as PaymentTermsConditionDesc,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      case
        when I_Status.Description       is not initial
        then I_Status.Description
        else 'Pending'
      end                                                    as Description

}
