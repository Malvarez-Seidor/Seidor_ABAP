@AbapCatalog.sqlViewName: 'ZCDS_V_COMPRAS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Report - View'
@Metadata.ignorePropagatedAnnotations: true
define view ZCDS_P_COMPRAS
  as select from I_JournalEntry  // Documento Contable del Documento de Compras
    
    inner join      zdt_ec_001                           as ElectronicDocuments  // Configuracion de Documentos Electonicos
            on ElectronicDocuments.companycode                         = I_JournalEntry.CompanyCode
           and ElectronicDocuments.documenttype                        = I_JournalEntry.AccountingDocumentType
           and ( ElectronicDocuments.documentsri                       = '03'
            or   ElectronicDocuments.documentsri                       = '07' )
           and ElectronicDocuments.sequence                            = '01'
           
    inner join      I_OperationalAcctgDocItem            as I_JournalEntryItemSupplier // Documento Contable del Documento de Compras Proveedor
            on I_JournalEntryItemSupplier.CompanyCode                  = I_JournalEntry.CompanyCode
           and I_JournalEntryItemSupplier.FiscalYear                   = I_JournalEntry.FiscalYear
           and I_JournalEntryItemSupplier.AccountingDocument           = I_JournalEntry.AccountingDocument
           and I_JournalEntryItemSupplier.AccountingDocumentType       = I_JournalEntry.AccountingDocumentType
           and I_JournalEntryItemSupplier.Supplier                     is not initial
           and I_JournalEntryItemSupplier.FinancialAccountType         = 'K'
          
    inner join      I_BusinessPartner                    as I_BusinessPartner // Proveedor
            on I_BusinessPartner.BusinessPartner                       = I_JournalEntryItemSupplier.Supplier

    inner join I_Businesspartnertaxnumber                as I_Businesspartnertaxnumber // Identificacion Proveedor
            on I_Businesspartnertaxnumber.BusinessPartner              = I_JournalEntryItemSupplier.Supplier
            
    inner join      I_BusinessUserVH                     as I_BusinessUser  // Usuario Creador Documento Contable
            on I_BusinessUser.UserID                                   = I_JournalEntry.AccountingDocCreatedByUser
          
    inner join      I_CompanyCode                        as I_Company  // Datos de Sociedad
            on I_Company.CompanyCode                                   = I_JournalEntry.CompanyCode

    inner join      I_AccountingDocumentTypeText         as I_AccountingDocumentTypeText  // Clase de Documento Contable por Compra
            on I_AccountingDocumentTypeText.AccountingDocumentType     = I_JournalEntry.AccountingDocumentType
           and I_AccountingDocumentTypeText.Language                   = $session.system_language
    
    inner join      ZCDS_VC_TAX_COM                      as I_OperationalAcctgDocTaxItem
            on I_OperationalAcctgDocTaxItem.AccountingDocument         = I_JournalEntry.AccountingDocument
           and I_OperationalAcctgDocTaxItem.CompanyCode                = I_JournalEntry.CompanyCode
           and I_OperationalAcctgDocTaxItem.FiscalYear                 = I_JournalEntry.FiscalYear
    
    left outer join zdt_fi_doc_liq                       as LiquidationPurchase // Liquidacion de Compras Documento Electronico
            on LiquidationPurchase.companycode                         = I_JournalEntry.CompanyCode
           and LiquidationPurchase.accountingdocument                  = I_JournalEntry.AccountingDocument
           and LiquidationPurchase.accountingdocumenttype              = I_JournalEntry.AccountingDocumentType

    left outer join zdt_fi_doc_ret                       as Withholdings // Retencion Documento Electronico
            on Withholdings.companycode                                = I_JournalEntry.CompanyCode
           and Withholdings.accountingdocument                         = I_JournalEntry.AccountingDocument
           and Withholdings.accountingdocumenttype                     = I_JournalEntry.AccountingDocumentType

    left outer join zdt_ec_004                           as I_TypeIdentification // Typo Identificacion Cliente del SRI
            on I_TypeIdentification.bptaxtype                          = I_Businesspartnertaxnumber.BPTaxType
           and I_TypeIdentification.typedoccument                      = '01' 
           and I_TypeIdentification.companycode                        = I_JournalEntry.CompanyCode
    
    left outer join ZSH_STATUS                           as I_Status                  
            on I_Status.value_low                                      =   LiquidationPurchase.documentstatus
            
    left outer join ZSH_STATUS                           as I_StatusWithholdings                  
            on I_StatusWithholdings.value_low                          =   Withholdings.documentstatus
    
    association [0..1] to I_PaymentTermsConditionsText   as I_PaymentTermsConditionsText
           on  I_PaymentTermsConditionsText.PaymentTerms               =  I_JournalEntryItemSupplier.PaymentTerms
          and  I_PaymentTermsConditionsText.Language                   =  $session.system_language
          and  I_PaymentTermsConditionsText.PaymentTermsConditionDesc  is not initial
{

  @ObjectModel.text.element: [ 'CompanyCodeName' ]
  key I_JournalEntry.CompanyCode                              as CompanyCode,

  key I_JournalEntry.FiscalYear                               as FiscalYear,

  key I_JournalEntry.AccountingDocument                       as AccountingDocument,
  
  @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
  key I_JournalEntry.AccountingDocumentType                   as AccountingDocumentType,

      case
        when LiquidationPurchase.supplier                     is not initial
        then LiquidationPurchase.supplier
        when Withholdings.supplier                            is not initial
        then Withholdings.supplier
        else I_JournalEntryItemSupplier.Supplier
        end                                                   as Supplier,

      case
        when LiquidationPurchase.businessname                 is not initial
        then LiquidationPurchase.businessname
        when Withholdings.businessname                        is not initial
        then Withholdings.businessname
        else I_BusinessPartner.BusinessPartnerFullName
        end                                                   as Businessname,
        
      case
        when LiquidationPurchase.typeid                       is not initial
        then LiquidationPurchase.typeid
        when Withholdings.typeid                              is not initial
        then Withholdings.typeid
        else I_TypeIdentification.typedi
        end                                                   as TypeId,
        
      case
        when LiquidationPurchase.idnumber                     is not initial
        then LiquidationPurchase.idnumber
        when Withholdings.idnumber                            is not initial
        then Withholdings.idnumber
        when I_Businesspartnertaxnumber.BPTaxLongNumber       is not initial
        then I_Businesspartnertaxnumber.BPTaxLongNumber
        else I_Businesspartnertaxnumber.BPTaxNumber
        end                                                   as IdNumber,
        
      case
        when LiquidationPurchase.establishment                is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.establishment
        when ElectronicDocuments.documentsri                  = '07' 
         and I_JournalEntry.DocumentReferenceID               is not initial
        then substring( I_JournalEntry.DocumentReferenceID, 1, 3 )
        else '000'
        end                                                   as Establishment,

      case
        when LiquidationPurchase.emissionpoint                is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.emissionpoint
        when ElectronicDocuments.documentsri                  = '07' 
         and I_JournalEntry.DocumentReferenceID               is not initial
        then substring( I_JournalEntry.DocumentReferenceID, 4, 3 )
        else '000'
        end                                                   as Emissionpoint,

      case
        when LiquidationPurchase.sequential                   is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.sequential
        when ElectronicDocuments.documentsri                  = '07'
         and I_JournalEntry.DocumentReferenceID               is not initial
        then substring( I_JournalEntry.DocumentReferenceID, 7, 9 )
        else '000000000'
        end                                                   as Sequential,
      
      case
        when LiquidationPurchase.accesskey                    is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.accesskey
        when ElectronicDocuments.documentsri                  = '07'
         and I_JournalEntryItemSupplier.DocumentItemText      is not initial
        then I_JournalEntryItemSupplier.DocumentItemText
        else ''
        end                                                   as Accesskey,
        
      case
        when LiquidationPurchase.documenttype                 is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.documenttype
        when ElectronicDocuments.documentsri                  = '03'
        then ElectronicDocuments.documentsri
        when ElectronicDocuments.documentsri                  = '07'
        then '01'
        else ''
        end                                                   as DocumentType,

      case
        when LiquidationPurchase.issuedate                    is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.issuedate
        when ElectronicDocuments.documentsri                  = '03'
        then I_JournalEntry.PostingDate
        when ElectronicDocuments.documentsri                  = '07'
         and I_JournalEntry.DocumentDate                      is not initial
        then I_JournalEntry.DocumentDate
        else '00000000'
        end                                                   as IssueDate,

      @ObjectModel.text.element: [ 'Description' ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      case
        when LiquidationPurchase.documentstatus               is not initial
         and ElectronicDocuments.documentsri                  = '03'
        then LiquidationPurchase.documentstatus
        when ElectronicDocuments.documentsri                  = '03'
        then 'PENDING'
        when ElectronicDocuments.documentsri                  = '07'
        then 'AUTHORIZED'
        else ''
        end                                                   as DocumentStatus,
        
     case
        when Withholdings.establishment                       is not initial
        then Withholdings.establishment
        else   '000'
        end                                                   as EstablishmentWith,

      case
        when Withholdings.emissionpoint                       is not initial
        then Withholdings.emissionpoint
        else   '000'
        end                                                   as EmissionpointWith,

      case
        when Withholdings.sequential                          is not initial
        then Withholdings.sequential
        else   '000000000'
        end                                                   as SequentialWith,
      
      case
        when Withholdings.accesskey                           is not initial
        then Withholdings.accesskey
        else   ''
        end                                                   as AccesskeyWith,
        
      case
        when Withholdings.documenttype                        is not initial
        then Withholdings.documenttype
        when ElectronicDocuments.documentsri                  = '03'
        then '07'
        else ElectronicDocuments.documentsri
        end                                                   as DocumentTypeWith,

      case
        when Withholdings.issuedate                           is not initial
        then Withholdings.issuedate
        else '00000000'
        end                                                   as IssueDateWith,

      @ObjectModel.text.element: [ 'DescriptionW' ]
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      case
        when Withholdings.documentstatus                      is not initial
        then Withholdings.documentstatus
        else 'PENDING'
        end                                                   as DocumentStatusWith,
      
      @ObjectModel.text.element: [ 'PaymentTermsConditionDesc' ]
      I_JournalEntryItemSupplier.PaymentTerms                 as PaymentTerms,
      
      I_JournalEntryItemSupplier.PaymentMethod                as PaymentMethod,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      ( I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy + 
        I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy )  as NetAmount,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy      as TaxAmountInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy  as TaxBaseAmountInCoCodeCrcy,
      
      I_JournalEntry.CompanyCodeCurrency                      as CompanyCodeCurrency,
      
      @Semantics.fiscal.yearPeriod: true
      I_JournalEntry.FiscalPeriod                             as FiscalPeriod,
      
      I_JournalEntry.PostingDate                              as PostingDate,
      
      I_JournalEntry.AccountingDocumentHeaderText             as AccountingDocumentHeaderText,
      
      I_JournalEntry.DocumentReferenceID                      as DocumentReferenceID,
      
      case
        when I_JournalEntry.ReverseDocument                      is not initial
        then I_JournalEntry.ReverseDocument
        else 'No'
        end                                                   as ReverseDocument,
      
      I_JournalEntry.AccountingDocCreatedByUser               as AccountingDocCreatedByUser,
      
      I_JournalEntry.Reference1InDocumentHeader               as Reference1InDocumentHeader,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_BusinessUser.PersonFullName                           as UserFullName,
      
//      1 Persona
//      2 Organización
//      3 Grupo
      I_BusinessPartner.BusinessPartnerCategory               as BusinessPartnerCategory,
      
      I_BusinessPartner.BusinessPartnerGrouping               as BusinessPartnerGrouping,
     
      I_BusinessPartner.IsNaturalPerson                       as IsNaturalPerson,

      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_Company.CompanyCodeName                               as CompanyCodeName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      I_AccountingDocumentTypeText.AccountingDocumentTypeName as AccountingDocumentTypeName,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      i_paymenttermsconditionstext.PaymentTermsConditionDesc  as PaymentTermsConditionDesc,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      case
        when ElectronicDocuments.documentsri                 = '03'
         and I_Status.Description                       is not initial
        then I_Status.Description
        when ElectronicDocuments.documentsri                 = '03'
         and LiquidationPurchase.documentstatus         is initial
        then 'Pending'
        when ElectronicDocuments.documentsri                 = '07'
         and LiquidationPurchase.documentstatus         is initial
        then 'Authorized'
        else 'Authorized'
      end                                                     as Description, 
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      case
        when I_StatusWithholdings.Description       is not initial
        then I_StatusWithholdings.Description
        else 'Pending'
      end                                                    as DescriptionW
      
}
