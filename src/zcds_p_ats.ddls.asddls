@AbapCatalog.sqlViewName: 'ZCDS_V_ATS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Report - View'
@Metadata.ignorePropagatedAnnotations: true
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define view ZCDS_P_ATS
  as select from I_JournalEntry  // Documento Contable
       
    inner join      ZCDS_VC_TAX_COM                      as I_OperationalAcctgDocTaxItem
            on I_OperationalAcctgDocTaxItem.AccountingDocument         = I_JournalEntry.AccountingDocument
           and I_OperationalAcctgDocTaxItem.CompanyCode                = I_JournalEntry.CompanyCode
           and I_OperationalAcctgDocTaxItem.FiscalYear                 = I_JournalEntry.FiscalYear
           
    left outer join ZCDS_VED_REP_COM_ATS                 as I_PurchasingDocuments 
            on I_PurchasingDocuments.AccountingDocument                = I_JournalEntry.AccountingDocument
           and I_PurchasingDocuments.CompanyCode                       = I_JournalEntry.CompanyCode
           and I_PurchasingDocuments.FiscalYear                        = I_JournalEntry.FiscalYear
           
    left outer join ZCDS_VED_REP_VEN_ATS                 as I_SalesDocuments  
            on I_SalesDocuments.AccountingDocument                     = I_JournalEntry.AccountingDocument
           and I_SalesDocuments.CompanyCode                            = I_JournalEntry.CompanyCode
           and I_SalesDocuments.FiscalYear                             = I_JournalEntry.FiscalYear
    
    left outer join      zdt_ec_001                      as ElectronicDocuments       
        on  ElectronicDocuments.companycode                            = I_SalesDocuments.CompanyCode
       and  ElectronicDocuments.documenttype                           = I_SalesDocuments.BillingDocumentType
       and  ElectronicDocuments.documentsri                            = '01'
       and  ElectronicDocuments.sequence                               = '01'
    
    
{

  key I_JournalEntry.CompanyCode                              as CompanyCode,

  key I_JournalEntry.FiscalYear                               as FiscalYear,
  
  key I_JournalEntry.FiscalPeriod                             as FiscalPeriod,
  
  key I_JournalEntry.AccountingDocument                       as AccountingDocument,
  
  key I_JournalEntry.AccountingDocumentType                   as AccountingDocumentType,
  
  
  
  key case
        when I_SalesDocuments.BillingDocument     is not initial
        then I_SalesDocuments.BillingDocument
        else ''
        end                                                   as BillingDocument,
 
  key case
        when I_SalesDocuments.BillingDocumentType is not initial
        then I_SalesDocuments.BillingDocumentType
        else ''
        end                                                   as BillingDocumentType,

      case
        when I_PurchasingDocuments.Supplier        is not initial
        then I_PurchasingDocuments.Supplier
        when I_SalesDocuments.Customer            is not initial
        then I_SalesDocuments.Customer
        else ''
        end                                                   as BusinessPartner,

      case
        when I_PurchasingDocuments.BusinessName    is not initial
        then I_PurchasingDocuments.BusinessName
        when I_SalesDocuments.BusinessName        is not initial
        then I_SalesDocuments.BusinessName
        else ''
        end                                                   as Businessname,
        
      case
        when I_PurchasingDocuments.TypeId          is not initial
        then I_PurchasingDocuments.TypeId
        when I_SalesDocuments.TypeId              is not initial
        then I_SalesDocuments.TypeId
        else ''
        end                                                   as TypeId,
        
      case
        when I_PurchasingDocuments.IdNumber        is not initial
        then I_PurchasingDocuments.IdNumber
        when I_SalesDocuments.IdNumber            is not initial
        then I_SalesDocuments.IdNumber
        else ''
        end                                                   as IdNumber,
        
      case
        when I_PurchasingDocuments.Establishment   is not initial
        then I_PurchasingDocuments.Establishment
        when I_SalesDocuments.Establishment       is not initial
        then I_SalesDocuments.Establishment
        else ''
        end                                                   as Establishment,

      case
        when I_PurchasingDocuments.Emissionpoint   is not initial
        then I_PurchasingDocuments.Emissionpoint
        when I_SalesDocuments.Emissionpoint       is not initial
        then I_SalesDocuments.Emissionpoint
        else ''
        end                                                   as Emissionpoint,

      case
        when I_PurchasingDocuments.Sequential      is not initial
        then I_PurchasingDocuments.Sequential
        when I_SalesDocuments.Sequential          is not initial
        then I_SalesDocuments.Sequential
        else ''
        end                                                   as Sequential,
      
      case
        when I_PurchasingDocuments.Accesskey       is not initial
        then I_PurchasingDocuments.Accesskey
        when I_SalesDocuments.Accesskey           is not initial
        then I_SalesDocuments.Accesskey
        else ''
        end                                                   as Accesskey,
        
      case
        when I_PurchasingDocuments.DocumentType    is not initial
        then I_PurchasingDocuments.DocumentType
        when I_SalesDocuments.DocumentType        is not initial
        then I_SalesDocuments.DocumentType
        else ''
        end                                                   as DocumentType,
              
      case
        when I_JournalEntry.IsReversed            is not initial
        then 'Anulado'
        when I_PurchasingDocuments.DocumentType    is not initial
        then 'Compra'
        when I_SalesDocuments.DocumentType        is not initial
         and ElectronicDocuments.export           is initial
        then 'Venta'
        when I_SalesDocuments.DocumentType        is not initial
         and ElectronicDocuments.export           is not initial
        then 'Exportacion'
        else ''
        end                                                   as TypeProces,

      case
        when I_PurchasingDocuments.IssueDate       is not initial
        then I_PurchasingDocuments.IssueDate
        when I_SalesDocuments.IssueDate           is not initial
        then I_SalesDocuments.IssueDate
        else ''
        end                                                   as IssueDate,
        
      case
        when I_PurchasingDocuments.AuthorizationDate is not initial
        then I_PurchasingDocuments.AuthorizationDate
        when I_SalesDocuments.AuthorizationDate     is not initial
        then I_SalesDocuments.AuthorizationDate
        else ''
        end                                                   as AuthorizationDate,

      case
        when I_PurchasingDocuments.DocumentStatus  is not initial
        then I_PurchasingDocuments.DocumentStatus
        when I_SalesDocuments.DocumentStatus      is not initial
        then I_SalesDocuments.DocumentStatus
        else ''
        end                                                   as DocumentStatus,
    
    
      case
        when I_SalesDocuments.AccountingDocumentWith   is not initial
        then I_SalesDocuments.AccountingDocumentWith
//        when I_PurchasingDocuments.AccountingDocument   is not initial
//        then I_PurchasingDocuments.AccountingDocument
        else ''
        end                                                   as AccountingDocumentWith,
        
      case
        when I_SalesDocuments.FiscalYearWith      is not initial
        then I_SalesDocuments.FiscalYearWith
//        when I_PurchasingDocuments.FiscalYear     is not initial
//        then I_PurchasingDocuments.FiscalYear
        else ''
        end                                                   as FiscalYearWith
       
}
