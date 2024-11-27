@AbapCatalog.sqlViewName: 'ZCDS_RV_EMI_RET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Emision de Retenciones - View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view ZCDS_P_EMI_RET
  as select from    I_JournalEntry
    inner join      zdt_ec_001     as ElectronicDocuments on  ElectronicDocuments.companycode            = I_JournalEntry.CompanyCode
                                                          and ElectronicDocuments.documenttype           = I_JournalEntry.AccountingDocumentType
                                                          and ElectronicDocuments.sequence               = '01'
                                                          and ( ElectronicDocuments.documentsri          = '03' 
                                                             or ElectronicDocuments.documentsri          = '07' )
                                                             
    left outer join zdt_fi_doc_ret as Withholdings        on  Withholdings.accountingdocument            = I_JournalEntry.AccountingDocument
                                                          and Withholdings.accountingdocumenttype        = I_JournalEntry.AccountingDocumentType
    left outer join zdt_fi_doc_liq as LiquidationPurchase on  LiquidationPurchase.accountingdocument     = I_JournalEntry.AccountingDocument
                                                          and LiquidationPurchase.accountingdocumenttype = I_JournalEntry.AccountingDocumentType

{

  key I_JournalEntry.CompanyCode as Companycode,

  key I_JournalEntry.FiscalYear  as Fiscalyear,

  key I_JournalEntry.AccountingDocument as Accountingdocument,

  key I_JournalEntry.AccountingDocumentType as Accountingdocumenttype,

  case
    when Withholdings.documentstatus is not initial
     and ElectronicDocuments.documentsri = '07'
      then Withholdings.documentstatus
    when LiquidationPurchase.documentstatus = 'AUTHORIZED'
      and ElectronicDocuments.documentsri = '03'
      then 'PENDING'
    when ElectronicDocuments.documentsri = '03'
      then 'ERROR'
    when ElectronicDocuments.documentsri = '07'
      then 'PENDING'
    else ''
    end       as Documentstatus,
  
  case
    when Withholdings.documenttype is not initial
      then Withholdings.documenttype
    else '07'
  end as Documenttype
  
}
