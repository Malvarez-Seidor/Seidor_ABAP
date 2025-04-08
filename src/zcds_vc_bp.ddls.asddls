@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bussines Parnert - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZCDS_VC_BP
   as select from I_JournalEntryItem           as I_JournalEntryItem

{

  key CompanyCode                    as CompanyCode,
  key FiscalYear                     as FiscalYear,
  key AccountingDocument             as AccountingDocument,
      Supplier                       as Supplier,
      AccountingDocCreatedByUser     as AccountingDocCreatedByUser,
      DocumentItemText               as DocumentItemText
      
} where FinancialAccountType      = 'K'
    and Ledger                    = '0L'
//    and IsReversal is initial
//    and IsReversed is initial
group by
  CompanyCode,
  AccountingDocument,
  FiscalYear,
  Supplier,
  AccountingDocCreatedByUser,
  DocumentItemText
