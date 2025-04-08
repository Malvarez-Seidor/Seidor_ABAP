@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report Tax - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZCDS_VC_TAX
  as select from I_OperationalAcctgDocTaxItem

{

  key CompanyCode                    as CompanyCode,
  key AccountingDocument             as AccountingDocument,
  key FiscalYear                     as FiscalYear,
      TaxCode                        as TaxCode,
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum(TaxBaseAmountInCoCodeCrcy) as TaxBaseAmountInCoCodeCrcy,
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum(TaxAmountInCoCodeCrcy)     as TaxAmountInCoCodeCrcy,
      CompanyCodeCurrency            as CompanyCodeCurrency

}
group by
  CompanyCode,
  AccountingDocument,
  FiscalYear,
  TaxCode,
  CompanyCodeCurrency
