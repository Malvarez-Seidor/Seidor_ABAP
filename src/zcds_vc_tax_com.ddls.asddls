@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Report Tax - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZCDS_VC_TAX_COM
  as select from I_OperationalAcctgDocTaxItem

{

  key CompanyCode                    as CompanyCode,
  key AccountingDocument             as AccountingDocument,
  key FiscalYear                     as FiscalYear,
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
  CompanyCodeCurrency
