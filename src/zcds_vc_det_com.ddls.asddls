@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Purchase Report - View Entity Dimension Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_VC_DET_COM
  as select from I_OperationalAcctgDocItem

    inner join   I_ProductDescription         as I_ProductDescription         on  I_ProductDescription.Product  = I_OperationalAcctgDocItem.Product
                                                                              and I_ProductDescription.Language = $session.system_language

    inner join   I_OperationalAcctgDocTaxItem as I_OperationalAcctgDocTaxItem on  I_OperationalAcctgDocTaxItem.AccountingDocument = I_OperationalAcctgDocItem.AccountingDocument
                                                                              and I_OperationalAcctgDocTaxItem.CompanyCode        = I_OperationalAcctgDocItem.CompanyCode
                                                                              and I_OperationalAcctgDocTaxItem.FiscalYear         = I_OperationalAcctgDocItem.FiscalYear
                                                                              and I_OperationalAcctgDocTaxItem.TaxCode            = I_OperationalAcctgDocItem.TaxCode

    inner join   zdt_ec_003                   as TypesTaxes                   on  TypesTaxes.taxcode     = I_OperationalAcctgDocTaxItem.TaxCode
                                                                              and TypesTaxes.companycode = I_OperationalAcctgDocTaxItem.CompanyCode

{

  key I_OperationalAcctgDocItem.CompanyCode                        as CompanyCode,
  key I_OperationalAcctgDocItem.FiscalYear                         as FiscalYear,
  key I_OperationalAcctgDocItem.AccountingDocument                 as AccountingDocument,
  key I_OperationalAcctgDocItem.AccountingDocumentItem             as AccountingDocumentItem,
      I_OperationalAcctgDocItem.DocumentItemText                   as DocumentItemText,

      @ObjectModel.text.element: [ 'ProductDescription' ]
      I_OperationalAcctgDocItem.Product                            as Product,
      I_ProductDescription.ProductDescription                      as ProductDescription,

      @Aggregation.default: #SUM
      @Semantics: { quantity : {unitOfMeasure: 'BaseUnit'} }
      I_OperationalAcctgDocItem.Quantity                           as Quantity,
      I_OperationalAcctgDocItem.BaseUnit                           as BaseUnit,
      I_OperationalAcctgDocItem.TaxCode                            as TaxCode,
      TypesTaxes.taxratepercent                                    as TaxRatePercent,

      @Aggregation.default: #SUM
      @Semantics: { amount : {currencyCode: 'CompanyCodeCurrency'} }
      I_OperationalAcctgDocItem.AbsoluteAmountInCoCodeCrcy         as TaxBaseAmountInCoCodeCrcy,

      @Aggregation.default: #SUM
      @Semantics: { amount : {currencyCode: 'CompanyCodeCurrency'} }
      ( cast( I_OperationalAcctgDocItem.AbsoluteAmountInCoCodeCrcy as abap.fltp ) * 
        cast( TypesTaxes.taxratepercent as abap.fltp ) / cast( 100 as abap.fltp ) ) as TaxAmountInCoCodeCrcy,

      @Aggregation.default: #SUM
      @Semantics: { amount : {currencyCode: 'CompanyCodeCurrency'} }
      ( cast( I_OperationalAcctgDocItem.AbsoluteAmountInCoCodeCrcy as abap.fltp ) + 
      ( cast( I_OperationalAcctgDocItem.AbsoluteAmountInCoCodeCrcy as abap.fltp ) * 
        cast( TypesTaxes.taxratepercent as abap.fltp ) / cast( 100 as abap.fltp ) ) ) as TotalAmountInCoCodeCrcy,

      @Aggregation.default: #SUM
      @Semantics: { amount : {currencyCode: 'CompanyCodeCurrency'} }
      I_OperationalAcctgDocItem.AbsoluteAmountInCoCodeCrcy         as AbsoluteAmountInCoCodeCrcy,

      TypesTaxes.taxsupportid                                      as taxSupportId,
      TypesTaxes.supporttaxcode                                    as SupportTaxCode,
      TypesTaxes.taxsidrate                                        as TaxsIdRate,

      I_OperationalAcctgDocItem.CompanyCodeCurrency                as CompanyCodeCurrency

}
