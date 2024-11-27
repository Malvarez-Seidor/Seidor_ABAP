@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Sales report' // View Entity Dimension for Connections
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'BillingDocument'

define view entity ZCDS_VC_DET_VEN
  as select from I_BillingDocumentItem
  
  inner join   zdt_ec_003                   as TypesTaxes                   on  TypesTaxes.taxcode     = I_BillingDocumentItem.TaxCode
                                                                            and TypesTaxes.companycode = I_BillingDocumentItem.CompanyCode
{

  key I_BillingDocumentItem.BillingDocument               as BillingDocument,
  key I_BillingDocumentItem.BillingDocumentItem           as BillingDocumentItem,
      
      @ObjectModel.text.element: [ 'BillingDocumentItemText' ]
      I_BillingDocumentItem.Product                       as Product,
      I_BillingDocumentItem.BillingDocumentItemText       as BillingDocumentItemText,
      
      @Aggregation.default: #SUM
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      I_BillingDocumentItem.BillingQuantity               as BillingQuantity,
      I_BillingDocumentItem.BaseUnit                      as BaseUnit,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      I_BillingDocumentItem.NetAmount                     as NetAmount,
      
      I_BillingDocumentItem.TaxCode                       as TaxCode,
      TypesTaxes.taxratepercent                           as taxratepercent,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      I_BillingDocumentItem.TaxAmount                     as TaxAmount,
       
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      I_BillingDocumentItem.EligibleAmountForCashDiscount as EligibleAmountForCashDiscount,

      I_BillingDocumentItem.TransactionCurrency           as TransactionCurrency,

      I_BillingDocumentItem.SalesDocument                 as SalesDocument,
      I_BillingDocumentItem.SalesDocumentItem             as SalesDocumentItem

}
