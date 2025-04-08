@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report FOB - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define view entity ZCDS_VC_FOB
  as select from zdt_ec_005 as CondicionType
  
  inner join I_BillingDocument               as I_BillingDocument 
          on I_BillingDocument.CompanyCode       = CondicionType.companycode
  
  inner join I_BillingDocumentItemPrcgElmnt  as I_BillingDocumentItemPrcgElmnt 
          on I_BillingDocumentItemPrcgElmnt.BillingDocument   = I_BillingDocument.BillingDocument
         and I_BillingDocumentItemPrcgElmnt.ConditionType     = CondicionType.ccondition
         and I_BillingDocumentItemPrcgElmnt.ConditionInactiveReason is initial 
  
  inner join zdt_ec_003                      as TaxType 
          on TaxType.companycode  = CondicionType.companycode
         and TaxType.taxsupportid = CondicionType.typecondition
         and TaxType.taxcode      = I_BillingDocumentItemPrcgElmnt.TaxCode
  
  

{

  key I_BillingDocument.CompanyCode                                 as CompanyCode,
  key I_BillingDocument.FiscalYear                                  as FiscalYear,
  key I_BillingDocument.AccountingDocument                          as AccountingDocument,
  key I_BillingDocument.BillingDocument                             as BillingDocument,
  key I_BillingDocument.BillingDocumentType                         as BillingDocumentType,
      I_BillingDocumentItemPrcgElmnt.TaxCode                        as TaxCode,
      I_BillingDocumentItemPrcgElmnt.ConditionType                  as ConditionType,
      TaxType.taxsupportid                                          as TaxSupportId,
      TaxType.taxsidrate                                            as TaxsIdRate,
      
      @Semantics.amount.currencyCode: 'ConditionCurrency'
      sum(I_BillingDocumentItemPrcgElmnt.ConditionAmount)           as ConditionAmount,
      @Semantics.amount.currencyCode: 'ConditionCurrency'
      sum(I_BillingDocumentItemPrcgElmnt.ConditionRateValue)        as ConditionRateValue,
      I_BillingDocumentItemPrcgElmnt.ConditionCurrency              as ConditionCurrency

} where CondicionType.typecondition = '5'
     or CondicionType.typecondition = '6' 
group by
  I_BillingDocument.CompanyCode,
  I_BillingDocument.FiscalYear,
  I_BillingDocument.AccountingDocument,
  I_BillingDocument.BillingDocument,
  I_BillingDocument.BillingDocumentType,
  I_BillingDocumentItemPrcgElmnt.TaxCode,
  I_BillingDocumentItemPrcgElmnt.ConditionType,
  TaxType.taxsupportid,
  TaxType.taxsidrate,
  I_BillingDocumentItemPrcgElmnt.ConditionCurrency
