@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Withholdings report' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_VC_DET_RET
  as select from    I_Withholdingtaxitem
    left outer join I_WithholdingTaxCode as I_WithholdingTaxCode     on  I_WithholdingTaxCode.WithholdingTaxCode = I_Withholdingtaxitem.WithholdingTaxCode
                                                                     and I_WithholdingTaxCode.Country            = 'EC'

    left outer join I_Extendedwhldgtaxcode as I_ExtendedWhldgTaxCode on  I_ExtendedWhldgTaxCode.WithholdingTaxType = I_Withholdingtaxitem.WithholdingTaxType
                                                                     and I_ExtendedWhldgTaxCode.WithholdingTaxCode = I_Withholdingtaxitem.WithholdingTaxCode
                                                                     and I_ExtendedWhldgTaxCode.Country            = I_WithholdingTaxCode.Country

    inner join zdt_ec_014             as I_OfficialCode         on  I_OfficialCode.withholdingtaxtype = I_Withholdingtaxitem.WithholdingTaxType
                                                                and I_OfficialCode.withholdingtaxcode = I_Withholdingtaxitem.WithholdingTaxCode

{

  key I_Withholdingtaxitem.CompanyCode                        as CompanyCode,
  key I_Withholdingtaxitem.FiscalYear                         as FiscalYear,
  key I_Withholdingtaxitem.AccountingDocument                 as AccountingDocument,
  key I_Withholdingtaxitem.AccountingDocumentItem             as AccountingDocumentItem,
  key I_Withholdingtaxitem.WithholdingTaxType                 as WithholdingTaxType,
      I_Withholdingtaxitem.WithholdingTaxCode                 as WithholdingTaxCode,
      I_OfficialCode.officialwhldgtaxcode                     as OfficialWhldgTaxCode,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      abs( I_Withholdingtaxitem.WhldgTaxBaseAmtInCoCodeCrcy ) as WhldgTaxBaseAmtInCoCodeCrcy,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      abs( I_Withholdingtaxitem.WhldgTaxAmtInCoCodeCrcy     ) as WhldgTaxAmtInCoCodeCrcy,

      I_Withholdingtaxitem.WithholdingTaxPercent              as WithholdingTaxPercent,
      I_Withholdingtaxitem.CompanyCodeCurrency                as CompanyCodeCurrency,
      I_Withholdingtaxitem.Country                            as Country,
      I_ExtendedWhldgTaxCode.WhldgTaxReferenceText            as WhldgTaxReferenceText,
      I_WithholdingTaxCode.WhldgTaxCodeName                   as WhldgTaxCodeName

}
