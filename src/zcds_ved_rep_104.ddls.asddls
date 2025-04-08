@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Report 104' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define view entity ZCDS_VED_REP_104
  with parameters
    @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' } } ]
    P_CompanyCode  : bukrs,
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
    P_FiscalYear   : gjahr,
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_FiscalYearPeriodText' , element: 'FiscalPeriod' }, distinctValues: true } ]
    P_FiscalPeriod : fins_fiscalperiod
  as select from    ZCDS_VC_DET_104_2 as Report104
{ 
  
  key Report104.CodigoSeccion                                              as CodigoSeccion,
  key Report104.CodeNeto                                                   as CodeNeto,
      Report104.CodeBase                                                   as CodeBase,
      Report104.CodeTax                                                    as CodeTax,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.TaxBaseAmountInCoCodeCrcy                                  as TaxBaseAmount,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.TaxAmountInCoCodeCrcy  + Report104.TaxAmountCredit         as TaxAmount,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.TaxBaseAmountCredit                                        as TaxBaseAmountCredit,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.TaxAmountCredit                                            as TaxAmountCredit,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.TaxBaseAmountInCoCodeCrcy  + Report104.TaxBaseAmountCredit as NetoAmount,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.WhldgTaxBaseAmtInCoCodeCrcy                                as WhldgTaxBaseAmtInCoCodeCrcy,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.WhldgTaxAmtInCoCodeCrcy                                    as WhldgTaxAmtInCoCodeCrcy,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.WhldgTaxBaseVentas                                         as WhldgTaxBaseVentas,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.WhldgTaxAmtVentas                                          as WhldgTaxAmtVentas,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.BaseRetencion                                              as BaseRetencion,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      Report104.ValoRetencion                                              as ValoRetencion,
      
      Report104.CompanyCodeCurrency                                        as CompanyCodeCurrency,
      
      Report104.DescriptionBase                                            as DescriptionBase,
      Report104.DescriptionNeto                                            as DescriptionNeto,
      Report104.DescriptionTax                                             as DescriptionTax
    
} 
  where CompanyCode  = $parameters.P_CompanyCode
    and FiscalYear   = $parameters.P_FiscalYear
    and FiscalPeriod = $parameters.P_FiscalPeriod
    and CodeBase     is not initial
   group by CodigoSeccion, CodeNeto, CodeBase, CodeTax, CompanyCodeCurrency, WhldgTaxBaseAmtInCoCodeCrcy, WhldgTaxBaseVentas, 
            TaxBaseAmountInCoCodeCrcy, TaxAmountInCoCodeCrcy, TaxBaseAmountCredit, TaxAmountCredit, WhldgTaxAmtInCoCodeCrcy, 
            WhldgTaxAmtVentas, BaseRetencion, ValoRetencion, DescriptionBase, DescriptionNeto, DescriptionTax
