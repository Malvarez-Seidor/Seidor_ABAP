@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Report 103' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define view entity ZCDS_VED_REP_103
  with parameters
    @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' } } ]
    P_CompanyCode  : bukrs,
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
    P_FiscalYear   : gjahr,
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_FiscalYearPeriodText' , element: 'FiscalPeriod' }, distinctValues: true } ]
    P_FiscalPeriod : fins_fiscalperiod
  as select from    ZCDS_VC_DET_103 as Report103
      
{
  key Report103.CodigoSeccion                                   as CodigoSeccion,
  key Report103.CodeReport                                      as CodeReport,
      Report103.CodeBase                                        as CodeBase,
      Report103.CodeWithholding                                 as CodeWithholding,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report103.WhldgTaxBaseAmtInCoCodeCrcy         )      as WhldgTaxBaseAmtInCoCodeCrcy,
     
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report103.WhldgTaxAmtInCoCodeCrcy             )      as WhldgTaxAmtInCoCodeCrcy,
      
      Report103.CompanyCodeCurrency                             as CompanyCodeCurrency,
      
      Report103.Description                                     as Description

} where CompanyCode  = $parameters.P_CompanyCode
    and FiscalYear   = $parameters.P_FiscalYear
    and FiscalPeriod = $parameters.P_FiscalPeriod
   group by CodigoSeccion, CodeReport, CodeBase, CodeWithholding, CompanyCodeCurrency, Description
