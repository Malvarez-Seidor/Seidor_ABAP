@AbapCatalog.sqlViewName: 'ZSH_I_TAXES'
@AbapCatalog.viewEnhancementCategory: [#NONE]
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipos de Impuestos Search Hepl Code'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS

define view ZSH_TAXES
  as select from I_TaxCode as TypesTaxes 
  association [1..1] to I_TaxCodeText as _TaxCodeText on  _TaxCodeText.TaxCode                 = TypesTaxes.TaxCode
                                                      and _TaxCodeText.TaxCalculationProcedure = TypesTaxes.TaxCalculationProcedure
                                                      and _TaxCodeText.Language                = $session.system_language

{
@AnalyticsDetails.query.axis: #ROWS
  key  TaxCalculationProcedure  as TaxCalculationProcedure,
  @AnalyticsDetails.query.axis: #ROWS
  key  TaxCode                  as TaxCode,
       _TaxCodeText.TaxCodeName as TaxCodeName,
       TaxTolerancePercent      as TaxTolerancePercent,
       TargetTaxCode            as TargetTaxCode,
       _TaxCodeText.Language    as Language 

}
where
  TaxCalculationProcedure = '0TXEC'
