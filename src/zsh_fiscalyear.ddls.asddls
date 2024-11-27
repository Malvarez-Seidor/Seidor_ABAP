@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fiscal Year Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_FISCALYEAR
  as select distinct from I_JournalEntry 
{
  key FiscalYear
      
      //I_FiscalYearForCompanyCode
}
