@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Documentos FI Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSH_DOCUMENT_FI 
as select distinct from I_JournalEntry 
{
  key AccountingDocument,
  key CompanyCode,
  key FiscalYear
}
