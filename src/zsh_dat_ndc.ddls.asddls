@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Fecha Emision de Notas de Credito Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_DAT_NDC
  as select distinct from zdt_sd_doc_ndc
{
  key issuedate as IssueDate
}
