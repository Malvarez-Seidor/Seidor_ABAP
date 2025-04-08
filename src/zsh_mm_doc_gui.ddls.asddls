@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Secuanecial Guia Ayuda de Busqueda'
//@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_MM_DOC_GUI
  as select from zdt_mm_doc_guia
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @UI: { lineItem: [ { label : 'Secuencial' } ] }
  key sequential   as Sequential,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      companycode  as Companycode,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      @UI: { lineItem: [ { label : 'Documento SRI' } ] }
      documenttype as Documenttype

}
