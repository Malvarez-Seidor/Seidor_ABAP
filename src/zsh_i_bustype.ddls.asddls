@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo Identificacion  Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_I_BUSTYPE 
as select from I_BusPartTaxTypeText
{
    
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Search.ranking: #HIGH
    @ObjectModel.text.element: [ 'Description' ]
    key BPTaxType as BPTaxType,
    @Semantics.text: true
      @Search.defaultSearchElement: true 
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
    TaxTypeName as Description
    
} where Language = $session.system_language
