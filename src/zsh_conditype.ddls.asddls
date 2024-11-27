@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Condicion de Precios  Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_CONDITYPE 
 as select from I_PricingConditionType
{   
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Search.ranking: #HIGH
    key ConditionApplication,
    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.8
    @Search.ranking: #HIGH
    @ObjectModel.text.element: [ 'Description' ]
    key ConditionType,
    @Semantics.text: true
      @Search.defaultSearchElement: true 
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      _Text[1: Language = $session.system_language].ConditionTypeName as Description
}
