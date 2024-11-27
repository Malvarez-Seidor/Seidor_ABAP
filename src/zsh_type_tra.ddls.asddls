@AbapCatalog.sqlViewName: 'ZSH_I_TYPE_TRA'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipo de Transacción Search Hepl Code'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view ZSH_TYPE_TRA
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZDD_TYPE_TRA')
{
  @Consumption.hidden: true
  key domain_name,
  //@Consumption.hidden: true
  key value_position,
      @Semantics.language: true
  key language,
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @ObjectModel.text.element: [ 'Description' ]
      value_low,
      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      text as Description
}
