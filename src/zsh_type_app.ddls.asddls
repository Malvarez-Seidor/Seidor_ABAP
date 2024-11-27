@AbapCatalog.sqlViewName: 'ZSH_I_TYPE_APP'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tipos de Aplicación Search Hepl Code'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view ZSH_TYPE_APP
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZDD_TYPE_APP')
{
  @Consumption.hidden: true
  key domain_name,
  //@Consumption.hidden: true
  key value_position,
      @Semantics.language: true
  key language,
      //@Search.defaultSearchElement: true
      //@Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @ObjectModel.text.element: [ 'Description' ]
      value_low,
      @Semantics.text: true
      @Search.ranking: #LOW
      text as Description
}
