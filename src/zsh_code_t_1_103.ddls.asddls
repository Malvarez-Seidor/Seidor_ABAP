@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Code Type 1 for 103 - Search Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view entity ZSH_CODE_T_1_103 
as select from zdt_ec_018
{
  
  @Search.ranking: #HIGH
  @UI: { lineItem: [ { label : 'Codigo' } ] }
  key code as Code,
  
  @Search.ranking: #HIGH
  @UI: { lineItem: [ { label : 'Descripción' } ] }
  description as Description
  
} where report   = '1' 
    and typecode = '1'
