@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Document Year Ayuda de Busqueda'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
//@Search.searchable: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZSH_Documentyear
  as select distinct from I_GoodsMovementCube
{
  key MaterialDocumentYear

}
