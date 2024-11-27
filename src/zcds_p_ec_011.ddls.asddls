@EndUserText.label: 'Carrier Data - Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
   serviceQuality: #X,
   sizeCategory: #S,
   dataClass: #MIXED
}
define view entity ZCDS_P_EC_011
  as projection on ZCDS_RV_EC_011
{

  key Companycode                   as CompanyCode,
  key Materialdocumentyear          as MaterialDocumentYear,
  key Materialdocument              as MaterialDocument,
  key Goodsmovementtype             as GoodsMovementType,
  key Carrierid                     as CarrierId,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid                        as TypeId,
      
      Businessname                  as BusinessName,
      Carplate                      as CarPlate,
      Startdate                     as StartDate,
      Enddate                       as EndDate,
      /* Associations */
      _TransferGuides : redirected to parent ZCDS_P_DOC_MM_GUIA
}
