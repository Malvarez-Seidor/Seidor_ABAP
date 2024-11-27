//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier Data - Projection View'
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
   serviceQuality: #X,
   sizeCategory: #S,
   dataClass: #MIXED
}

define view entity ZCDS_P_EC_010
  as projection on ZCDS_RV_EC_010
{
  key Companycode          as CompanyCode,
  key Fiscalyear           as FiscalYear,
  key Deliverydocument     as DeliveryDocument,
  key Deliverydocumenttype as DeliveryDocumentType,
  key Carrierid            as CarrierId,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid               as TypeId,

      Businessname         as BusinessName,
      Carplate             as CarPlate,
      Startdate            as StartDate,
      Enddate              as EndDate,
      /* Associations */
      _TransportGuides : redirected to parent ZCDS_P_DOC_SD_GUIA
}
