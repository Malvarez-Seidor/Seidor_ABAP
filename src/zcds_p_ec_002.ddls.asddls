@EndUserText.label: 'Sequential Administrator' // Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_002
  as projection on ZCDS_RV_EC_002
{
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' }] } ]
  key Companycode              as CompanyCode,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
  key Documentsri              as DocumentSri,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB' , element: 'Establishment' }, distinctValues: true } ]
  key Establishment            as Establishment,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION' , element: 'EmissionPoint' }, distinctValues: true } ]
  key Emissionpoint            as EmissionPoint,
      Objet                    as Objet,
      Address                  as Address,
      Sequential               as Sequential,

      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName as CompanyCodeName

}
