@EndUserText.label: 'Code Reports - Projection View'
@AccessControl.authorizationCheck: #NOT_ALLOWED
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_018
  provider contract transactional_query
  as projection on ZCDS_RV_EC_018
{       

  @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                 additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
                 
  @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode              as CompanyCode,
  
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_REP_IND', element: 'value_low' }, distinctValues: true } ]
  key Report                   as Report,
  
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_CODE', element: 'value_low' }, distinctValues: true } ]
  key Typecode                 as TypeCode,
  
  key Code                     as Code,
      Description              as Description,

      _Company.CompanyCodeName as CompanyCodeName

}
