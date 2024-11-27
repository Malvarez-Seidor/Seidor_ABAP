@EndUserText.label: 'Table of Constants' //- Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_007
  as projection on ZCDS_RV_EC_007
{ 
  @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                 additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode              as CompanyCode,
  
   @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_API', element: 'value_low' }, distinctValues: true } ]
  key Api                      as Api,
  
  key Fieldname                as Fieldname,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_SIGN', element: 'value_low' }, distinctValues: true } ]
  key Sign                     as Sign,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_OPTION', element: 'value_low' }, distinctValues: true } ]
  key Options                  as Options,
  
  key Sequence                 as Sequence,
      Low                      as Low,
      High                     as High,
      
      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName as CompanyCodeName
}
