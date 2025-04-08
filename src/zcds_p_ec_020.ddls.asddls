@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report Configuration 104' //Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_020
  provider contract transactional_query 
  as projection on ZCDS_RV_EC_020
{
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
                     
    @ObjectModel.text.element: ['CompanyCodeName']
    key CompanyCode   as CompanyCode,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_SEL', element: 'value_low' }, distinctValues: true } ]
    key SelectionType as SelectionType,
    
    key Value         as Value,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_104', element: 'Code' }, distinctValues: true } ]
    key CodeBase      as CodeBase,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_104', element: 'Code' }, distinctValues: true } ]
    key CodeNet       as CodeNet,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_104', element: 'Code' }, distinctValues: true } ]
    key CodeTax       as CodeTax,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_2_104', element: 'Code' }, distinctValues: true } ]
    CodigoSeccion     as CodigoSeccion,
    
    _Company.CompanyCodeName as CompanyCodeName
    
}
