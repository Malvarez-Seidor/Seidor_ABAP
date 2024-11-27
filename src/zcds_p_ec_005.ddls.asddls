@EndUserText.label: 'Clase de Condiciones' // Projection View
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_005
  as projection on ZCDS_RV_EC_005

{
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                         as CompanyCode,
  
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'ZSH_CONDITYPE', element: 'ConditionType' },
                     additionalBinding: [ { localElement: 'Ccondition', element: 'ConditionApplication' },
                                          { localElement: 'ConditionTypeName', element: 'Description' } ] } ]
  key Ccondition                          as CCondition,
  
      @ObjectModel.text.element: ['ConditionApplication']
  key conditionapplication                as ConditionApplication,
  
      @Consumption.valueHelpDefinition: [{ entity: {name: 'ZSH_TYPE_COND' , element: 'value_low' }, distinctValues: true }]
      Typecondition                       as TypeCondition,
      
      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName            as CompanyCodeName,
      
      @ObjectModel.text.element: ['ConditionTypeName']
      _PricingCondition.ConditionTypeName as ConditionTypeName
}
