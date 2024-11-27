@EndUserText.label: 'Payment Method' //Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_006
  as projection on ZCDS_RV_EC_006
{ 
  @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                    additionalBinding: [{ localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                             as CompanyCode,
  
  @Consumption.valueHelpDefinition: [ { entity:  { name: 'ZSH_PAYMETHOD', element: 'PaymentMethod' },
                 additionalBinding: [ { localElement: 'PaymentMethodDescription', element: 'PaymentMethodDescription' } ] } ]
  key Paymentmethod                           as PaymentMethod,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_PAYMENT', element: 'value_low' } } ]
  key Paymentsri                              as PaymentSri,
  
      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName                as CompanyCodeName,
      
      @ObjectModel.text.element: ['PaymentMethodDescription']
      _PaymentMethod.PaymentMethodDescription as PaymentMethodDescription
}
