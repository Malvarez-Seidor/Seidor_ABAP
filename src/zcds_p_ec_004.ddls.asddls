@EndUserText.label: 'Types of Identification' //Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_004
  as projection on ZCDS_RV_EC_004
{
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                                 additionalBinding: [{ localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode              as CompanyCode,

      @Consumption.valueHelpDefinition: [ { entity:   { name: 'ZSH_I_BUSTYPE', element: 'BPTaxType' }, 
                                 additionalBinding: [ { localElement: 'TaxTypeName', element: 'Description' } ] } ]
  key BPTaxType                as BPTaxType,

      @Consumption.valueHelpDefinition: [ { entity: {name: 'ZSH_TYPE_TRA' , element: 'value_low' }, distinctValues: true } ]
  key Typedoccument            as TypeDoccument,

      @Consumption.valueHelpDefinition: [ { entity: {name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typedi                   as Typedi,

      @ObjectModel.text.element: ['CompanyCodeName']
      _BusPartTax.TaxTypeName  as TaxTypeName,

      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName as CompanyCodeName
}
