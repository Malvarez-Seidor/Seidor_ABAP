@EndUserText.label: 'Report Configuration 103' // Projection View
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_019
  provider contract transactional_query
  as projection on ZCDS_RV_EC_019
{

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]

      @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode                                as CompanyCode,

    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Extendedwhldgtaxcode', element: 'WithholdingTaxType' }, distinctValues: true } ]
  key WithholdingTaxType                         as WithholdingTaxType,

      @ObjectModel.text.element: ['WhldgTaxCodeName']
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Extendedwhldgtaxcode', element: 'WithholdingTaxCode' }, distinctValues: true } ]
  key WithholdingTaxCode                         as WithholdingTaxCode,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_103', element: 'Code' }, distinctValues: true } ]
  key CodeReport                                 as CodeReport,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_103', element: 'Code' }, distinctValues: true } ]
      CodeBase                                   as CodeBase,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_1_103', element: 'Code' }, distinctValues: true } ]    
      CodeWithholding                            as CodeWithholding,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_CODE_T_2_103', element: 'Code' }, distinctValues: true } ]
      CodigoSeccion                              as CodigoSeccion,

      _Company.CompanyCodeName                   as CompanyCodeName,
      _ExtendedWhldgTaxCodeText.WhldgTaxCodeName as WhldgTaxCodeName

}
