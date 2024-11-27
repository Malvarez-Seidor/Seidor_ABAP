@EndUserText.label: 'Electronic Documents'// Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_001
  
  as projection on ZCDS_RV_EC_001
{
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]

  key Companycode              as CompanyCode,
  key Documenttype             as DocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI', element: 'value_low' }, distinctValues: true } ]
  key Documentsri              as DocumentSri,
  key Sequence                 as Sequence,
      
      @Consumption.valueHelpDefinition: [ { entity:{ name: 'I_DistributionChannelText' , element: 'DistributionChannel' }, distinctValues: true } ]
      Export                   as Export,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_DistributionChannelText' , element: 'DistributionChannel' }, distinctValues: true } ]
      Refunds                  as Refunds,
      Reason                   as Reason,
      
      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName as CompanyCodeName


}
