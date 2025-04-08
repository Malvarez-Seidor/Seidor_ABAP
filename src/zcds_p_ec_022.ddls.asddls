@EndUserText.label: 'Purchasing Document ATS'// Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_022
  provider contract transactional_query
  as projection on ZCDS_RV_EC_022
{
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
                     
      @ObjectModel.text.element: ['CompanyCodeName']
  key Companycode              as CompanyCode,

      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENTTYPE' , element: 'AccountingDocumentType' }, distinctValues: true } ]
  key AccountingDocumentType         as AccountingDocumentType,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI', element: 'value_low' }, distinctValues: true } ]
      Codesri                        as CodeSri,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      CompanyCodeName                as CompanyCodeName,
      
      @Search.defaultSearchElement: true
      AccountingDocumentTypeName     as AccountingDocumentTypeName

}
