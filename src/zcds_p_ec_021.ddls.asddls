@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report ATS - Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_021
  provider contract transactional_query 
as projection on ZCDS_RV_EC_021
{   
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' }, 
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
                     
    @ObjectModel.text.element: ['CompanyCodeName']
    key Companycode                 as Companycode,
    
    @Semantics.fiscal.year: true
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
    key FiscalYear                  as FiscalYear,
    
    @Semantics.fiscal.year: true
    @Consumption.valueHelpDefinition: [ { entity: { name: 'I_FiscalYearPeriodText' , element: 'FiscalPeriod' }, distinctValues: true } ]
    key Monat                       as Monat,
    
    Purchases                       as Purchases,
    Sales                           as Sales,
    Export                          as Export,
    Cancel                          as Cancel,
    StatusXML                       as StatusXML,
    
    @Search.defaultSearchElement: true
    @ObjectModel.text.element: [ 'Description' ]
//    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS' , element: 'value_low' }, distinctValues: true } ]
    Status                          as Status,
    Criticality                     as Criticality,
    
    @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #ATTACHMENT }
    Xml                             as Xml,
    
    @Semantics.mimeType: true
    MimeType                        as MimeType,
    FileName                        as FileName,
    
    @Semantics.text:true 
    @Search.defaultSearchElement: true
    CompanyCodeName                 as CompanyCodeName,
    
    @Semantics.text:true 
    @Search.defaultSearchElement: true
    Description                     as Description,
    
    _DetailsTransactionalAnnex
}
