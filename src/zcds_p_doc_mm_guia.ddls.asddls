@EndUserText.label: 'Transfer Guides - Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

define root view entity ZCDS_P_DOC_MM_GUIA
  as projection on ZCDS_RV_DOC_MM_GUIA
{

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CompanyCodeName' ]
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                as CompanyCode,

      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_Documentyear' , element: 'MaterialDocumentYear' }, distinctValues: true } ]
  key MaterialDocumentyear       as MaterialDocumentYear,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_Materialdocument' , element: 'MaterialDocument' }, distinctValues: true } ]
  key Materialdocument           as MaterialDocument,
      
      @ObjectModel.text.element: [ 'GoodsMovementTypeName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_GoodsMovementTypeT' , element: 'GoodsMovementType' },
                     additionalBinding: [ { localElement: 'GoodsMovementTypeName', element: 'GoodsMovementTypeName' } ] } ]
  key GoodsMovementType          as GoodsMovementType,

      Idnumber                   as IdNumber,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB' , element: 'Establishment' }, distinctValues: true } ]
      Establishment              as Establishment,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION' , element: 'EmissionPoint' }, distinctValues: true } ]
      Emissionpoint              as Emissionpoint,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_MM_DOC_GUI' , element: 'Sequential' }, distinctValues: true } ]
      Sequential                 as Sequential,
      
      Accesskey                  as Accesskey,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Documenttype               as DocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_MM_DAT_GUI' , element: 'Issuedate' }, distinctValues: true } ]
      Issuedate                  as IssueDate,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS' , element: 'value_low' }, distinctValues: true } ]
      Documentstatus             as DocumentStatus,
      
      @Semantics.text: true
      Messagedocument            as MessageDocument,
      Authorizationdate          as AuthorizationDate,
      
      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #ATTACHMENT }
      Xml                        as Xml,
      
      @Semantics.mimeType: true
      Mimetype                   as MimeType,
      
      Filename                   as FileName,
      
      Documentsupplier           as DocumentSupplier,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_MaterialDocumentTP' , element: 'DocumentDate' }, distinctValues: true } ]
      DocumentDate               as DocumentDate,
      
      @ObjectModel.text.element: [ 'PlantName' ]
      Plant                      as Plant,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_MaterialDocumentTP' , element: 'PostingDate' }, distinctValues: true } ]
      PostingDate                as PostingDate,
      
      GoodsMovementIsCancelled   as GoodsMovementIsCancelled,
      MaterialDocumentItem       as MaterialDocumentItem,
      MaterialDocumentRecordType as MaterialDocumentRecordType,
      
      @ObjectModel.text.element: [ 'StorageLocationName' ]
      StorageLocation            as StorageLocation,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      CompanyCodeName            as CompanyCodeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      GoodsMovementTypeName      as GoodsMovementTypeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      PlantName                  as PlantName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      StorageLocationName        as StorageLocationName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      Description                as Description,
      
      criticality                as Criticality,
      
      _TransportData : redirected to composition child ZCDS_P_EC_011

}
