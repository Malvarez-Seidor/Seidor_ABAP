@EndUserText.label: 'Sequential Maintenance - Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_008
  as projection on ZCDS_RV_EC_008
{
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCode', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                                    as CompanyCode,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI', element: 'value_low' } } ]
  key Documentsri                                    as DocumentSri,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB', element: 'Establishment' } } ]
  key Establishment                                  as Establishment,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION', element: 'EmissionPoint' } } ]
  key Emissionpoint                                  as EmissionPoint,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BusinessUserVH', element: 'UserID' },
                         additionalBinding: [ { localElement: 'UserDescription', element: 'PersonFullName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['UserDescription']
  key Users                                          as Users,
  
  key Sequence                                       as Sequence,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENTTYPE', element: 'AccountingDocumentType' },
                     additionalBinding: [ { localElement: 'AccountingDocumentTypeName', element: 'AccountingDocumentTypeName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
      Accountingdocumenttype                         as AccountingDocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BillingDocumentTypeText_2', element: 'BillingDocumentType' },
                     additionalBinding: [ { localElement: 'BillingDocumentTypeName', element: 'BillingDocumentTypeName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BillingDocumentTypeName' ]
      Billingdocumenttype                            as BillingDocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_DeliveryDocumentTypeText', element: 'DeliveryDocumentType' },
                     additionalBinding: [ { localElement: 'DeliveryDocumentTypeName', element: 'DeliveryDocumentTypeName' } ] } ]
      @Search.defaultSearchElement: true      
      @ObjectModel.text.element: [ 'DeliveryDocumentTypeName' ]
      Deliverydocumenttype                           as DeliveryDocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_GoodsMovementTypeT', element: 'GoodsMovementType' },
                     additionalBinding: [ { localElement: 'GoodsMovementTypeName', element: 'GoodsMovementTypeName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'GoodsMovementTypeName' ]
      Goodsmovementtype                              as GoodsMovementType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_SalesOrganizationText', element: 'SalesOrganization' },
                     additionalBinding: [ { localElement: 'SalesOrganizationName', element: 'SalesOrganizationName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'SalesOrganizationName' ]
      Salesorganization                              as SalesOrganization,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Plant', element: 'Plant' },
                      additionalBinding: [ { localElement: 'PlantName', element: 'PlantName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'PlantName' ]
      Plant                                          as Plant,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_StorageLocation', element: 'StorageLocation' },
                      additionalBinding: [ { localElement: 'StorageLocationName', element: 'StorageLocationName' } ] } ]
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'StorageLocationName' ]
      Storagelocation                                as StorageLocation,

      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName                       as CompanyCodeName,

      @ObjectModel.text.element: ['AccountingDocumentTypeName']
      _AccountingDocument.AccountingDocumentTypeName as AccountingDocumentTypeName,

      @ObjectModel.text.element: ['BillingDocumentTypeName']
      _BillingDocumentType.BillingDocumentTypeName   as BillingDocumentTypeName,

      @ObjectModel.text.element: ['DeliveryDocumentTypeName']
      _DeliveryDocumentType.DeliveryDocumentTypeName as DeliveryDocumentTypeName,

      @ObjectModel.text.element: ['GoodsMovementTypeName']
      _GoodsMovementType.GoodsMovementTypeName       as GoodsMovementTypeName,

      @ObjectModel.text.element: ['SalesOrganizationName']
      _SalesOrganization.SalesOrganizationName       as SalesOrganizationName,

      @ObjectModel.text.element: ['PlantName']
      _Plant.PlantName                               as PlantName,

      @ObjectModel.text.element: ['StorageLocationName']
      _StorageLocation.StorageLocationName           as StorageLocationName,

//      @ObjectModel.text.element: ['UserDescription']
      _Users.UserDescription                         as UserDescription

}
