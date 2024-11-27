///@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Transport Guides - Projection View'
@Metadata.allowExtensions: true
define root view entity ZCDS_P_DOC_SD_GUIA
  as projection on ZCDS_RV_DOC_SD_GUIA
{

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CompanyCodeName' ]
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                  as CompanyCode,

      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key Fiscalyear                   as FiscalYear,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_DeliveryDocumentStdVH' , element: 'DeliveryDocument'  } } ]
  key Deliverydocument             as DeliveryDocument,

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'DeliveryDocumentTypeName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_DeliveryDocumentTypeText' , element: 'DeliveryDocumentType' }, distinctValues: true } ]
  key Deliverydocumenttype         as DeliveryDocumentType,

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BusinessName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Customer_VH' , element: 'Customer' }, distinctValues: true } ]
      Shiptoparty                  as ShiptParty,

      @Search.defaultSearchElement: true
      Businessname                 as BusinessName,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid                       as TypeId,

      Idnumber                     as IdNumber,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB' , element: 'Establishment' }, distinctValues: true } ]
      Establishment                as Establishment,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION' , element: 'EmissionPoint' }, distinctValues: true } ]
      Emissionpoint                as EmissionPoint,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_SD_DOC_GUI' , element: 'Sequential' }, distinctValues: true } ]
      Sequential                   as Sequential,

      Accesskey                    as Accesskey,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Documenttype                 as DocumentType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_SD_DAT_GUI' , element: 'IssueDate' }, distinctValues: true } ]
      Issuedate                    as IssueDate,

      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS' , element: 'value_low' }, distinctValues: true } ]
      Documentstatus               as DocumentStatus,

      @Semantics.text: true
      Messagedocument              as MessageDocument,

      Authorizationdate            as AuthorizationDate,

      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #ATTACHMENT }
      Xml                          as Xml,

      @Semantics.mimeType: true
      Mimetype                     as MimeType,

      Filename                     as FileName,

      Documentsupplier             as DocumentSupplier,

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'UserFullName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BusinessUserVH' , element: 'UserID' }, distinctValues: true } ]
      CreatedByUser                as CreatedByUser,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ERDAT' , element: 'CreationDate' }, distinctValues: true } ]
      CreationDate                 as CreationDate,

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'SalesOrganizationName' ]
      SalesOrganization            as SalesOrganization,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FKDAT', element: 'BillingDocumentDate' }, distinctValues: true } ]
      BillingDocumentDate          as BillingDocumentDate,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_LFDAT', element: 'DeliveryDate' }, distinctValues: true } ]
      DeliveryDate                 as DeliveryDate,
      CompleteDeliveryIsDefined    as CompleteDeliveryIsDefined,
      DeliveryBlockReason          as DeliveryBlockReason,
      IsExportDelivery             as IsExportDelivery,
      PickingDate                  as PickingDate,
      TotalNetAmount               as TotalNetAmount,
      @Semantics.currencyCode: true
      TransactionCurrency          as TransactionCurrency,
      ReleasedCreditAmount         as ReleasedCreditAmount,
      OverallSDProcessStatus       as OverallSDProcessStatus,
      OverallGoodsMovementStatus   as OverallGoodsMovementStatus,
      OverallDelivReltdBillgStatus as OverallDelivReltdBillgStatus,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      CompanyCodeName              as CompanyCodeName,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      DeliveryDocumentTypeName     as DeliveryDocumentTypeName,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      UserFullName                 as UserFullName,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      SalesOrganizationName        as SalesOrganizationName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      Description                  as Description,
      
      criticality                  as Criticality,
      
      _TransportData : redirected to composition child ZCDS_P_EC_010
}
