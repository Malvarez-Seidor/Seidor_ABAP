@EndUserText.label: 'Liquidation Supports' //- 'Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_012
  provider contract transactional_query
  as projection on ZCDS_RV_EC_012
{

      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' } } ]
      //                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                 as CompanyCode,
      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key Fiscalyear                  as FiscalYear,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENT_FI' , element: 'AccountingDocument' }, distinctValues: true } ]
  key Accountingdocument          as AccountingDocument,

      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AccountingDocumentTypeText' , element: 'AccountingDocumentType' },
      //                     additionalBinding: [ { localElement: 'AccountingDocumentTypeName', element: 'AccountingDocumentTypeName' } ],
                     distinctValues: true } ]
  key Accountingdocumenttype      as AccountingDocumentType,
      Filestatus                  as FileStatus,

      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', acceptableMimeTypes: [ 'text/csv' ], contentDispositionPreference: #ATTACHMENT }
      Attachment                  as Attachment,

      @Semantics.mimeType: true
      Mimetype                    as MimeType,
      Filename                    as FileName,
      Criticality                 as Criticality,

      AmountInCompanyCodeCurrency as AmountInCompanyCodeCurrency,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BusinessPartnerFullName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Supplier_VH' , element: 'Supplier' }, distinctValues: true } ]
      Supplier                    as Supplier,

      @Semantics.currencyCode: true
      CompanyCodeCurrency         as CompanyCodeCurrency,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'UserFullName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BusinessUserVH' , element: 'UserID' }, distinctValues: true } ]
      AccountingDocCreatedByUser  as AccountingDocCreatedByUser,

      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      @ObjectModel.text.element: [ 'BusinessPartnerFullName' ]
      BusinessPartnerFullName     as BusinessPartnerFullName,

      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
      @ObjectModel.text.element: [ 'UserFullName' ]
      UserFullName                as UserFullName,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      CompanyCodeName             as CompanyCodeName,

      @Semantics.text:true
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      AccountingDocumentTypeName  as AccountingDocumentTypeName,

      _SupportDetails : redirected to composition child ZCDS_P_EC_013

}
