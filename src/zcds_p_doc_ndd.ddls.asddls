@EndUserText.label: 'Debit Notes' //- Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_DOC_NDD
  as projection on ZCDS_RV_DOC_NDD
{
 
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CompanyCodeName' ]
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode              as CompanyCode,
  
      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key Fiscalyear               as FiscalYear,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENT_FI' , element: 'AccountingDocument' }, distinctValues: true } ]
  key Accountingdocument       as AccountingDocument,
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENTTYPE' , element: 'AccountingDocumentType' }, distinctValues: true } ]
  key Accountingdocumenttype   as AccountingDocumentType,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BillingDocumentStdVH' , element: 'BillingDocument' }, distinctValues: true } ]
  key Billingdocument          as BillingDocument,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BillingDocumentTypeName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BillingDocumentTypeStdVH' , element: 'BillingDocumentType' }, distinctValues: true } ]
      Billingdocumenttype      as BillingDocumentType,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BusinessName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Customer_VH' , element: 'Customer' }, distinctValues: true } ]
      Soldtoparty              as SoldToParty,
      
      @Search.defaultSearchElement: true
      Businessname             as BusinessName,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid                   as TypeId,
      
      Idnumber                 as IdNumber,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB' , element: 'Establishment' }, distinctValues: true } ]
      Establishment            as Establishment,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION' , element: 'EmissionPoint' }, distinctValues: true } ]
      Emissionpoint            as EmissionPoint,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOC_NDD' , element: 'Sequential' }, distinctValues: true } ]
      Sequential               as Sequential,
      
      Accesskey                as Accesskey,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Documenttype             as DocumentType,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DAT_NDD' , element: 'IssueDate' }, distinctValues: true } ]
      Issuedate                as IssueDate,
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS' , element: 'value_low' }, distinctValues: true } ]
      Documentstatus           as DocumentStatus,
      
      @Semantics.text: true
      Messagedocument          as MessageDocument,
      
      Authorizationdate        as AuthorizationDate,
      
      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #INLINE }
      Xml                      as Xml,
      
      @Semantics.mimeType: true
      Mimetype                 as MimeType,
      
      Filename                 as FileName,
      
      Documentsupplier         as DocumentSupplier,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'UserFullName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BusinessUserVH' , element: 'UserID' }, distinctValues: true } ]
      CreatedByUser            as CreatedByUser,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ERDAT' , element: 'CreationDate' }, distinctValues: true } ]
      CreationDate             as CreationDate,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'SalesOrganizationName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_SalesOrganizationText' , element: 'SalesOrganization' }, distinctValues: true } ]
      SalesOrganization        as SalesOrganization,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FKDAT' , element: 'BillingDocumentDate' }, distinctValues: true } ]
      BillingDocumentDate      as BillingDocumentDate,
      
      BillingDocumentIsCancelled as BillingDocumentIsCancelled,
      
      TotalNetAmount           as TotalNetAmount,
      @Semantics.currencyCode: true
      TransactionCurrency      as TransactionCurrency,
      
      TotalTaxAmount           as TotalTaxAmount,
      
      DocumentReferenceID      as DocumentReferenceID,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      CompanyCodeName          as CompanyCodeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      BillingDocumentTypeName  as BillingDocumentTypeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      UserFullName             as UserFullName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      SalesOrganizationName    as SalesOrganizationName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      AccountingDocumentTypeName as AccountingDocumentTypeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      Description              as Description,
      
      criticality              as Criticality

}
