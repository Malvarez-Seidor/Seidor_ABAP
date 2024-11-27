@EndUserText.label: 'Withholdings' //'- Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_DOC_RET
  as projection on ZCDS_RV_DOC_RET
{

      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CompanyCodeName' ]
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode                    as CompanyCode,

      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key Fiscalyear                     as FiscalYear,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENT_FI' , element: 'AccountingDocument' }, distinctValues: true } ]
  key Accountingdocument             as AccountingDocument,
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENTTYPE' , element: 'AccountingDocumentType' }, distinctValues: true } ]
  key Accountingdocumenttype         as AccountingDocumentType,
  
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'BusinessName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Supplier_VH' , element: 'Supplier' }, distinctValues: true } ]
      Supplier                       as Supplier,
      
      @Search.defaultSearchElement: true
      Businessname                   as BusinessName,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TISRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid                         as TypeId,
      
      Idnumber                       as IdNumber,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_ESTAB' , element: 'Establishment' }, distinctValues: true } ]
      Establishment                  as Establishment,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_EMISSION' , element: 'EmissionPoint' }, distinctValues: true } ]
      Emissionpoint                  as EmissionPoint,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOC_LIQ' , element: 'Sequential' }, distinctValues: true } ]
      Sequential                     as Sequential,
      
      Accesskey                      as Accesskey,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Documenttype                   as DocumentType,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DAT_LIQ' , element: 'IssueDate' }, distinctValues: true } ]
      Issuedate                      as IssueDate,
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS' , element: 'value_low' }, distinctValues: true } ]
      Documentstatus                 as DocumentStatus,
      
      @Semantics.text: true
      Messagedocument                as MessageDocument,
      
      Authorizationdate              as AuthorizationDate,
      
      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #ATTACHMENT }
      Xml                            as Xml,
      
      @Semantics.mimeType: true
      Mimetype                       as MimeType,
      
      Filename                       as FileName,
      
      Documentsupplier               as DocumentSupplier,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'UserFullName' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_BusinessUserVH' , element: 'UserID' }, distinctValues: true } ]
      AccountingDocCreatedByUser     as AccountingDocCreatedByUser,
      
      AccountingDocumentCreationDate as AccountingDocumentCreationDate,
      
      ReverseDocument                as ReverseDocument,
      
      PostingDate                    as PostingDate,
      
      AccountingDocumentHeaderText   as AccountingDocumentHeaderText,
      
      DocumentItemText               as DocumentItemText,
      
      NetAmountIsPosted              as NetAmountIsPosted,
      
      @Semantics.currencyCode: true
      CompanyCodeCurrency            as CompanyCodeCurrency,
      
      TaxBaseAmountIsNetAmount       as TaxBaseAmountIsNetAmount,
      
      DocumentReferenceID            as DocumentReferenceID,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      CompanyCodeName                as CompanyCodeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      UserFullName                   as UserFullName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      AccountingDocumentTypeName     as AccountingDocumentTypeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      Description                    as Description,
      
      criticality                    as Criticality
      
}
