@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales report - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@ObjectModel.usageType:{
  serviceQuality: #X,
    sizeCategory: #S,
       dataClass: #MIXED
}

define view entity ZCDS_VED_REP_VEN
 with parameters P_CompanyCode  : bukrs,
                 P_FiscalYear   : gjahr,
                 P_FiscalPeriod : fins_fiscalperiod
as select from ZCDS_P_VENTAS
  association [1..*] to ZCDS_VC_DET_VEN     as _BillingDocumentItem     on   $projection.BillingDocument        = _BillingDocumentItem.BillingDocument
  association [0..*] to ZCDS_VC_DET_RET     as _Withholdingtaxitem      on   $projection.AccountingDocumentWith = _Withholdingtaxitem.AccountingDocument
                                                                        and  $projection.FiscalYearWith         = _Withholdingtaxitem.FiscalYear
                                                                        and  $projection.CompanyCode            = _Withholdingtaxitem.CompanyCode
  
{
  
  @ObjectModel.text.element: [ 'CompanyCodeName' ]
  @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
            additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key CompanyCode                  as CompanyCode,
  
  @Semantics.fiscal.year: true
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key FiscalYear                   as FiscalYear,
  
  key BillingDocument              as BillingDocument,
  
  @ObjectModel.text.element: [ 'BillingDocumentTypeName' ]
  key BillingDocumentType          as BillingDocumentType,
  key AccountingDocument           as AccountingDocument,
  
  @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
  key AccountingDocumentType       as AccountingDocumentType,
  
  @Semantics.fiscal.yearPeriod: true
  @Consumption.valueHelpDefinition: [ { entity: { name: 'I_FiscalYearPeriod' , element: 'FiscalPeriod' }, distinctValues: true } ]
      FiscalPeriod                 as FiscalPeriod,
      
  @ObjectModel.text.element: [ 'Businessname' ]
      Customer                     as Customer,
      Businessname                 as BusinessName,
      Typeid                       as TypeId,
      Idnumber                     as IdNumber,
      
      concat( Establishment,concat( Emissionpoint, Sequential ) ) as NumberDocument,

      Accesskey                    as Accesskey,
      Documenttype                 as DocumentType,
      Issuedate                    as IssueDate,
      
  @ObjectModel.text.element: [ 'Description' ]
      Documentstatus               as DocumentStatus,
      BillingDocumentIsCancelled   as BillingDocumentIsCancelled,
      
//  @ObjectModel.text.element: [ 'PaymentTermsConditionDesc' ]
//      CustomerPaymentTerms         as CustomerPaymentTerms,
//      PaymentMethod                as PaymentMethod,
      
  @Semantics.amount.currencyCode: 'TransactionCurrency'
      TotalNetAmount               as TotalNetAmount,
      TransactionCurrency          as TransactionCurrency,
      PostingDate                  as PostingDate,
      AccountingDocumentHeaderText as AccountingDocumentHeaderText,
      DocumentReferenceID          as DocumentReferenceID,
      ReverseDocument              as ReverseDocument,
      AccountingDocumentWith       as AccountingDocumentWith,
      FiscalYearWith               as FiscalYearWith,
      
  @ObjectModel.text.element: [ 'DocumentTypeNameWith' ]
      AccountingDocumentTypeWith   as AccountingDocumentTypeWith,
      PostingDateRWith             as PostingDateRWith,
      DocumentHeaderTextWith       as DocumentHeaderTextWith,
      DocumentReferenceIDWith      as DocumentReferenceIDWith,
      ReverseDocumentWith          as ReverseDocumentWith,
      AccountingDocCreatedByUserWith as AccountingDocCreatedByUserWith,
   
   @ObjectModel.text.element: [ 'PersonFullNameWith' ]
      PersonFullNameWith           as PersonFullNameWith,
      
  @Semantics.amount.currencyCode: 'TransactionCurrency'
      TotalTaxAmount               as TotalTaxAmount,
      
  @ObjectModel.text.element: [ 'PersonFullName' ]
      CreatedByUser                as CreatedByUser,
      CompanyCodeName              as CompanyCodeName,
      BillingDocumentTypeName      as BillingDocumentTypeName,
      AccountingDocumentTypeName   as AccountingDocumentTypeName,
      DocumentTypeNameWith         as DocumentTypeNameWith,
      PersonFullName               as PersonFullName,
//      PaymentTermsConditionDesc    as PaymentTermsConditionDesc,
      Description                  as Description,
      _BillingDocumentItem,
      _Withholdingtaxitem

} where CompanyCode  = $parameters.P_CompanyCode
    and FiscalYear   = $parameters.P_FiscalYear
    and FiscalPeriod = $parameters.P_FiscalPeriod
