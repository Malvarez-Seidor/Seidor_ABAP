@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Report - View Entity Dimension'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZCDS_VED_REP_COM
  with parameters
    P_CompanyCode  : bukrs,
    P_FiscalYear   : gjahr,
    P_FiscalPeriod : fins_fiscalperiod
  as select from ZCDS_P_COMPRAS

  association [0..*] to ZCDS_VC_DET_COM as _AccountingDocumentItem on  $projection.CompanyCode        = _AccountingDocumentItem.CompanyCode
                                                                   and $projection.FiscalYear         = _AccountingDocumentItem.FiscalYear
                                                                   and $projection.AccountingDocument = _AccountingDocumentItem.AccountingDocument
                                                                   
  association [0..*] to ZCDS_VC_DET_RET as _Withholdingtaxitem     on  $projection.CompanyCode        = _Withholdingtaxitem .CompanyCode
                                                                   and $projection.FiscalYear         = _Withholdingtaxitem .FiscalYear
                                                                   and $projection.AccountingDocument = _Withholdingtaxitem .AccountingDocument
{
  @ObjectModel.text.element: [ 'CompanyCodeName' ]
  @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
            additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key CompanyCode                  as CompanyCode,
  
  @Semantics.fiscal.year: true
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key FiscalYear                   as FiscalYear,
  key AccountingDocument           as AccountingDocument,
  
  @ObjectModel.text.element: [ 'AccountingDocumentTypeName' ]
  key AccountingDocumentType       as AccountingDocumentType,
      
      @ObjectModel.text.element: [ 'BusinessName' ]
      Supplier                     as Supplier,
      Businessname                 as BusinessName,
      TypeId                       as TypeId,
      IdNumber                     as IdNumber,
      
//      Establishment                as Establishment,
//      Emissionpoint                as Emissionpoint,
//      Sequential                   as Sequential,
      concat( Establishment,concat( Emissionpoint, Sequential ) ) as NumberDocument,
      
      Accesskey                    as Accesskey,
      DocumentType                 as DocumentType,
      IssueDate                    as IssueDate,
      
      @ObjectModel.text.element: [ 'Description' ]
      DocumentStatus               as DocumentStatus,
      
//      EstablishmentWith            as EstablishmentWith,
//      EmissionpointWith            as EmissionpointWith,
//      SequentialWith               as SequentialWith,
      concat( EstablishmentWith,concat( EmissionpointWith, SequentialWith ) ) as NumberDocumentWith,
      
      AccesskeyWith                as AccesskeyWith,
      DocumentTypeWith             as DocumentTypeWith,
      IssueDateWith                as IssueDateWith,
      
      @ObjectModel.text.element: [ 'DescriptionWith' ]
      DocumentStatusWith           as DocumentStatusWith,
      
      @ObjectModel.text.element: [ 'PaymentTermsConditionDesc' ]
      PaymentTerms                 as PaymentTerms,
      PaymentMethod                as PaymentMethod,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      NetAmount                    as NetAmount,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      TaxBaseAmountInCoCodeCrcy    as TaxBaseAmountInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      TaxAmountInCoCodeCrcy        as TaxAmountInCoCodeCrcy,
      
      CompanyCodeCurrency          as CompanyCodeCurrency,
      
      @Semantics.fiscal.yearPeriod: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_FiscalYearPeriod' , element: 'FiscalPeriod' }, distinctValues: true } ]
      FiscalPeriod                 as FiscalPeriod,
      PostingDate                  as PostingDate,
      AccountingDocumentHeaderText as AccountingDocumentHeaderText,
      DocumentReferenceID          as DocumentReferenceID,
      ReverseDocument              as ReverseDocument,
      
      @ObjectModel.text.element: [ 'UserFullName' ]
      AccountingDocCreatedByUser   as AccountingDocCreatedByUser,
      Reference1InDocumentHeader   as Reference1InDocumentHeader,
      
      UserFullName                 as UserFullName,
      BusinessPartnerCategory      as BusinessPartnerCategory,
      
      BusinessPartnerGrouping      as BusinessPartnerGrouping,
      
      IsNaturalPerson              as IsNaturalPerson,
      CompanyCodeName              as CompanyCodeName,
      AccountingDocumentTypeName   as AccountingDocumentTypeName,
      
      PaymentTermsConditionDesc    as PaymentTermsConditionDesc,
      
      Description                  as Description,
      DescriptionW                 as DescriptionWith,

      _AccountingDocumentItem,
      _Withholdingtaxitem
      
} where CompanyCode  = $parameters.P_CompanyCode
    and FiscalYear   = $parameters.P_FiscalYear
    and FiscalPeriod = $parameters.P_FiscalPeriod
