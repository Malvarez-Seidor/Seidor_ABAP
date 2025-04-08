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
define view entity ZCDS_VED_REP_ATS
  as select from ZCDS_P_ATS
  association [0..*] to ZCDS_VED_REP_COM_ATS as _AccountingDocumentItem on $projection.CompanyCode        = _AccountingDocumentItem.CompanyCode
                                                                       and $projection.FiscalYear         = _AccountingDocumentItem.FiscalYear
                                                                       and $projection.AccountingDocument = _AccountingDocumentItem.AccountingDocument
                                                                   
  association [0..*] to ZCDS_VED_REP_VEN_ATS as _BillingDocumentItem    on $projection.CompanyCode        = _BillingDocumentItem.CompanyCode
                                                                       and $projection.FiscalYear         = _BillingDocumentItem.FiscalYear
                                                                       and $projection.AccountingDocument = _BillingDocumentItem.AccountingDocument
                                                                       and $projection.BillingDocument    = _BillingDocumentItem.BillingDocument
                                                                       
{
  
  @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' } } ]
  key CompanyCode                  as CompanyCode,
  
  @Semantics.fiscal.year: true
  @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key FiscalYear                   as FiscalYear,
  
  key AccountingDocument           as AccountingDocument,
  
  key AccountingDocumentType       as AccountingDocumentType,
  
  key FiscalPeriod                 as FiscalPeriod,
  
  key BillingDocument              as BillingDocument,
  
  key BillingDocumentType          as BillingDocumentType,
      
      TypeProces                   as TypeProces,
      
      BusinessPartner              as BusinessPartner,
      
      Businessname                 as BusinessName,
      
      TypeId                       as TypeId,
      
      IdNumber                     as IdNumber,
      
      DocumentType                 as DocumentType,
      
      concat( Establishment,concat( Emissionpoint, Sequential ) ) as NumberDocument,
      
      Accesskey                    as Accesskey,
      
      IssueDate                    as IssueDate,
      
      DocumentStatus               as DocumentStatus,
      
      AuthorizationDate            as AuthorizationDate,
      
      AccountingDocumentWith       as AccountingDocumentWith,
      
      FiscalYearWith               as FiscalYearWith,
      
      _AccountingDocumentItem,
      
      _BillingDocumentItem
      
} where DocumentStatus is not initial //= 'AUTHORIZED'
    and DocumentType   is not initial
    and TypeProces     is not initial
