//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Support Details - Projection View'
@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType:{
   serviceQuality: #X,
   sizeCategory: #S,
   dataClass: #MIXED
}

define  view entity ZCDS_P_EC_013
  as projection on ZCDS_RV_EC_013
{

      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' } } ]
  key Companycode            as CompanyCode,
  
      @Semantics.fiscal.year: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_FISCALYEAR' , element: 'FiscalYear' }, distinctValues: true } ]
  key Fiscalyear             as FiscalYear,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENT_FI' , element: 'AccountingDocument' }, distinctValues: true } ]
  key Accountingdocument     as AccountingDocument,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_DOCUMENTTYPE' , element: 'AccountingDocumentType' }, distinctValues: true } ]
  key Accountingdocumenttype as AccountingDocumentType,
  key Documentitem           as DocumentItem,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Typeid                 as TypeId,
      Idnumber               as IdNumber,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
      Documenttype           as DocumentType,
      Establishment          as Establishment,
      Emissionpoint          as EmissionPoint,
      Sequential             as Sequential,
      Accesskey              as Accesskey,
      Issuedate              as IssueDate,
   
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_TaxCodeText', element: 'TaxCode' } } ]
      Taxcode                as TaxCode,
      Amountbasetax          as AmountBaseTax,
      Amountbasetax0         as AmountBaseTax0,
      Amountbasenotax        as AmountBaseNoTax,
      Amountbaseexetax       as AmountBaseExeTax,
      Amounttax              as AmountTax,
      Amountice              as AmountIce,
      Total_Price             as Total_Price,

      @Semantics.currencyCode: true
      Currency               as Currency,
      
      _LiquidationSupports : redirected to parent ZCDS_P_EC_012

}
