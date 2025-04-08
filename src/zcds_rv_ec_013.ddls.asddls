@EndUserText.label: 'Support Details' //'- Roow View Interface'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel: {

      semanticKey: [ '_LiquidationSupports' ]

}

define view entity ZCDS_RV_EC_013

  as select from zdt_ec_013 as SupportDetails
      association to parent ZCDS_RV_EC_012 as _LiquidationSupports 
                  on $projection.Companycode            = _LiquidationSupports.CompanyCode
                 and $projection.Fiscalyear             = _LiquidationSupports.FiscalYear
                 and $projection.Accountingdocument     = _LiquidationSupports.AccountingDocument
                 and $projection.Accountingdocumenttype = _LiquidationSupports.AccountingDocumentType
      association [0..1] to I_Currency          as _Currency on $projection.CompanyCodeCurrency = _Currency.Currency 
{
  
//  @ObjectModel.text.association: '_LiquidationSupports'
  @ObjectModel.foreignKey.association: '_LiquidationSupports'
  @ObjectModel.text.reference.association: '_LiquidationSupports'
  key SupportDetails.companycode            as Companycode,
//  @ObjectModel.text.association: '_LiquidationSupports'
  key SupportDetails.fiscalyear             as Fiscalyear,
//  @ObjectModel.text.association: '_LiquidationSupports'
  key SupportDetails.accountingdocument     as Accountingdocument,
//  @ObjectModel.text.association: '_LiquidationSupports'
  key SupportDetails.accountingdocumenttype as Accountingdocumenttype,
  key SupportDetails.draftuuid              as Draftuuid,
      SupportDetails.typeid                 as Typeid,
      SupportDetails.idnumber               as Idnumber,
      SupportDetails.documenttype           as Documenttype,
      SupportDetails.establishment          as Establishment,
      SupportDetails.emissionpoint          as Emissionpoint,
      SupportDetails.sequential             as Sequential,
      SupportDetails.accesskey              as Accesskey,
      SupportDetails.issuedate              as Issuedate,
      SupportDetails.taxcode                as Taxcode,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amountbasetax          as Amountbasetax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amountbasetax0         as Amountbasetax0,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amountbasenotax        as Amountbasenotax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amountbaseexetax       as Amountbaseexetax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amounttax              as Amounttax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.amountice              as Amountice,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      SupportDetails.total_price            as Total_Price,
      
      case
        when SupportDetails.currency is not initial
        then SupportDetails.currency
        else _LiquidationSupports.CompanyCodeCurrency
        end       as Currency,
      
      SupportDetails.last_changed_by           as Last_changed_by,
      _LiquidationSupports.CompanyCodeCurrency as CompanyCodeCurrency,
      _LiquidationSupports,
      _Currency
      
}
