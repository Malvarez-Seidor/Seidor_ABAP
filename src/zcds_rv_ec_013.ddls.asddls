@EndUserText.label: 'Support Details' //'- Roow View Interface'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
//@AbapCatalog.sqlViewName: 'ZCDS_EC_013'
@ObjectModel: {

      semanticKey: [ '_LiquidationSupports' ]

}

define view entity ZCDS_RV_EC_013
  as select from zdt_ec_013 as SupportDetails
      association to parent ZCDS_RV_EC_012 as _LiquidationSupports 
                  on $projection.Companycode            = _LiquidationSupports.Companycode
                 and $projection.Fiscalyear             = _LiquidationSupports.Fiscalyear
                 and $projection.Accountingdocument     = _LiquidationSupports.Accountingdocument
                 and $projection.Accountingdocumenttype = _LiquidationSupports.Accountingdocumenttype
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
  key SupportDetails.documentitem           as Documentitem,
      SupportDetails.typeid                 as Typeid,
      SupportDetails.idnumber               as Idnumber,
      SupportDetails.documenttype           as Documenttype,
      SupportDetails.establishment          as Establishment,
      SupportDetails.emissionpoint          as Emissionpoint,
      SupportDetails.sequential             as Sequential,
      SupportDetails.accesskey              as Accesskey,
      SupportDetails.issuedate              as Issuedate,
      SupportDetails.taxcode                as Taxcode,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amountbasetax          as Amountbasetax,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amountbasetax0         as Amountbasetax0,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amountbasenotax        as Amountbasenotax,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amountbaseexetax       as Amountbaseexetax,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amounttax              as Amounttax,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.amountice              as Amountice,
      @Semantics.amount.currencyCode: 'Currency'
      SupportDetails.total_price            as Total_Price,
      SupportDetails.currency               as Currency,
      SupportDetails.last_changed_by        as Last_changed_by,
      _LiquidationSupports
      
}
