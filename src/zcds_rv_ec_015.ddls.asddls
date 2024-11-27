@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Documents - Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_015
  as select from zdt_ec_015
  composition [0..*] of ZCDS_RV_EC_016 as _Withholdings
  composition [0..*] of ZCDS_RV_EC_017 as _PurchaseOrders
{
  key companycode          as Companycode,
  key documentsri          as Documentsri,
  key accesskey            as Accesskey,
  key documentsupplier     as Documentsupplier,
      environment          as Environment,
      establishment        as Establishment,
      emissionpoint        as Emissionpoint,
      sequential           as Sequential,
      supplierid           as Supplierid,
      suppliername         as Suppliername,
      companyname          as Companyname,
      @Semantics.amount.currencyCode: 'Currency'
      iceamount            as Iceamount,
      @Semantics.amount.currencyCode: 'Currency'
      taxamount            as Taxamount,
      @Semantics.amount.currencyCode: 'Currency'
      discountamount       as Discountamount,
      @Semantics.amount.currencyCode: 'Currency'
      subtotalamount       as Subtotalamount,
      @Semantics.amount.currencyCode: 'Currency'
      totalamount          as Totalamount,
      currency             as Currency,
      customerid           as Customerid,
      customername         as Customername,
      purchasingdocument   as Purchasingdocument,
      supplierinvoice      as Supplierinvoice,
      fiscalyear           as Fiscalyear,
      accountingdocument   as Accountingdocument,
      accountingfiscalyear as Accountingfiscalyear,
      supplier             as Supplier,
      customer             as Customer,
      creationdate         as Creationdate,
      creationtime         as Creationtime,
      rejectiondate        as Rejectiondate,
      rejectiontime        as Rejectiontime,
      issuedate            as Issuedate,
      issuetime            as Issuetime,
      authorizationdate    as Authorizationdate,
      authorizationtime    as Authorizationtime,
      documentstatus       as Documentstatus,
      receptionstatus      as Receptionstatus,
      typesupport          as Typesupport,
      reason               as Reason,
      message              as Message,
      supportdocumenttype  as Supportdocumenttype,
      supportdocument      as Supportdocument,
      supportdocumentdate  as Supportdocumentdate,
      _Withholdings,
      _PurchaseOrders
}
