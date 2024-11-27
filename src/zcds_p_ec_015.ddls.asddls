@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Documents - Projection View'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_P_EC_015
  provider contract transactional_query
  as projection on ZCDS_RV_EC_015
{
  key Companycode          as CompanyCode,
  key Documentsri          as DocumentSri,
  key Accesskey            as Accesskey,
  key Documentsupplier     as DocumentSupplier,
      Environment          as Environment,
      Establishment        as Establishment,
      Emissionpoint        as Emissionpoint,
      Sequential           as Sequential,
      Supplierid           as SupplierId,
      Suppliername         as SupplierName,
      Companyname          as CompanyName,
      @Semantics.amount.currencyCode: 'Currency'
      Iceamount            as IceAmount,
      @Semantics.amount.currencyCode: 'Currency'
      Taxamount            as TaxAmount,
      @Semantics.amount.currencyCode: 'Currency'
      Discountamount       as DiscountAmount,
      @Semantics.amount.currencyCode: 'Currency'
      Subtotalamount       as SubtotalAmount,
      @Semantics.amount.currencyCode: 'Currency'
      Totalamount          as TotalAmount,
      Currency             as Currency,
      Customerid           as CustomerId,
      Customername         as CustomerName,
      Purchasingdocument   as PurchasingDocument,
      Supplierinvoice      as SupplierInvoice,
      Fiscalyear           as FiscalYear,
      Accountingdocument   as AccountingDocument,
      Accountingfiscalyear as AccountingFiscalYear,
      Supplier             as Supplier,
      Customer             as Customer,
      Creationdate         as CreationDate,
      Creationtime         as CreationTime,
      Rejectiondate        as RejectionDate,
      Rejectiontime        as RejectionTime,
      Issuedate            as IssueDate,
      Issuetime            as IssueTime,
      Authorizationdate    as AuthorizationDate,
      Authorizationtime    as AuthorizationTime,
      Documentstatus       as DocumentStatus,
      Receptionstatus      as ReceptionStatus,
      Typesupport          as TypeSupport,
      Reason               as Reason,
      Message              as Message,
      Supportdocumenttype  as SupportDocumentType,
      Supportdocument      as SupportDocument,
      Supportdocumentdate  as SupportDocumentDate,

      _Withholdings   : redirected to composition child ZCDS_P_EC_016,
      _PurchaseOrders : redirected to composition child ZCDS_P_EC_017

}
