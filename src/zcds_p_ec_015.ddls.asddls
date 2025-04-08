@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Documents' // Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_015
  provider contract transactional_query
  as projection on ZCDS_RV_EC_015
{
  
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CompanyCodeName' ]
      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                     additionalBinding: [ { localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode          as CompanyCode,
  
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TRSRI' , element: 'value_low' }, distinctValues: true } ]
  key Documentsri          as DocumentSri,
  
  key Accesskey            as Accesskey,
  key Documentsupplier     as DocumentSupplier,
      Environment          as Environment,
      Establishment        as Establishment,
      Emissionpoint        as EmissionpPoint,
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
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Supplier_VH' , element: 'Supplier' }, distinctValues: true } ]
      Supplier             as Supplier,
      
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_Customer_VH' , element: 'Customer' }, distinctValues: true } ]
      Customer             as Customer,
      
      Creationdate         as CreationDate,
      Creationtime         as CreationTime,
      Rejectiondate        as RejectionDate,
      Rejectiontime        as RejectionTime,
      Issuedate            as IssueDate,
      Issuetime            as IssueTime,
      Authorizationdate    as AuthorizationDate,
      Authorizationtime    as AuthorizationTime,
      
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'Description' ]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_STATUS_REC' , element: 'value_low' }, distinctValues: true } ]
      Documentstatus       as DocumentStatus,
      
      criticality          as Criticality,
      Receptionstatus      as ReceptionStatus,
      Typesupport          as TypeSupport,
      Reason               as Reason,
      Message              as Message,
      Supportdocumenttype  as SupportDocumentType,
      Supportdocument      as SupportDocument,
      Supportdocumentdate  as SupportDocumentDate,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      CompanyCodeName      as CompanyCodeName,
      
      @Semantics.text:true 
      @Search.defaultSearchElement: true
      Description          as Description,
           
      _Withholdings   : redirected to composition child ZCDS_P_EC_016,
      _PurchaseOrders : redirected to composition child ZCDS_P_EC_017

}
