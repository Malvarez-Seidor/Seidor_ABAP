@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Documents' //- Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true

define root view entity ZCDS_RV_EC_015_R
  as select from zdt_ec_015 as ReceptionDocument
  inner join   I_CompanyCode             as I_Company           on  I_Company.CompanyCode             = ReceptionDocument.companycode
  left outer join  I_Businesspartnertaxnumber as I_customerid   on  I_customerid.BPTaxLongNumber      = ReceptionDocument.customerid
                                                                or  I_customerid.BPTaxNumber          = ReceptionDocument.customerid
  left outer join  I_Businesspartnertaxnumber as I_supplierid   on  I_supplierid.BPTaxLongNumber      = ReceptionDocument.supplierid
                                                                or  I_supplierid.BPTaxNumber          = ReceptionDocument.supplierid
  
{
  key ReceptionDocument.companycode          as Companycode,
  key ReceptionDocument.documentsri          as Documentsri,
  key ReceptionDocument.accesskey            as Accesskey,
  key ReceptionDocument.documentsupplier     as Documentsupplier,
      ReceptionDocument.environment          as Environment,
      ReceptionDocument.establishment        as Establishment,
      ReceptionDocument.emissionpoint        as Emissionpoint,
      ReceptionDocument.sequential           as Sequential,
      
        
      ReceptionDocument.supplierid           as Supplierid,
      ReceptionDocument.suppliername         as Suppliername,
      ReceptionDocument.companyname          as Companyname,
      @Semantics.amount.currencyCode: 'Currency'
      ReceptionDocument.iceamount            as Iceamount,
      @Semantics.amount.currencyCode: 'Currency'
      ReceptionDocument.taxamount            as Taxamount,
      @Semantics.amount.currencyCode: 'Currency'
      ReceptionDocument.discountamount       as Discountamount,
      @Semantics.amount.currencyCode: 'Currency'
      ReceptionDocument.subtotalamount       as Subtotalamount,
      @Semantics.amount.currencyCode: 'Currency'
      ReceptionDocument.totalamount          as Totalamount,
      ReceptionDocument.currency             as Currency,
      ReceptionDocument.customerid           as Customerid,
      ReceptionDocument.customername         as Customername,
      ReceptionDocument.purchasingdocument   as Purchasingdocument,
      ReceptionDocument.supplierinvoice      as Supplierinvoice,
      ReceptionDocument.fiscalyear           as Fiscalyear,
      ReceptionDocument.accountingdocument   as Accountingdocument,
      ReceptionDocument.accountingfiscalyear as Accountingfiscalyear,
      
      case 
        when ReceptionDocument.documentsri = '01' and 
             ReceptionDocument.supplier is not initial 
        then ReceptionDocument.supplier  
        
        when ReceptionDocument.documentsri = '01' and 
             I_supplierid.BusinessPartner is not initial 
        then I_supplierid.BusinessPartner
        
        when ReceptionDocument.documentsri = '04' and 
             ReceptionDocument.supplier is not initial 
        then ReceptionDocument.supplier
        
        when ReceptionDocument.documentsri = '04' and 
             I_supplierid.BusinessPartner is not initial 
        then I_supplierid.BusinessPartner
        
        when ReceptionDocument.documentsri = '05' and 
             ReceptionDocument.supplier is not initial 
        then ReceptionDocument.supplier
        
        when ReceptionDocument.documentsri = '05' and 
             I_supplierid.BusinessPartner is not initial 
        then I_supplierid.BusinessPartner
        
        else ''
        end                            as Supplier,
      
      case 
        when ReceptionDocument.documentsri = '03' and 
             ReceptionDocument.customer is not initial 
        then ReceptionDocument.customer  
        
        when ReceptionDocument.documentsri = '03' and 
             I_supplierid.BusinessPartner is not initial 
        then I_supplierid.BusinessPartner
        
        when ReceptionDocument.documentsri = '07' and 
             ReceptionDocument.customer is not initial 
        then ReceptionDocument.customer
        
        when ReceptionDocument.documentsri = '07' and 
             I_supplierid.BusinessPartner is not initial 
        then I_supplierid.BusinessPartner
        
        else ''
        end                            as Customer,
        
      ReceptionDocument.creationdate         as Creationdate,
      ReceptionDocument.creationtime         as Creationtime,
      ReceptionDocument.rejectiondate        as Rejectiondate,
      ReceptionDocument.rejectiontime        as Rejectiontime,
      ReceptionDocument.issuedate            as Issuedate,
      ReceptionDocument.issuetime            as Issuetime,
      ReceptionDocument.authorizationdate    as Authorizationdate,
      ReceptionDocument.authorizationtime    as Authorizationtime,
      
      case ReceptionDocument.documentstatus
        when '01'    then 0
        when '02'    then 3
        when '03'    then 1
        when '04'    then 2
        else 0
        end                            as criticality,
      
      case ReceptionDocument.documentstatus
        when '01'    then 'Pending'
        when '02'    then 'Process'
        when '03'    then 'Error'
        when '04'    then 'Canceled'
        else ''
        end                            as Documentstatus,
           
      ReceptionDocument.receptionstatus      as Receptionstatus,
      ReceptionDocument.typesupport          as Typesupport,
      ReceptionDocument.reason               as Reason,
      ReceptionDocument.message              as Message,
      ReceptionDocument.supportdocumenttype  as Supportdocumenttype,
      ReceptionDocument.supportdocument      as Supportdocument,
      ReceptionDocument.supportdocumentdate  as Supportdocumentdate,
      
      @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 } 
      I_Company.CompanyCodeName
      
}
