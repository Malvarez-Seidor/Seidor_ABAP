@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Refunds Details - Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_VC_DET_REEM 
  as select from  I_JournalEntry
  inner join zdt_ec_013 as SupportDetails on SupportDetails.companycode            = I_JournalEntry.CompanyCode
                                         and SupportDetails.fiscalyear             = I_JournalEntry.FiscalYear
                                         and SupportDetails.accountingdocument     = I_JournalEntry.AccountingDocument
                                         and SupportDetails.accountingdocumenttype = I_JournalEntry.AccountingDocumentType
  {
 
     key SupportDetails.companycode            as Companycode,
     key SupportDetails.fiscalyear             as Fiscalyear,
     key SupportDetails.accountingdocument     as Accountingdocument,
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
         SupportDetails.last_changed_by        as Last_changed_by
    
}
