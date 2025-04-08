@AbapCatalog.sqlViewName: 'ZCDS_V_ATS_CON'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Record Count ATS'
@Metadata.ignorePropagatedAnnotations: true
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK", "KEY_CHECK" ]  } */
define view ZCDS_P_ATS_CONT
  as select from ZCDS_P_ATS  as TransactionalAnnex
      
{

  key TransactionalAnnex.CompanyCode                                                as CompanyCode,

  key TransactionalAnnex.FiscalYear                                                 as FiscalYear,
  
  key TransactionalAnnex.FiscalPeriod                                               as FiscalPeriod,
     
     TransactionalAnnex.TypeProces                                                  as TypeProces,
     
     count( distinct TransactionalAnnex.AccountingDocument )                        as RecordCount
  
} group by TransactionalAnnex.CompanyCode, TransactionalAnnex.FiscalYear, 
          TransactionalAnnex.FiscalPeriod, TransactionalAnnex.TypeProces
