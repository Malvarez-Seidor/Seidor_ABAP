@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 104 withholdings'
@Metadata.ignorePropagatedAnnotations: true

@Metadata.allowExtensions: true

@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_P_104_RET 
as select from zdt_ec_020 as Retenciones
    
    inner join zdt_ec_020                   as Reporte104      
            on Reporte104.companycode                             = Retenciones.companycode
           and Reporte104.codebase                                = Retenciones.codebase
           and ( Reporte104.selectiontype                           = '3'
            or   Reporte104.selectiontype                           = '4' )
    
    inner join I_JournalEntry               as I_JournalEntry 
            on I_JournalEntry.CompanyCode                        = Retenciones.companycode
//           and I_JournalEntry.FiscalYear                         = I_Withholdingtaxitem.FiscalYear
           and I_JournalEntry.AccountingDocumentType             = Reporte104.value
           and I_JournalEntry.IsReversal is initial    
           and I_JournalEntry.IsReversed is initial
    
    inner join I_Withholdingtaxitem         as I_Withholdingtaxitem   
            on I_Withholdingtaxitem.CompanyCode                  = I_JournalEntry.CompanyCode
           and I_Withholdingtaxitem.FiscalYear                   = I_JournalEntry.FiscalYear
           and I_Withholdingtaxitem.AccountingDocument           = I_JournalEntry.AccountingDocument
//           and I_Withholdingtaxitem.WithholdingTaxType         = Reporte104.withholdingtaxtype
           and I_Withholdingtaxitem.WithholdingTaxCode           = Retenciones.value
{
    
    key I_Withholdingtaxitem.CompanyCode              as    CompanyCode,
    key I_Withholdingtaxitem.FiscalYear               as    FiscalYear,
    key I_Withholdingtaxitem.AccountingDocument       as    AccountingDocument,
    key I_Withholdingtaxitem.AccountingDocumentItem   as    AccountingDocumentItem,
        
        Retenciones.codebase                          as     codeBase,
        Retenciones.codenet                           as     CodeNet,
        Retenciones.codetax                           as     CodeTax,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        abs( I_Withholdingtaxitem.WhldgTaxBaseAmtInCoCodeCrcy ) as WhldgTaxBaseAmtInCoCodeCrcy,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        abs( I_Withholdingtaxitem.WhldgTaxAmtInCoCodeCrcy     ) as WhldgTaxAmtInCoCodeCrcy,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        abs( I_Withholdingtaxitem.WhldgTaxBaseAmtInCoCodeCrcy ) as BaseRetencion,
        
         @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        abs( I_Withholdingtaxitem.WhldgTaxAmtInCoCodeCrcy     ) as ValoRetencion,
        
        I_JournalEntry.CompanyCodeCurrency          as CompanyCodeCurrency
    
} where Retenciones.selectiontype                           = '2'
