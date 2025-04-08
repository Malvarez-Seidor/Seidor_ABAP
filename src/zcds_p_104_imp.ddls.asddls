@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report 104 Taxes - Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'
define view entity ZCDS_P_104_IMP 
  as select from zdt_ec_020 as Impuestos 
 
    inner join zdt_ec_020                   as Reporte104      
            on Reporte104.companycode                            = Impuestos.companycode
           and Reporte104.codebase                               = Impuestos.codebase
           and ( Reporte104.selectiontype                        = '3'
            or   Reporte104.selectiontype                        = '4' )
    
    inner join I_JournalEntry               as I_JournalEntry 
            on I_JournalEntry.CompanyCode                        = Impuestos.companycode
           and I_JournalEntry.AccountingDocumentType             = Reporte104.value
           and I_JournalEntry.IsReversal is initial    
           and I_JournalEntry.IsReversed is initial
    
    inner join I_OperationalAcctgDocTaxItem as I_OperationalAcctgDocTaxItem 
            on I_OperationalAcctgDocTaxItem.CompanyCode          = I_JournalEntry.CompanyCode
           and I_OperationalAcctgDocTaxItem.FiscalYear           = I_JournalEntry.FiscalYear
           and I_OperationalAcctgDocTaxItem.AccountingDocument   = I_JournalEntry.AccountingDocument
           and I_OperationalAcctgDocTaxItem.TaxCode              = Impuestos.value
{
    
    key I_OperationalAcctgDocTaxItem.CompanyCode                as    CompanyCode,
    key I_OperationalAcctgDocTaxItem.FiscalYear                 as    FiscalYear,
    key I_OperationalAcctgDocTaxItem.AccountingDocument         as    AccountingDocument,
    key I_OperationalAcctgDocTaxItem.TaxItem                    as    TaxItem,
        
        Impuestos.codebase                                      as     CodeBase,
        Impuestos.codenet                                       as     CodeNet,
        Impuestos.codetax                                       as     CodeTax,
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        case
        when Reporte104.selectiontype = '3' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'H'
        then abs( I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy )
        when Reporte104.selectiontype = '4' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'S'
        then I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy
        else  cast( '0.00' as abap.curr( 16, 2 ) )
        end as TaxBaseAmountInCoCodeCrcy,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        case
        when Reporte104.selectiontype = '3' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'H'
        then abs( I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy )
        when Reporte104.selectiontype = '4' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'S'
        then I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy
        else  cast( '0.00' as abap.curr( 16, 2 ) )
        end as TaxAmountInCoCodeCrcy,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        case
        when Reporte104.selectiontype = '3' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'S'
        then I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy * ( - 1 )
        when Reporte104.selectiontype = '4' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'H'
        then I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy
        else  cast( '0.00' as abap.curr( 16, 2 ) )
        end as TaxBaseAmountCredit,
        
        @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
        case
        when Reporte104.selectiontype = '3' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'S'
        then I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy * ( - 1 )
        when Reporte104.selectiontype = '4' 
         and I_OperationalAcctgDocTaxItem.DebitCreditCode = 'H'
        then I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy
        else  cast( '0.00' as abap.curr( 16, 2 ) )
        end as TaxAmountCredit,
        
        I_JournalEntry.CompanyCodeCurrency        as CompanyCodeCurrency
    
} where Impuestos.selectiontype                           = '1'
 