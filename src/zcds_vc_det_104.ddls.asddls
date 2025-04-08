@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details 104' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_VC_DET_104
  as select from    zdt_ec_020 as Reporte104
    
    inner join I_JournalEntry               as I_JournalEntry 
            on I_JournalEntry.CompanyCode                        = Reporte104.companycode
//           and I_JournalEntry.FiscalYear                         = I_Withholdingtaxitem.FiscalYear
           and I_JournalEntry.AccountingDocumentType             = Reporte104.value
           and I_JournalEntry.IsReversal is initial    
           and I_JournalEntry.IsReversed is initial    
    
    left outer join ZCDS_P_104_IMP as I_OperationalAcctgDocTaxItem 
            on I_OperationalAcctgDocTaxItem.CompanyCode          = I_JournalEntry.CompanyCode
           and I_OperationalAcctgDocTaxItem.FiscalYear           = I_JournalEntry.FiscalYear
           and I_OperationalAcctgDocTaxItem.AccountingDocument   = I_JournalEntry.AccountingDocument
           and I_OperationalAcctgDocTaxItem.CodeBase             = Reporte104.codebase
           and I_OperationalAcctgDocTaxItem.CodeNet              = Reporte104.codenet
           and I_OperationalAcctgDocTaxItem.CodeTax              = Reporte104.codetax
    
    left outer join ZCDS_P_104_RET         as I_Withholdingtaxitem
            on I_Withholdingtaxitem.CompanyCode                  = I_JournalEntry.CompanyCode
           and I_Withholdingtaxitem.FiscalYear                   = I_JournalEntry.FiscalYear
           and I_Withholdingtaxitem.AccountingDocument           = I_JournalEntry.AccountingDocument
//           and I_Withholdingtaxitem.WithholdingTaxType           = Reporte104.withholdingtaxtype
//           and I_Withholdingtaxitem.WithholdingTaxCode           = Retenciones.value
           and I_Withholdingtaxitem.codeBase                     = Reporte104.codebase
           and I_Withholdingtaxitem.CodeNet                      = Reporte104.codenet
           and I_Withholdingtaxitem.CodeTax                      = Reporte104.codetax
           
    left outer join I_JournalEntryItem           as I_JournalEntryItem  // Retencion Asociada a la Venta
            on I_JournalEntryItem.InvoiceReferenceFiscalYear     = I_JournalEntry.FiscalYear
           and I_JournalEntryItem.CompanyCode                    = I_JournalEntry.CompanyCode
           and I_JournalEntryItem.InvoiceReference               = I_JournalEntry.AccountingDocument
           and I_JournalEntryItem.InvoiceReferenceFiscalYear     = I_JournalEntry.FiscalYear
           and I_JournalEntryItem.AccountingDocumentType        <> I_JournalEntry.AccountingDocumentType
           and I_JournalEntryItem.Customer                      is not initial
           and I_JournalEntryItem.Ledger                         = '0L'
           and I_JournalEntryItem.IsReversal is initial    
           and I_JournalEntryItem.IsReversed is initial 
          
    left outer join I_JournalEntry               as I_Withholding  // Documento Contable de la Retencion 
            on I_Withholding.FiscalYear                          = I_JournalEntryItem.FiscalYear
           and I_Withholding.CompanyCode                         = I_JournalEntryItem.CompanyCode
           and I_Withholding.AccountingDocument                  = I_JournalEntryItem.AccountingDocument
           and I_Withholding.IsReversal is initial    
           and I_Withholding.IsReversed is initial
    
    left outer join ZCDS_P_104_RET         as I_WithholdingVentas   
            on I_WithholdingVentas.CompanyCode                  = I_Withholding.CompanyCode
           and I_WithholdingVentas.FiscalYear                   = I_Withholding.FiscalYear
           and I_WithholdingVentas.AccountingDocument           = I_Withholding.AccountingDocument
//           and I_Withholdingtaxitem.WithholdingTaxType = Reporte104.withholdingtaxtype
           and I_WithholdingVentas.codeBase                     = Reporte104.codebase
                                                                     
    left outer join zdt_ec_018                           as _TextosBase    
            on _TextosBase.code                              = Reporte104.codebase
           and _TextosBase.companycode                       = Reporte104.companycode
    left outer join zdt_ec_018                           as _TextosNeto    
            on _TextosNeto.code                              = Reporte104.codenet
           and _TextosNeto.companycode                       = Reporte104.companycode
    left outer join zdt_ec_018                           as _TextosTax     
            on _TextosTax.code                               = Reporte104.codetax
           and _TextosTax.companycode                        = Reporte104.companycode

{
  
  key I_JournalEntry.CompanyCode                                     as CompanyCode,
  key I_JournalEntry.FiscalYear                                      as FiscalYear,
  key I_JournalEntry.AccountingDocument                              as AccountingDocument,
  key I_JournalEntry.AccountingDocumentType                          as AccountingDocumentType,
  key cast( I_JournalEntry.FiscalPeriod  as fins_fiscalperiod preserving type )       as FiscalPeriod,
      Reporte104.selectiontype                                       as SelectionType,   
      Reporte104.codigoseccion                                       as CodigoSeccion,
      Reporte104.codenet                                             as CodeNeto,
      Reporte104.codebase                                            as CodeBase,
      Reporte104.codetax                                             as CodeTax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_OperationalAcctgDocTaxItem.TaxBaseAmountInCoCodeCrcy )  as TaxBaseAmountInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_OperationalAcctgDocTaxItem.TaxAmountInCoCodeCrcy )      as TaxAmountInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_OperationalAcctgDocTaxItem.TaxBaseAmountCredit )        as TaxBaseAmountCredit,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_OperationalAcctgDocTaxItem.TaxAmountCredit )            as TaxAmountCredit,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_Withholdingtaxitem.WhldgTaxBaseAmtInCoCodeCrcy        ) as WhldgTaxBaseAmtInCoCodeCrcy,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_Withholdingtaxitem.WhldgTaxAmtInCoCodeCrcy            ) as WhldgTaxAmtInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_WithholdingVentas.WhldgTaxBaseAmtInCoCodeCrcy         ) as WhldgTaxBaseVentas,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( I_WithholdingVentas.WhldgTaxAmtInCoCodeCrcy             ) as WhldgTaxAmtVentas,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      case
        when I_WithholdingVentas.BaseRetencion is not initial
        then I_WithholdingVentas.BaseRetencion
        when I_Withholdingtaxitem.BaseRetencion is not initial
        then I_Withholdingtaxitem.BaseRetencion 
        else I_WithholdingVentas.BaseRetencion  
        end as BaseRetencion,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      case
        when I_WithholdingVentas.ValoRetencion is not initial
        then I_WithholdingVentas.ValoRetencion
        when I_Withholdingtaxitem.ValoRetencion is not initial
        then I_Withholdingtaxitem.ValoRetencion
        else I_WithholdingVentas.ValoRetencion
      end as ValoRetencion,
      
      I_JournalEntry.CompanyCodeCurrency                             as CompanyCodeCurrency,
      _TextosBase.description                                        as DescriptionBase,
      _TextosNeto.description                                        as DescriptionNeto,
      _TextosTax.description                                         as DescriptionTax

} where Reporte104.codebase                 is not initial
    and ( Reporte104.selectiontype                           = '3'
       or Reporte104.selectiontype                           = '4' )
  group by I_JournalEntry.CompanyCode, 
           I_JournalEntry.FiscalYear,
           I_JournalEntry.AccountingDocument,
           I_JournalEntry.AccountingDocumentType, 
           I_JournalEntry.FiscalPeriod,
           Reporte104.selectiontype,
           Reporte104.codigoseccion,
           Reporte104.codenet,
           Reporte104.codebase,
           Reporte104.codetax,
           I_JournalEntry. CompanyCodeCurrency,
           I_WithholdingVentas.BaseRetencion,
           I_Withholdingtaxitem.BaseRetencion,
           I_WithholdingVentas.ValoRetencion, 
           I_Withholdingtaxitem.ValoRetencion,
           _TextosBase.description,
           _TextosNeto.description,
           _TextosTax.description
