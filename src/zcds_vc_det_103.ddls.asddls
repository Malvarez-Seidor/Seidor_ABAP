@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details 103' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

//@Analytics.dataCategory: #DIMENSION
@Analytics.internalName: #LOCAL
@ObjectModel.representativeKey: 'AccountingDocument'

define view entity ZCDS_VC_DET_103
  as select from    zdt_ec_019 as Reporte103
  
    inner join I_Withholdingtaxitem   as I_Withholdingtaxitem         on I_Withholdingtaxitem.CompanyCode        = Reporte103.companycode
                                                                     and I_Withholdingtaxitem.WithholdingTaxType = Reporte103.withholdingtaxtype
                                                                     and I_Withholdingtaxitem.WithholdingTaxCode = Reporte103.withholdingtaxcode
                                                                     
    inner join I_JournalEntry                       as I_JournalEntry on I_JournalEntry.CompanyCode              = I_Withholdingtaxitem.CompanyCode
                                                                     and I_JournalEntry.FiscalYear               = I_Withholdingtaxitem.FiscalYear
                                                                     and I_JournalEntry.AccountingDocument       = I_Withholdingtaxitem.AccountingDocument
                                                                     and I_JournalEntry.IsReversal is initial    
                                                                     and I_JournalEntry.IsReversed is initial
    
    inner join zdt_ec_001                           as ElectronicDocuments
            on ElectronicDocuments.companycode                         = I_JournalEntry.CompanyCode
           and ElectronicDocuments.documenttype                        = I_JournalEntry.AccountingDocumentType
           and ( ElectronicDocuments.documentsri                       = '03'
            or   ElectronicDocuments.documentsri                       = '07' )
                                                                
    left outer join I_WithholdingTaxCode as I_WithholdingTaxCode      on I_WithholdingTaxCode.WithholdingTaxCode = I_Withholdingtaxitem.WithholdingTaxCode
                                                                     and I_WithholdingTaxCode.Country            = 'EC'

    left outer join I_Extendedwhldgtaxcode as I_ExtendedWhldgTaxCode  on I_ExtendedWhldgTaxCode.WithholdingTaxType = I_Withholdingtaxitem.WithholdingTaxType
                                                                     and I_ExtendedWhldgTaxCode.WithholdingTaxCode = I_Withholdingtaxitem.WithholdingTaxCode
                                                                     and I_ExtendedWhldgTaxCode.Country            = I_WithholdingTaxCode.Country
                                                                     
    inner join zdt_ec_018                           as _Textos        on _Textos.code                              = Reporte103.codereport
                                                                     and _Textos.companycode                       = Reporte103.companycode

{
  
  key I_JournalEntry.CompanyCode                                                  as CompanyCode,
  key I_JournalEntry.FiscalYear                                                   as FiscalYear,
  key I_JournalEntry.AccountingDocument                                           as AccountingDocument,
  key I_JournalEntry.AccountingDocumentType                                       as AccountingDocumentType,
  key cast( I_JournalEntry.FiscalPeriod  as fins_fiscalperiod preserving type )   as FiscalPeriod,
//  key I_Withholdingtaxitem.WithholdingTaxType       as WithholdingTaxType,
//  key I_Withholdingtaxitem.WithholdingTaxCode       as WithholdingTaxCode,
        
      Reporte103.codigoseccion                                                    as CodigoSeccion,
      Reporte103.codereport                                                       as CodeReport,
      Reporte103.codebase                                                         as CodeBase,
      Reporte103.codewithholding                                                  as CodeWithholding,
      
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( abs( I_Withholdingtaxitem.WhldgTaxBaseAmtInCoCodeCrcy ) )              as WhldgTaxBaseAmtInCoCodeCrcy,
     
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( abs( I_Withholdingtaxitem.WhldgTaxAmtInCoCodeCrcy     ) )              as WhldgTaxAmtInCoCodeCrcy,
      
      I_JournalEntry.CompanyCodeCurrency                                          as CompanyCodeCurrency,
      
      _Textos.description                                                         as Description

} group by I_JournalEntry.CompanyCode, 
           I_JournalEntry.FiscalYear, 
           I_JournalEntry.AccountingDocument,
           I_JournalEntry.AccountingDocumentType, 
           I_JournalEntry.FiscalPeriod, 
           I_JournalEntry.CompanyCodeCurrency, 
           Reporte103.codigoseccion,
           Reporte103.codereport, 
           Reporte103.codebase,
           Reporte103.codewithholding,
           _Textos.description
