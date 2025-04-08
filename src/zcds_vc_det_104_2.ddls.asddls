@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Details Report 104' // View Entity Dimension for Connections'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define view entity ZCDS_VC_DET_104_2
  as select from    ZCDS_VC_DET_104 as Report104
      
{ 

  key Report104.CompanyCode                                     as CompanyCode,
  key Report104.FiscalYear                                      as FiscalYear,
  key Report104.FiscalPeriod                                    as FiscalPeriod,
  key Report104.CodigoSeccion                                   as CodigoSeccion,
  key Report104.CodeNeto                                        as CodeNeto,
      Report104.CodeBase                                        as CodeBase,
      Report104.CodeTax                                         as CodeTax,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.TaxBaseAmountInCoCodeCrcy           )      as TaxBaseAmountInCoCodeCrcy,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.TaxAmountInCoCodeCrcy               )      as TaxAmountInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.TaxBaseAmountCredit                 )      as TaxBaseAmountCredit,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.TaxAmountCredit                     )      as TaxAmountCredit,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.WhldgTaxBaseAmtInCoCodeCrcy         )      as WhldgTaxBaseAmtInCoCodeCrcy,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.WhldgTaxAmtInCoCodeCrcy             )      as WhldgTaxAmtInCoCodeCrcy,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.WhldgTaxBaseVentas                  )      as WhldgTaxBaseVentas,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.WhldgTaxAmtVentas                   )      as WhldgTaxAmtVentas,
      
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.BaseRetencion                  )           as BaseRetencion,
     
      @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
      sum( Report104.ValoRetencion                   )          as ValoRetencion,
      
      Report104.CompanyCodeCurrency                             as CompanyCodeCurrency,
      
      Report104.DescriptionBase                                 as DescriptionBase,
      Report104.DescriptionNeto                                 as DescriptionNeto,
      Report104.DescriptionTax                                  as DescriptionTax

}where Report104.SelectionType                           = '3'
    or Report104.SelectionType                           = '4'
    and Report104.CodeBase is not initial
    group by CompanyCode, FiscalYear, FiscalPeriod, CodigoSeccion, 
           CodeNeto, CodeBase, CodeTax, CompanyCodeCurrency,
           DescriptionBase, DescriptionNeto, DescriptionTax
