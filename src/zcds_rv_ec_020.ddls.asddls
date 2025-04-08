@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report Configuration 104' // Roow View Interface
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_020
  as select from zdt_ec_020 as ReportConfiguration

  association [1..1] to I_CompanyCode as _Company on _Company.CompanyCode = ReportConfiguration.companycode

{

  key companycode   as CompanyCode,
  key selectiontype as SelectionType,
  key value         as Value,
  key codebase      as CodeBase,
  key codenet       as CodeNet,
  key codetax       as CodeTax,
      codigoseccion as CodigoSeccion,
      _Company
      
}
