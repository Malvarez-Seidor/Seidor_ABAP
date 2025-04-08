@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report Configuration 104' //Roow View Interface
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_019
  as select from zdt_ec_019 as ReportConfiguration

  association [1..1] to I_CompanyCode              as _Company                  on  _Company.CompanyCode = ReportConfiguration.companycode

  association [1..1] to I_Extendedwhldgtaxcode     as _ExtendedWhldgTaxCode     on  _ExtendedWhldgTaxCode.WithholdingTaxType = ReportConfiguration.withholdingtaxtype
                                                                                and _ExtendedWhldgTaxCode.WithholdingTaxCode = ReportConfiguration.withholdingtaxcode
                                                                                and _ExtendedWhldgTaxCode.Country            = 'EC'

  association [1..1] to I_ExtendedWhldgTaxCodeText as _ExtendedWhldgTaxCodeText on  _ExtendedWhldgTaxCodeText.WithholdingTaxType = ReportConfiguration.withholdingtaxtype
                                                                                and _ExtendedWhldgTaxCodeText.WithholdingTaxCode = ReportConfiguration.withholdingtaxcode
                                                                                and _ExtendedWhldgTaxCodeText.CountryCode        = 'EC'
                                                                                and _ExtendedWhldgTaxCodeText.Language           = $session.system_language

{
  key companycode        as CompanyCode,
  key withholdingtaxtype as WithholdingTaxType,
  key withholdingtaxcode as WithholdingTaxCode,
  key codereport         as CodeReport,
      codebase           as CodeBase,
      codewithholding    as CodeWithholding,
      codigoseccion      as CodigoSeccion,
      _Company,
      _ExtendedWhldgTaxCode,
      _ExtendedWhldgTaxCodeText

}
