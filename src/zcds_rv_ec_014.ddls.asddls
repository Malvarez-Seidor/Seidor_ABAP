//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Official Withholding Key' //Roow View Interface
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_014
  as select from zdt_ec_014 as OfficialWithholdingKey
  association [1..1] to I_Extendedwhldgtaxcode     as _ExtendedWhldgTaxCode     on  _ExtendedWhldgTaxCode.WithholdingTaxType = OfficialWithholdingKey.withholdingtaxtype
                                                                                and _ExtendedWhldgTaxCode.WithholdingTaxCode = OfficialWithholdingKey.withholdingtaxcode
                                                                                and _ExtendedWhldgTaxCode.Country            = 'EC'
  association [1..1] to I_ExtendedWhldgTaxCodeText as _ExtendedWhldgTaxCodeText on  _ExtendedWhldgTaxCodeText.WithholdingTaxType = OfficialWithholdingKey.withholdingtaxtype
                                                                                and _ExtendedWhldgTaxCodeText.WithholdingTaxCode = OfficialWithholdingKey.withholdingtaxcode
                                                                                and _ExtendedWhldgTaxCodeText.CountryCode        = 'EC'
                                                                                and _ExtendedWhldgTaxCodeText.Language           = 'S' //$session.system_language
{
  key OfficialWithholdingKey.withholdingtaxtype   as Withholdingtaxtype,
  key OfficialWithholdingKey.withholdingtaxcode   as Withholdingtaxcode,
      OfficialWithholdingKey.officialwhldgtaxcode as Officialwhldgtaxcode,
      OfficialWithholdingKey.withholdingtype      as Withholdingtype,

      _ExtendedWhldgTaxCode,
      _ExtendedWhldgTaxCodeText

}
