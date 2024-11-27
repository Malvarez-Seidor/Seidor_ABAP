//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Types of Taxes - Roow View Interface'
define root view entity ZCDS_RV_EC_003
  as select from zdt_ec_003 as TypesTaxes
  association [1..1] to ZSH_TAXES     as _Taxes   on  _Taxes.TaxCode                 = TypesTaxes.taxcode
                                                  and _Taxes.Language                = $session.system_language
                                                  and _Taxes.TaxCalculationProcedure = '0TXEC'
  association [1..1] to I_CompanyCode as _Company on  _Company.CompanyCode = TypesTaxes.companycode
{
  key companycode    as Companycode,
  key taxcode        as Taxcode,
      notax          as Notax,
      tax0           as Tax0,
      exempttax      as Exempttax,
      tax            as Tax,
      taxsupportid   as Taxsupportid,
      taxsidrate     as Taxsidrate,
      taxratepercent as Taxratepercent,
      supporttaxcode as supporttaxcode,
      _Company,
      _Taxes
}
