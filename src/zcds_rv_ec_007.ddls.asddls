//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Table of Constants' // - Roow View Interface'
define root view entity ZCDS_RV_EC_007
  as select from zdt_ec_007 as Constants
  association [1..1] to I_CompanyCode as _Company on _Company.CompanyCode = Constants.companycode
{
  key companycode as Companycode,
  key api         as Api,
  key fieldname   as Fieldname,
  key sign        as Sign,
  key options     as Options,
  key sequence    as Sequence,
      low         as Low,
      high        as High,
      _Company
}
