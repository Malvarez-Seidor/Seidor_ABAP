//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sequential Administrator' //- Roow View Interface'
define root view entity ZCDS_RV_EC_002
  as select from zdt_ec_002 as SeqAdministrator
  association [1..1] to I_CompanyCode as _Company on _Company.CompanyCode = SeqAdministrator.companycode
{
  key companycode   as Companycode,
  key documentsri   as Documentsri,
  key establishment as Establishment,
  key emissionpoint as Emissionpoint,
      objet         as Objet,
      address       as Address,
      sequential    as Sequential,
      _Company
}
