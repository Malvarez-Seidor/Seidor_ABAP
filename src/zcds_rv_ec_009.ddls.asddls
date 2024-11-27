//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SRI Countries - Roow View Interface'
define root view entity ZCDS_RV_EC_009
as select from zdt_ec_009 as  SriCountries
association [1..1] to I_CountryText                    as _Country  on  _Country.Country      = SriCountries.country
                                                                   and  _Country.Language     = $session.system_language
{

    key country     as Country,
    countrysri      as Countrysri,
    taxhavencountry as Taxhavencountry,
    pais_conv       as Pais_conv,
    _Country
    
}
