@EndUserText.label: 'SRI Countries - Projection View'
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_009 
 as projection on ZCDS_RV_EC_009
{

      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CountryText', element: 'Country' } } ]
  key Country         as Country,
      Countrysri      as CountrySri,
      Taxhavencountry as TaxHavenCountry,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Pais_conv    as Pais_Conv,

      @ObjectModel.text.element: ['CountryName']
      _Country.CountryName       as CountryName 
  

}
