@EndUserText.label: 'SRI Countries - Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_009
  provider contract transactional_query 
 as projection on ZCDS_RV_EC_009
{

      @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_CountryText', element: 'Country' } } ]
  key Country         as Country,
      Countrysri      as CountrySri,
      Taxhavencountry as TaxHavenCountry,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Taxagreement    as TaxAgreement,
      
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_REG' , element: 'value_low' }, distinctValues: true } ]
      Taxregime       as TaxRegime,

//      @ObjectModel.text.element: ['CountryName']
      _Country.CountryName       as CountryName 
      

}
