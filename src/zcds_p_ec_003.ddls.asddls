@EndUserText.label: 'Types of Taxes' //  Projection View
//@AccessControl.authorizationCheck: #NOT_REQUIRED
//@Search.searchable: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_003
  as projection on ZCDS_RV_EC_003
{
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CompanyCodeVH', element: 'CompanyCode' },
                      additionalBinding: [{ localElement: 'CompanyCodeName', element: 'CompanyCodeName' } ] } ]
  key Companycode              as CompanyCode,
     @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TAXES', element: 'TaxCode' },
         additionalBinding: [ { localElement: 'TaxCodeName', element: 'TaxCodeName' } ] } ]
      @Search.defaultSearchElement: true
  key Taxcode                  as TaxCode,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Notax                    as Notax,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Tax0                     as Tax0,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Exempttax                as ExemptTax,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_APP', element: 'value_low' }, distinctValues: true } ]
      Tax                      as Tax,
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_TAX', element: 'value_low' }, distinctValues: true } ]
      Taxsupportid             as TaxSupportId,
      Taxsidrate               as TaxsIdRate,
      Taxratepercent           as TaxRatePercent,
      supporttaxcode           as supportTaxCode,
      
       @ObjectModel.text.element: ['TaxCodeName']
      _Taxes.TaxCodeName as TaxCodeName,
            
      @ObjectModel.text.element: ['CompanyCodeName']
      _Company.CompanyCodeName as CompanyCodeName
}
