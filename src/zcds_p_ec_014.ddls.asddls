//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Official Withholding Key' // Projection View
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZCDS_P_EC_014 
 as projection on ZCDS_RV_EC_014
{   

    @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_ExtendedWhldgTaxCodeText', element: 'WithholdingTaxType' },
                     additionalBinding: [ { localElement: 'WithholdingTaxCode', element: 'WithholdingTaxCode' },
                                          { localElement: 'WhldgTaxReferenceText', element: 'WhldgTaxCodeName' } ] } ]
    key Withholdingtaxtype as WithholdingTaxType,
    
    @Consumption.valueHelpDefinition: [ { entity:  { name: 'I_ExtendedWhldgTaxCodeText', element: 'WithholdingTaxCode' },
                     additionalBinding: [ { localElement: 'WithholdingTaxType', element: 'WithholdingTaxType' },
                                          { localElement: 'WhldgTaxReferenceText', element: 'WhldgTaxCodeName' } ] } ]
    key Withholdingtaxcode as WithholdingTaxCode,
    
    Officialwhldgtaxcode   as OfficialWhldgTaxCode,
    
    @Consumption.valueHelpDefinition: [ { entity: { name: 'ZSH_TYPE_WIT', element: 'value_low' }, distinctValues: true } ]
    Withholdingtype        as WithholdingType,
    
    @Semantics.text : true
    _ExtendedWhldgTaxCode.WithholdingTaxPercent as WithholdingTaxPercent,
    
    @Semantics.text : true
    _ExtendedWhldgTaxCodeText.WhldgTaxCodeName as WhldgTaxReferenceText
    
}
