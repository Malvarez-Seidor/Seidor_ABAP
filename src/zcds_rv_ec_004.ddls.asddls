//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Types of Identification' //  - Roow View Interface

define root view entity ZCDS_RV_EC_004 
as select from zdt_ec_004 as TypeIdentification
association  [1..1] to I_BusPartTaxTypeText as _BusPartTax  on _BusPartTax.BPTaxType = TypeIdentification.bptaxtype
                                                           and _BusPartTax.Language  = $session.system_language
                                                           
association [1..1] to I_CompanyCode as _Company             on _Company.CompanyCode  = TypeIdentification.companycode

{
    key companycode   as Companycode,
    key bptaxtype     as BPTaxType,
    key typedoccument as Typedoccument,
        typedi        as Typedi,
    _Company,
    _BusPartTax

}
