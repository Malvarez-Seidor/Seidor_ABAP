@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Code Reports' //Roow View Interface
define root view entity ZCDS_RV_EC_018 
as select from zdt_ec_018 as CodeReport
association [1..1] to I_CompanyCode as _Company on _Company.CompanyCode = CodeReport.companycode
{
    key companycode as CompanyCode,
    
    key report      as Report,
    
    key typecode    as Typecode,
    
    key code        as Code,
    
    description     as Description,
    
    _Company
}
