//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Electronic Documents' // Roow View Interface
define root view entity ZCDS_RV_EC_001
  as select from zdt_ec_001 as ElectronicDocuments
  association [1..1] to I_CompanyCode as _Company on _Company.CompanyCode = ElectronicDocuments.companycode
  
{
  key companycode               as Companycode,
  key documenttype              as Documenttype,
  key documentsri               as Documentsri,
  key sequence                  as Sequence, 
      export                    as Export,
      refunds                   as Refunds,
      reason                    as Reason,
      _Company
}
