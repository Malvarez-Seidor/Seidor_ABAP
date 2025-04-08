@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Types of Supporting Documents'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZCDS_RV_EC_023 
  as select from zdt_ec_023 as SupportingDocuments
{
    key SupportingDocuments.codesri as Codesri,
    SupportingDocuments.description as Description
}
