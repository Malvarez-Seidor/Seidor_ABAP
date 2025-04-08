@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Types of Supporting Documents'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define root view entity ZCDS_P_EC_023
  provider contract transactional_query as
projection on ZCDS_RV_EC_023 as SupportingDocuments
{
    key SupportingDocuments.Codesri as CodeSri,
        SupportingDocuments.Description as Description
}
