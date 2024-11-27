@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Purchase Orders  - Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZCDS_RV_EC_017
  as select from zdt_ec_017
  association to parent ZCDS_RV_EC_015 as _ElectronicDocuments 
                  on $projection.Companycode            = _ElectronicDocuments.Companycode
                 and $projection.Documentsri            = _ElectronicDocuments.Documentsri
                 and $projection.Accesskey              = _ElectronicDocuments.Accesskey
                 and $projection.Documentsupplier       = _ElectronicDocuments.Documentsupplier
{
  key companycode                 as Companycode,
  key documentsri                 as Documentsri,
  key accesskey                   as Accesskey,
  key documentsupplier            as Documentsupplier,
  key purchasingdocument          as Purchasingdocument,
  key purchasingdocumentitem      as Purchasingdocumentitem,
      materialdocument            as Materialdocument,
      materialdocumentitem        as Materialdocumentitem,
      referencedocumentfiscalyear as Referencedocumentfiscalyear,
      invtrymgmtreferencedocument as Invtrymgmtreferencedocument,
      invtrymgmtrefdocumentitem   as Invtrymgmtrefdocumentitem,
      _ElectronicDocuments
}
