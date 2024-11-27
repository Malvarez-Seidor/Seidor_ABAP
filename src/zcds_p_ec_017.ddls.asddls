@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Purchase Orders - Projection View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZCDS_P_EC_017
  as projection on ZCDS_RV_EC_017
{
  key Companycode                 as CompanyCode,
  key Documentsri                 as DocumentSri,
  key Accesskey                   as Accesskey,
  key Documentsupplier            as DocumentSupplier,
  key Purchasingdocument          as PurchasingDocument,
  key Purchasingdocumentitem      as PurchasingDocumentItem,
      Materialdocument            as Materialdocument,
      Materialdocumentitem        as MaterialDocumentItem,
      Referencedocumentfiscalyear as ReferenceDocumentFiscalYear,
      Invtrymgmtreferencedocument as InvtryMgmtReferenceDocument,
      Invtrymgmtrefdocumentitem   as InvtryMgmtRefDocumentItem,

      _ElectronicDocuments : redirected to parent ZCDS_P_EC_015
}
