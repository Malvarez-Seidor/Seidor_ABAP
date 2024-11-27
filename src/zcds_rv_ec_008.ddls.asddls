//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sequential Maintenance' //Roow View Interface'
define root view entity ZCDS_RV_EC_008
  as select from zdt_ec_008 as SeqMaintenance
  association [1..1] to I_CompanyCode                as _Company            
                     on _Company.CompanyCode = SeqMaintenance.companycode
  association [1..1] to ZCDS_P_EC_002                as _SeqAdministrator   
                     on _SeqAdministrator.CompanyCode = SeqMaintenance.companycode
                    and _SeqAdministrator.DocumentSri = SeqMaintenance.documentsri
                    and _SeqAdministrator.Establishment = SeqMaintenance.establishment
                    and _SeqAdministrator.EmissionPoint = SeqMaintenance.emissionpoint
  association [1..1] to I_AccountingDocumentTypeText as _AccountingDocument 
                     on _AccountingDocument.AccountingDocumentType = SeqMaintenance.accountingdocumenttype
                    and _AccountingDocument.Language = $session.system_language 
  association [1..1] to I_BillingDocumentTypeText as _BillingDocumentType 
                     on _BillingDocumentType.BillingDocumentType  = SeqMaintenance.billingdocumenttype
                    and _BillingDocumentType.Language = $session.system_language
  association [1..1] to I_DeliveryDocumentTypeText as _DeliveryDocumentType 
                     on _DeliveryDocumentType.DeliveryDocumentType  = SeqMaintenance.deliverydocumenttype
                    and _DeliveryDocumentType.Language = $session.system_language
  association [1..1] to I_SalesOrganizationText as _SalesOrganization 
                     on _SalesOrganization.SalesOrganization  = SeqMaintenance.salesorganization
                    and _SalesOrganization.Language = $session.system_language
  association [1..1] to I_Plant as _Plant 
                     on _Plant.SalesOrganization  = SeqMaintenance.salesorganization
                     and _Plant.Plant  = SeqMaintenance.plant
  association [1..1] to I_User as _Users 
                     on _Users.UserID  = SeqMaintenance.users
  association [1..1] to I_StorageLocation    as _StorageLocation   on _StorageLocation.StorageLocation = SeqMaintenance.storagelocation
                                                                  and _StorageLocation.Plant  = SeqMaintenance.plant                                              
  association [1..1] to I_GoodsMovementTypeT as _GoodsMovementType on _GoodsMovementType.GoodsMovementType = SeqMaintenance.goodsmovementtype
                                                                  and _GoodsMovementType.Language          = $session.system_language
                    
{
  key companycode            as Companycode,
  key documentsri            as Documentsri,
  key establishment          as Establishment,
  key emissionpoint          as Emissionpoint,
  key users                  as Users,
  key sequence               as Sequence,
      accountingdocumenttype as Accountingdocumenttype,
      billingdocumenttype    as Billingdocumenttype,
      deliverydocumenttype   as Deliverydocumenttype,
      goodsmovementtype      as Goodsmovementtype, 
      salesorganization      as Salesorganization,
      plant                  as Plant,
      storagelocation        as Storagelocation,
      _Company,
      _SeqAdministrator,
      _AccountingDocument,
      _BillingDocumentType,
      _DeliveryDocumentType,
      _SalesOrganization,
      _Plant,
      _Users,
      _StorageLocation,
      _GoodsMovementType
}
