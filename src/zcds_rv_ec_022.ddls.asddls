@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchasing Document ATS - Roow View Interface'
define root view entity ZCDS_RV_EC_022
 as select from zdt_ec_022 as PurchasingDocumentATS      
                                                                     
 association [0..1] to I_CompanyCode    as _CompanyCode              on _CompanyCode.CompanyCode      = PurchasingDocumentATS.companycode
                                                                    and _CompanyCode.Language         = $session.system_language
 
 association [0..1] to  I_AccountingDocumentTypeText as _AccountingDocumentTypeText 
            on _AccountingDocumentTypeText.AccountingDocumentType = PurchasingDocumentATS.accountingdocumenttype
           and _AccountingDocumentTypeText.Language               = $session.system_language
 
{   
    
    key PurchasingDocumentATS.companycode                       as Companycode,
    key PurchasingDocumentATS.accountingdocumenttype            as AccountingDocumentType,
    codesri                                                     as Codesri,
    _CompanyCode.CompanyCodeName                                as CompanyCodeName,
    _AccountingDocumentTypeText.AccountingDocumentTypeName      as AccountingDocumentTypeName
    
    
}
