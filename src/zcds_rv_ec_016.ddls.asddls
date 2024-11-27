@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Withholdings  - Roow View Interface'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZCDS_RV_EC_016
  as select from zdt_ec_016
  association to parent ZCDS_RV_EC_015 as _ElectronicDocuments 
                  on $projection.Companycode            = _ElectronicDocuments.Companycode
                 and $projection.Documentsri            = _ElectronicDocuments.Documentsri
                 and $projection.Accesskey              = _ElectronicDocuments.Accesskey
                 and $projection.Documentsupplier       = _ElectronicDocuments.Documentsupplier
{
  key companycode          as Companycode,
  key documentsri          as Documentsri,
  key accesskey            as Accesskey,
  key documentsupplier     as Documentsupplier,
  key positem              as Positem,
      establishmentsupport as Establishmentsupport,
      emissionpointsupport as Emissionpointsupport,
      sequentialsupport    as Sequentialsupport,
      accesskeysupport     as Accesskeysupport,
      documentsrisupport   as Documentsrisupport,
      officialwhldgtaxcode as Officialwhldgtaxcode,
      codeid               as Codeid,
      @Semantics.amount.currencyCode: 'Currency'
      amountretained       as Amountretained,
      percentageretained   as Percentageretained,
      @Semantics.amount.currencyCode: 'Currency'
      baseretained         as Baseretained,
      currency             as Currency,
      _ElectronicDocuments
}
