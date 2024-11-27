@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Download Electronic Withholdings - Projection View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZCDS_P_EC_016
  as projection on ZCDS_RV_EC_016
{
  key Companycode          as CompanyCode,
  key Documentsri          as DocumentSri,
  key Accesskey            as Accesskey,
  key Documentsupplier     as DocumentSupplier,
  key Positem              as PosItem,
      Establishmentsupport as EstablishmentSupport,
      Emissionpointsupport as EmissionpointSupport,
      Sequentialsupport    as SequentialSupport,
      Accesskeysupport     as AccesskeySupport,
      Documentsrisupport   as DocumentSriSupport,
      Officialwhldgtaxcode as OfficialWhldgTaxCode,
      Codeid               as CodeId,
      @Semantics.amount.currencyCode: 'Currency'
      Amountretained       as AmountRetained,
      Percentageretained   as PercentageRetained,
      @Semantics.amount.currencyCode: 'Currency'
      Baseretained         as BaseRetained,
      Currency             as Currency,

      _ElectronicDocuments : redirected to parent ZCDS_P_EC_015
}
