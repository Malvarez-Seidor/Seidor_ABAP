//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier Data' //- Roow View Interface
define view entity ZCDS_RV_EC_011
  as select from zdt_ec_011
  association to parent ZCDS_RV_DOC_MM_GUIA as _TransferGuides on  $projection.Companycode          = _TransferGuides.Companycode
                                                               and $projection.Materialdocumentyear = _TransferGuides.MaterialDocumentyear
                                                               and $projection.Materialdocument     = _TransferGuides.Materialdocument
                                                               and $projection.Goodsmovementtype    = _TransferGuides.GoodsMovementType
{
  key companycode                    as Companycode,
  key materialdocumentyear           as Materialdocumentyear,
  key materialdocument               as Materialdocument,
  key goodsmovementtype              as Goodsmovementtype,
  key carrierid                      as Carrierid,
      typeid                         as Typeid,
      businessname                   as Businessname,
      carplate                       as Carplate,
      startdate                      as Startdate,
      enddate                        as Enddate,
      _TransferGuides

}
