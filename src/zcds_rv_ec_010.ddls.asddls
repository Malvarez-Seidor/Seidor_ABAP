//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier Data' // Roow View Interface'
define view entity ZCDS_RV_EC_010
  as select from zdt_ec_010
  association to parent ZCDS_RV_DOC_SD_GUIA as _TransportGuides on $projection.Companycode          = _TransportGuides.Companycode
                                                               and $projection.Deliverydocument     = _TransportGuides.Deliverydocument
                                                               and $projection.Fiscalyear           = _TransportGuides.Fiscalyear
                                                               and $projection.Deliverydocumenttype = _TransportGuides.Deliverydocumenttype
{
  key companycode          as Companycode,
  key fiscalyear           as Fiscalyear,
  key deliverydocument     as Deliverydocument,
  key deliverydocumenttype as Deliverydocumenttype,
  key carrierid            as Carrierid,
      typeid               as Typeid,
      businessname         as Businessname,
      carplate             as Carplate,
      startdate            as Startdate,
      enddate              as Enddate,
      _TransportGuides
}
