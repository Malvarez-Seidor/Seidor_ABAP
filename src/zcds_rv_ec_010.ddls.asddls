@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Carrier Data' // Roow View Interface'
define view entity ZCDS_RV_EC_010
  as select from zdt_ec_010
  association to parent ZCDS_RV_DOC_SD_GUIA as _TransportGuides on $projection.Companycode          = _TransportGuides.CompanyCode
                                                               and $projection.Deliverydocument     = _TransportGuides.DeliveryDocument
                                                               and $projection.Fiscalyear           = _TransportGuides.FiscalYear
                                                               and $projection.Deliverydocumenttype = _TransportGuides.DeliveryDocumentType
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
