@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report ATS - Roow View Interface'
@Metadata.allowExtensions: true
/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZCDS_RV_EC_021
 as select from ZCDS_P_ATS_CONT as PurchaseQuantity
 
 left outer join zdt_ec_021 as SimplifiedTransactionalAnnex           on SimplifiedTransactionalAnnex.companycode      = PurchaseQuantity.CompanyCode
                                                                     and SimplifiedTransactionalAnnex.fiscalyear       = PurchaseQuantity.FiscalYear
                                                                     and SimplifiedTransactionalAnnex.monat            = PurchaseQuantity.FiscalPeriod
 
 left outer join ZCDS_P_ATS_CONT as SalesQuantity                     on SalesQuantity.CompanyCode                     = PurchaseQuantity.CompanyCode
                                                                     and SalesQuantity.FiscalYear                      = PurchaseQuantity.FiscalYear
                                                                     and SalesQuantity.FiscalPeriod                    = PurchaseQuantity.FiscalPeriod
                                                                     and SalesQuantity.TypeProces                      = 'Venta'
 
 left outer join ZCDS_P_ATS_CONT as ExportQuantity                    on ExportQuantity.CompanyCode                    = PurchaseQuantity.CompanyCode
                                                                     and ExportQuantity.FiscalYear                     = PurchaseQuantity.FiscalYear
                                                                     and ExportQuantity.FiscalPeriod                   = PurchaseQuantity.FiscalPeriod
                                                                     and ExportQuantity.TypeProces                     = 'Exportacion'
                                                                    
 left outer join ZCDS_P_ATS_CONT as CancelQuantity                    on CancelQuantity.CompanyCode                    = PurchaseQuantity.CompanyCode
                                                                     and CancelQuantity.FiscalYear                     = PurchaseQuantity.FiscalYear
                                                                     and CancelQuantity.FiscalPeriod                   = PurchaseQuantity.FiscalPeriod
                                                                     and CancelQuantity.TypeProces                     = 'Anulado'
                                                                     
 association [0..*] to ZCDS_VED_REP_ATS as _DetailsTransactionalAnnex  on _DetailsTransactionalAnnex.CompanyCode        = PurchaseQuantity.CompanyCode
                                                                      and _DetailsTransactionalAnnex.FiscalYear         = PurchaseQuantity.FiscalYear
                                                                      and _DetailsTransactionalAnnex.FiscalPeriod       = PurchaseQuantity.FiscalPeriod
                                                                     
 association [0..1] to I_CompanyCode    as _CompanyCode              on _CompanyCode.CompanyCode                        = PurchaseQuantity.CompanyCode
                                                                    and _CompanyCode.Language                           = $session.system_language
                                                                    
 association [0..1] to ZSH_STATUS_ATS   as _Status                   on _Status.value_low                               = SimplifiedTransactionalAnnex.status
//                                                                    and _Status.language                                = $session.system_language

 association [0..1] to ZSH_STATUS_ATS   as _StatusDiference          on _StatusDiference.value_low                      = 'DIFFERENCE'
//                                                                    and _StatusDiference.language                       = $session.system_language

 association [0..1] to ZSH_STATUS_ATS   as _StatusPeding             on _StatusPeding.value_low                         = 'PENDING'
//                                                                    and _StatusPeding.language                          = $session.system_language
 
{   
    key PurchaseQuantity.CompanyCode                       as Companycode,
    key PurchaseQuantity.FiscalYear                        as FiscalYear,
    key PurchaseQuantity.FiscalPeriod                      as Monat,
    
    case
      when SimplifiedTransactionalAnnex.purchases is not initial 
      then SimplifiedTransactionalAnnex.purchases
      else PurchaseQuantity.RecordCount 
    end                             as Purchases,
    
    case
      when SimplifiedTransactionalAnnex.sales is not initial 
      then SimplifiedTransactionalAnnex.sales
      else SalesQuantity.RecordCount 
    end                             as Sales,
     
    case
      when SimplifiedTransactionalAnnex.export is not initial 
      then SimplifiedTransactionalAnnex.export
      else ExportQuantity.RecordCount 
    end                             as Export,
     
    case
      when SimplifiedTransactionalAnnex.cancel is not initial 
      then SimplifiedTransactionalAnnex.cancel
      else PurchaseQuantity.RecordCount 
    end                             as Cancel,
    
    SimplifiedTransactionalAnnex.status as StatusXML,
    
    case
//      when ( ( SalesQuantity.RecordCount  <> SimplifiedTransactionalAnnex.sales      //diferencia en Ventas
//       and   SalesQuantity.RecordCount is not initial )
//        or ( PurchaseQuantity.RecordCount <> SimplifiedTransactionalAnnex.purchases  //diferencia en Compras
//       and   PurchaseQuantity.RecordCount is not initial )
//        or ( ExportQuantity.RecordCount   <> SimplifiedTransactionalAnnex.export     //diferencia en Exportaciones
//       and   ExportQuantity.RecordCount is not initial )
//        or ( CancelQuantity.RecordCount   <> SimplifiedTransactionalAnnex.cancel     //diferencia en Aulados 
//       and   CancelQuantity.RecordCount is not initial ) )
//       
//      then 'DIFFERENCE'
      when SimplifiedTransactionalAnnex.status is initial
      then 'PENDING'
      when SimplifiedTransactionalAnnex.status is not initial
      then SimplifiedTransactionalAnnex.status
      else 'PENDING'
    end                             as Status,
    
    case
      when SimplifiedTransactionalAnnex.status is not initial
       and SalesQuantity.RecordCount <> SimplifiedTransactionalAnnex.sales //diferencia en Ventas
      then 2
      when SimplifiedTransactionalAnnex.status is not initial
       and PurchaseQuantity.RecordCount <> SimplifiedTransactionalAnnex.purchases //diferencia en Compras
      then 2
      when SimplifiedTransactionalAnnex.status is not initial
       and ExportQuantity.RecordCount <> SimplifiedTransactionalAnnex.export //diferencia en Exportaciones
      then 2
      when SimplifiedTransactionalAnnex.status is not initial
       and CancelQuantity.RecordCount <> SimplifiedTransactionalAnnex.cancel //diferencia en Aulados
      then 2
      when SimplifiedTransactionalAnnex.status is not initial
      then 3
      else 1
    end                             as Criticality,
    
    @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #ATTACHMENT }
    SimplifiedTransactionalAnnex.xml             as Xml,
    
    @Semantics.mimeType: true
    SimplifiedTransactionalAnnex.mimetype        as MimeType,
    
    SimplifiedTransactionalAnnex.filename        as FileName,
    
    _CompanyCode.CompanyCodeName                   as CompanyCodeName,
    
    case
//      when SimplifiedTransactionalAnnex.status is not initial
//       and SalesQuantity.RecordCount <> SimplifiedTransactionalAnnex.sales //diferencia en Ventas
//      then _StatusDiference.Description
//      when SimplifiedTransactionalAnnex.status is not initial
//       and PurchaseQuantity.RecordCount <> SimplifiedTransactionalAnnex.purchases //diferencia en Compras
//      then _StatusDiference.Description
//      when SimplifiedTransactionalAnnex.status is not initial
//       and ExportQuantity.RecordCount <> SimplifiedTransactionalAnnex.export //diferencia en Exportaciones
//      then _StatusDiference.Description
//      when SimplifiedTransactionalAnnex.status is not initial
//       and CancelQuantity.RecordCount <> SimplifiedTransactionalAnnex.cancel //diferencia en Aulados
//      then _StatusDiference.Description
      when SimplifiedTransactionalAnnex.status is not initial
      then _Status.Description
      else _StatusPeding.Description
    end                             as Description,
    
    
    _DetailsTransactionalAnnex
    
} where PurchaseQuantity.RecordCount is not initial
    and PurchaseQuantity.TypeProces = 'Compra'
