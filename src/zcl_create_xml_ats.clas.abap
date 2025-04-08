CLASS zcl_create_xml_ats DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_canceled      TYPE STANDARD TABLE OF zts_canceled,
           ty_export        TYPE STANDARD TABLE OF zts_export,
           ty_purchases     TYPE STANDARD TABLE OF zts_purchases,
           ty_sales         TYPE STANDARD TABLE OF zts_sales,
           ty_tot_sales     TYPE STANDARD TABLE OF zts_total_sales,
           ty_withholdings  TYPE STANDARD TABLE OF zts_withholdings,
           ty_support       TYPE STANDARD TABLE OF zts_support,
           ty_pagos         TYPE STANDARD TABLE OF zts_pago.

    DATA: gs_informant     TYPE zts_informant,
          gs_canceled      TYPE zts_canceled,
          gs_export        TYPE zts_export,
          gs_purchases     TYPE zts_purchases,
          gs_sales         TYPE zts_sales,
          gs_tot_sales     TYPE zts_total_sales,
          gs_withholdings  TYPE zts_withholdings,
          gs_support       TYPE zts_support,
          gs_pagos         TYPE zts_pago.

    DATA: gt_canceled     TYPE STANDARD TABLE OF zts_canceled,
          gt_export       TYPE STANDARD TABLE OF zts_export,
          gt_purchases    TYPE STANDARD TABLE OF zts_purchases,
          gt_sales        TYPE STANDARD TABLE OF zts_sales,
          gt_tot_sales    TYPE STANDARD TABLE OF zts_total_sales,
          gt_withholdings TYPE STANDARD TABLE OF zts_withholdings,
          gt_support      TYPE STANDARD TABLE OF zts_support,
          gt_pagos        TYPE STANDARD TABLE OF zts_pago.

    METHODS constructor IMPORTING is_informant    TYPE zts_informant
                                  it_canceled     TYPE zcl_create_xml_ats=>ty_canceled     OPTIONAL
                                  it_export       TYPE zcl_create_xml_ats=>ty_export       OPTIONAL
                                  it_purchases    TYPE zcl_create_xml_ats=>ty_purchases    OPTIONAL
                                  it_sales        TYPE zcl_create_xml_ats=>ty_sales        OPTIONAL
                                  it_total_sales  TYPE zcl_create_xml_ats=>ty_tot_sales    OPTIONAL
                                  it_withholdings TYPE zcl_create_xml_ats=>ty_withholdings OPTIONAL
                                  it_support      TYPE zcl_create_xml_ats=>ty_support      OPTIONAL.

    METHODS CrearXML           EXPORTING  xml   TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS informantXML       CHANGING  xml   TYPE string.

    METHODS ComprasXML         CHANGING  xml   TYPE string.

    METHODS VentasXML          CHANGING  xml   TYPE string.

    METHODS TotalVentasXML     CHANGING  xml   TYPE string.

    METHODS ExportacionesXML   CHANGING  xml   TYPE string.

    METHODS AnuladosXML        CHANGING  xml   TYPE string.

ENDCLASS.

CLASS zcl_create_xml_ats IMPLEMENTATION.

  METHOD constructor.

    gs_informant      = is_informant.
    gt_canceled[]     = it_canceled[].
    gt_export[]       = it_export[].
    gt_purchases[]    = it_purchases[].
    gt_sales[]        = it_sales[].
    gt_tot_sales[]    = it_total_sales[].
    gt_withholdings[] = it_withholdings[].
    gt_support[]      = it_support[].

  ENDMETHOD.

  METHOD crearxml.

    CONCATENATE xml '<iva>' INTO xml.

    me->informantXml( CHANGING xml = xml ).

    IF gt_purchases[] IS NOT INITIAL.
      me->comprasXml( CHANGING xml = xml ).
    ENDIF.

    IF gt_sales[] IS NOT INITIAL.
      me->ventasXml( CHANGING xml = xml ).
    ENDIF.

    IF gt_tot_sales[] IS NOT INITIAL.
      me->totalventasxml( CHANGING xml = xml ).
    ENDIF.

    IF gt_export[] IS NOT INITIAL.
      me->exportacionesXml( CHANGING xml = xml ).
    ENDIF.

    IF gt_canceled[] IS NOT INITIAL.
      me->anuladosXml( CHANGING xml = xml ).
    ENDIF.

    CONCATENATE xml '</iva>' INTO xml.

  ENDMETHOD.

  METHOD anuladosxml.

    CONCATENATE xml '<anulados>' INTO xml.

    LOOP AT gt_canceled INTO gs_canceled.

      CONCATENATE xml '<detalleAnulados>' INTO xml.

      IF gs_canceled-tipocomprobante IS NOT INITIAL.
        CONCATENATE xml '<tipoComprobante>' gs_canceled-tipocomprobante '</tipoComprobante>' INTO xml.
      ENDIF.

      IF gs_canceled-establecimiento IS NOT INITIAL.
        CONCATENATE xml '<establecimiento>' gs_canceled-establecimiento '</establecimiento>' INTO xml.
      ENDIF.

      IF gs_canceled-puntoemision IS NOT INITIAL.
        CONCATENATE xml '<puntoEmision>' gs_canceled-puntoemision '</puntoEmision>' INTO xml.
      ENDIF.

      IF gs_canceled-secuencialInicio IS NOT INITIAL.
        CONCATENATE xml '<secuencialInicio>' gs_canceled-secuencialInicio '</secuencialInicio>' INTO xml.
      ENDIF.

      IF gs_canceled-secuencialFin IS NOT INITIAL.
        CONCATENATE xml '<secuencialFin>' gs_canceled-secuencialFin '</secuencialFin>' INTO xml.
      ENDIF.

      IF gs_canceled-autorizacion IS NOT INITIAL.
        CONCATENATE xml '<autorizacion>' gs_canceled-autorizacion '</autorizacion>' INTO xml.
      ENDIF.

      CONCATENATE xml '</detalleAnulados>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</anulados>' INTO xml.

  ENDMETHOD.

  METHOD comprasxml.

    CONCATENATE xml '<compras>' INTO xml.

    LOOP AT gt_purchases INTO gs_purchases.

      CONCATENATE xml '<detalleCompras>' INTO xml.

      IF gs_purchases-codSustento IS NOT INITIAL.
        CONCATENATE xml '<codSustento>' gs_purchases-codSustento '</codSustento>' INTO xml.
      ENDIF.

      IF gs_purchases-tpIdProv IS NOT INITIAL.
        CONCATENATE xml '<tpIdProv>' gs_purchases-tpIdProv '</tpIdProv>' INTO xml.
      ENDIF.

      IF gs_purchases-idProv IS NOT INITIAL.
        CONCATENATE xml '<idProv>' gs_purchases-idProv '</idProv>' INTO xml.
      ENDIF.

      IF gs_purchases-tipoComprobante IS NOT INITIAL.
        CONCATENATE xml '<tipoComprobante>' gs_purchases-tipoComprobante '</tipoComprobante>' INTO xml.
      ENDIF.

      IF gs_purchases-tpIdProv EQ '03'.
        IF gs_purchases-tipoProv IS NOT INITIAL.
          CONCATENATE xml '<tipoProv>' gs_purchases-tipoProv '</tipoProv>' INTO xml.
        ENDIF.
      ENDIF.

      IF gs_purchases-denoPr IS NOT INITIAL.
        CONCATENATE xml '<denoProv>' gs_purchases-denoPr '</denoProv>' INTO xml.
      ENDIF.

      IF gs_purchases-tpIdProv EQ '01' OR gs_purchases-tpIdProv EQ '02' OR gs_purchases-tpIdProv EQ '03'.
        IF gs_purchases-parteRel IS NOT INITIAL.
          CONCATENATE xml '<parteRel>' gs_purchases-parteRel '</parteRel>' INTO xml.
        ENDIF.
      ENDIF.

      IF gs_purchases-fechaRegistro IS NOT INITIAL.
        CONCATENATE xml '<fechaRegistro>' gs_purchases-fechaRegistro '</fechaRegistro>' INTO xml.
      ENDIF.

      IF gs_purchases-establecimiento IS NOT INITIAL.
        CONCATENATE xml '<establecimiento>' gs_purchases-establecimiento '</establecimiento>' INTO xml.
      ENDIF.

      IF gs_purchases-puntoEmision IS NOT INITIAL.
        CONCATENATE xml '<puntoEmision>' gs_purchases-puntoEmision '</puntoEmision>' INTO xml.
      ENDIF.

      IF gs_purchases-secuencial IS NOT INITIAL.
        CONCATENATE xml '<secuencial>' gs_purchases-secuencial '</secuencial>' INTO xml.
      ENDIF.

      IF gs_purchases-fechaEmision IS NOT INITIAL.
        CONCATENATE xml '<fechaEmision>' gs_purchases-fechaEmision '</fechaEmision>' INTO xml.
      ENDIF.

      IF gs_purchases-autorizacion IS NOT INITIAL.
        CONCATENATE xml '<autorizacion>' gs_purchases-autorizacion '</autorizacion>' INTO xml.
      ENDIF.

      IF gs_purchases-baseNoGraIva IS NOT INITIAL.
        CONDENSE gs_purchases-baseNoGraIva NO-GAPS.
        CONCATENATE xml '<baseNoGraIva>' gs_purchases-baseNoGraIva '</baseNoGraIva>' INTO xml.
      ENDIF.

      IF gs_purchases-baseImponible IS NOT INITIAL.
        CONDENSE gs_purchases-baseImponible NO-GAPS.
        CONCATENATE xml '<baseImponible>' gs_purchases-baseImponible '</baseImponible>' INTO xml.
      ENDIF.

      IF gs_purchases-baseImpGrav IS NOT INITIAL.
        CONDENSE gs_purchases-baseImpGrav NO-GAPS.
        CONCATENATE xml '<baseImpGrav>' gs_purchases-baseImpGrav '</baseImpGrav>' INTO xml.
      ENDIF.

      IF gs_purchases-baseImpExe IS NOT INITIAL.
        CONDENSE gs_purchases-baseImpExe NO-GAPS.
        CONCATENATE xml '<baseImpExe>' gs_purchases-baseImpExe '</baseImpExe>' INTO xml.
      ENDIF.

      IF gs_purchases-montoIce IS NOT INITIAL.
        CONDENSE gs_purchases-montoIce NO-GAPS.
        CONCATENATE xml '<montoIce>' gs_purchases-montoIce '</montoIce>' INTO xml.
      ENDIF.

      IF gs_purchases-montoIva IS NOT INITIAL.
        CONDENSE gs_purchases-montoIva NO-GAPS.
        CONCATENATE xml '<montoIva>' gs_purchases-montoIva '</montoIva>' INTO xml.
      ENDIF.

      IF gs_purchases-valRetBien10 IS NOT INITIAL.
        CONDENSE gs_purchases-valRetBien10 NO-GAPS.
        CONCATENATE xml '<valRetBien10>' gs_purchases-valRetBien10 '</valRetBien10>' INTO xml.
      ENDIF.

      IF gs_purchases-valretserv20 IS NOT INITIAL.
        CONDENSE gs_purchases-valretserv20 NO-GAPS.
        CONCATENATE xml '<valRetServ20>' gs_purchases-valretserv20 '</valRetServ20>' INTO xml.
      ENDIF.

      IF gs_purchases-valorRetBienes IS NOT INITIAL.
        CONDENSE gs_purchases-valorRetBienes NO-GAPS.
        CONCATENATE xml '<valorRetBienes>' gs_purchases-valorRetBienes '</valorRetBienes>' INTO xml.
      ENDIF.

      IF gs_purchases-valretserv50 IS NOT INITIAL.
        CONDENSE gs_purchases-valretserv50 NO-GAPS.
        CONCATENATE xml '<valRetServ50>' gs_purchases-valretserv50 '</valRetServ50>' INTO xml.
      ENDIF.

      IF gs_purchases-valorRetServicios IS NOT INITIAL.
        CONDENSE gs_purchases-valorRetServicios NO-GAPS.
        CONCATENATE xml '<valorRetServicios>' gs_purchases-valorRetServicios '</valorRetServicios>' INTO xml.
      ENDIF.

      IF gs_purchases-valRetServ100 IS NOT INITIAL.
        CONDENSE gs_purchases-valRetServ100 NO-GAPS.
        CONCATENATE xml '<valRetServ100>' gs_purchases-valRetServ100 '</valRetServ100>' INTO xml.
      ENDIF.

      IF gs_purchases-totbasesImpReemb IS NOT INITIAL.
        CONDENSE gs_purchases-totbasesImpReemb NO-GAPS.
        CONCATENATE xml '<totbasesImpReemb>' gs_purchases-totbasesImpReemb '</totbasesImpReemb>' INTO xml.
      ENDIF.

      IF gs_purchases-pagoLocExt IS NOT INITIAL.

        CONCATENATE xml '<pagoExterior>' INTO xml.

        IF gs_purchases-pagoLocExt IS NOT INITIAL.
          CONCATENATE xml '<pagoLocExt>' gs_purchases-pagoLocExt '</pagoLocExt>' INTO xml.
        ENDIF.

        IF gs_purchases-tipoRegi IS NOT INITIAL.
          CONCATENATE xml '<tipoRegi>' gs_purchases-tipoRegi '</tipoRegi>' INTO xml.
        ENDIF.

        IF gs_purchases-paisEfecPagoGen IS NOT INITIAL.
          CONCATENATE xml '<paisEfecPagoGen>' gs_purchases-paisEfecPagoGen '</paisEfecPagoGen>' INTO xml.
        ENDIF.

        IF gs_purchases-paisEfecPagoParFis IS NOT INITIAL.
          CONCATENATE xml '<paisEfecPagoParFis>' gs_purchases-paisEfecPagoParFis '</paisEfecPagoParFis>' INTO xml.
        ENDIF.

        IF gs_purchases-denoPago IS NOT INITIAL.
          CONCATENATE xml '<denopagoRegFis>' gs_purchases-denoPago '</denopagoRegFis>' INTO xml.
        ENDIF.

        IF gs_purchases-paisEfecPago IS NOT INITIAL.
          CONCATENATE xml '<paisEfecPago>' gs_purchases-paisEfecPago '</paisEfecPago>' INTO xml.
        ENDIF.

        IF gs_purchases-aplicConvDobTrib IS NOT INITIAL.
          CONCATENATE xml '<aplicConvDobTrib>' gs_purchases-aplicConvDobTrib '</aplicConvDobTrib>' INTO xml.
        ENDIF.

        IF gs_purchases-pagExtSujRetNorLeg IS NOT INITIAL.
          CONCATENATE xml '<pagExtSujRetNorLeg>' gs_purchases-pagExtSujRetNorLeg '</pagExtSujRetNorLeg>' INTO xml.
        ENDIF.

        IF gs_purchases-pagoRegFis IS NOT INITIAL.
          CONCATENATE xml '<pagoRegFis>' gs_purchases-pagoRegFis '</pagoRegFis>' INTO xml.
        ENDIF.

        CONCATENATE xml '</pagoExterior>' INTO xml.

      ENDIF.

      IF gs_purchases-formaPago IS NOT INITIAL.
        CONCATENATE xml '<formasDePago>' INTO xml.
        CONCATENATE xml '<formaPago>' gs_purchases-formapago '</formaPago>' INTO xml.
        CONCATENATE xml '</formasDePago>' INTO xml.
      ENDIF.

      IF line_exists( me->gt_withholdings[ accountingdocument     = gs_purchases-accountingdocument
                                           accountingdocumenttype = gs_purchases-accountingdocumenttype ] ).

        CONCATENATE xml '<air>' INTO xml.

        LOOP AT me->gt_withholdings INTO gs_withholdings WHERE accountingdocument     = gs_purchases-accountingdocument
                                                           AND accountingdocumenttype = gs_purchases-accountingdocumenttype.

          CONCATENATE xml '<detalleAir>' INTO xml.

          IF gs_withholdings-codRetAir IS NOT INITIAL.
            CONCATENATE xml '<codRetAir>' gs_withholdings-codRetAir '</codRetAir>' INTO xml.
          ENDIF.

          IF gs_withholdings-baseImpAir IS NOT INITIAL.
            CONDENSE gs_withholdings-baseImpAir NO-GAPS.
            CONCATENATE xml '<baseImpAir>' gs_withholdings-baseImpAir '</baseImpAir>' INTO xml.
          ENDIF.

          IF gs_withholdings-porcentajeAir IS NOT INITIAL.
            CONDENSE gs_withholdings-porcentajeAir NO-GAPS.
            CONCATENATE xml '<porcentajeAir>' gs_withholdings-porcentajeAir '</porcentajeAir>' INTO xml.
          ENDIF.

          IF gs_withholdings-valRetAir IS NOT INITIAL.
            CONDENSE gs_withholdings-valRetAir NO-GAPS.
            CONCATENATE xml '<valRetAir>' gs_withholdings-valRetAir '</valRetAir>' INTO xml.
          ENDIF.

          IF gs_withholdings-fechaPagoDiv IS NOT INITIAL.
            CONCATENATE xml '<fechaPagoDiv>' gs_withholdings-fechaPagoDiv '</fechaPagoDiv>' INTO xml.
          ENDIF.

          IF gs_withholdings-imRentaSoc IS NOT INITIAL.
            CONCATENATE xml '<imRentaSoc>' gs_withholdings-imRentaSoc '</imRentaSoc>' INTO xml.
          ENDIF.

          IF gs_withholdings-anioUtDiv IS NOT INITIAL.
            CONCATENATE xml '<anioUtDiv>' gs_withholdings-anioUtDiv '</anioUtDiv>' INTO xml.
          ENDIF.

          CONCATENATE xml '</detalleAir>' INTO xml.

        ENDLOOP.

        CONCATENATE xml '</air>' INTO xml.

      ENDIF.

      IF gs_purchases-estabRetencion1 IS NOT INITIAL.
        CONCATENATE xml '<estabRetencion1>' gs_purchases-estabRetencion1 '</estabRetencion1>' INTO xml.
      ENDIF.

      IF gs_purchases-ptoEmiRetencion1 IS NOT INITIAL.
        CONCATENATE xml '<ptoEmiRetencion1>' gs_purchases-ptoEmiRetencion1 '</ptoEmiRetencion1>' INTO xml.
      ENDIF.

      IF gs_purchases-secRetencion1 IS NOT INITIAL.
        CONCATENATE xml '<secRetencion1>' gs_purchases-secRetencion1 '</secRetencion1>' INTO xml.
      ENDIF.

      IF gs_purchases-autRetencion1 IS NOT INITIAL.
        CONCATENATE xml '<autRetencion1>' gs_purchases-autRetencion1 '</autRetencion1>' INTO xml.
      ENDIF.

      IF gs_purchases-fechaEmiRet1 IS NOT INITIAL.
        CONCATENATE xml '<fechaEmiRet1>' gs_purchases-fechaEmiRet1 '</fechaEmiRet1>' INTO xml.
      ENDIF.

      IF gs_purchases-docModificado IS NOT INITIAL.
        CONCATENATE xml '<docModificado>' gs_purchases-docModificado '</docModificado>' INTO xml.
      ENDIF.

      IF gs_purchases-estabModificado IS NOT INITIAL.
        CONCATENATE xml '<estabModificado>' gs_purchases-estabModificado '</estabModificado>' INTO xml.
      ENDIF.

      IF gs_purchases-ptoEmiModificado IS NOT INITIAL.
        CONCATENATE xml '<ptoEmiModificado>' gs_purchases-ptoEmiModificado '</ptoEmiModificado>' INTO xml.
      ENDIF.

      IF gs_purchases-secModificado IS NOT INITIAL.
        CONCATENATE xml '<secModificado>' gs_purchases-secModificado '</secModificado>' INTO xml.
      ENDIF.

      IF gs_purchases-autModificado IS NOT INITIAL.
        CONCATENATE xml '<autModificado>' gs_purchases-autModificado '</autModificado>' INTO xml.
      ENDIF.

      IF line_exists( me->gt_support[ accountingdocument     = gs_purchases-accountingdocument
                                      accountingdocumenttype = gs_purchases-accountingdocumenttype ] ).

        CONCATENATE xml '<reembolsos>' INTO xml.

        LOOP AT me->gt_support INTO gs_support WHERE accountingdocument     = gs_purchases-accountingdocument
                                                 AND accountingdocumenttype = gs_purchases-accountingdocumenttype.

          CONCATENATE xml '<reembolso>' INTO xml.

          IF gs_support-tipoComprobanteReemb IS NOT INITIAL.
            CONCATENATE xml '<tipoComprobanteReemb>' gs_support-tipoComprobanteReemb '</tipoComprobanteReemb>' INTO xml.
          ENDIF.

          IF gs_support-tpIdProvReemb IS NOT INITIAL.
            CONCATENATE xml '<tpIdProvReemb>' gs_support-tpIdProvReemb '</tpIdProvReemb>' INTO xml.
          ENDIF.

          IF gs_support-idProvReemb IS NOT INITIAL.
            CONCATENATE xml '<idProvReemb>' gs_support-idProvReemb '</idProvReemb>' INTO xml.
          ENDIF.

          IF gs_support-establecimientoReemb IS NOT INITIAL.
            CONCATENATE xml '<establecimientoReemb>' gs_support-establecimientoReemb '</establecimientoReemb>' INTO xml.
          ENDIF.

          IF gs_support-puntoEmisionReemb IS NOT INITIAL.
            CONCATENATE xml '<puntoEmisionReemb>' gs_support-puntoEmisionReemb '</puntoEmisionReemb>' INTO xml.
          ENDIF.

          IF gs_support-secuencialReemb IS NOT INITIAL.
            CONCATENATE xml '<secuencialReemb>' gs_support-secuencialReemb '</secuencialReemb>' INTO xml.
          ENDIF.

          IF gs_support-fechaEmisionReemb IS NOT INITIAL.
            CONCATENATE xml '<fechaEmisionReemb>' gs_support-fechaEmisionReemb '</fechaEmisionReemb>' INTO xml.
          ENDIF.

          IF gs_support-autorizacionReemb IS NOT INITIAL.
            CONCATENATE xml '<autorizacionReemb>' gs_support-autorizacionReemb '</autorizacionReemb>' INTO xml.
          ENDIF.

          IF gs_support-baseImponibleReemb IS NOT INITIAL.
            CONDENSE gs_support-baseImponibleReemb NO-GAPS.
            CONCATENATE xml '<baseImponibleReemb>' gs_support-baseImponibleReemb '</baseImponibleReemb>' INTO xml.
          ENDIF.

          IF gs_support-baseImpGravReemb IS NOT INITIAL.
            CONDENSE gs_support-baseImpGravReemb NO-GAPS.
            CONCATENATE xml '<baseImpGravReemb>' gs_support-baseImpGravReemb '</baseImpGravReemb>' INTO xml.
          ENDIF.

          IF gs_support-baseNoGraIvaReemb IS NOT INITIAL.
            CONDENSE gs_support-baseNoGraIvaReemb NO-GAPS.
            CONCATENATE xml '<baseNoGraIvaReemb>' gs_support-baseNoGraIvaReemb '</baseNoGraIvaReemb>' INTO xml.
          ENDIF.

          IF gs_support-baseImpExeReemb IS NOT INITIAL.
            CONDENSE gs_support-baseImpExeReemb NO-GAPS.
            CONCATENATE xml '<baseImpExeReemb>' gs_support-baseImpExeReemb '</baseImpExeReemb>' INTO xml.
          ENDIF.

          IF gs_support-montoicereemb IS NOT INITIAL.
            CONDENSE gs_support-montoIceReemb NO-GAPS.
            CONCATENATE xml '<montoIceRemb>' gs_support-montoIceReemb '</montoIceRemb>' INTO xml.
          ENDIF.

          IF gs_support-montoIvaRemb IS NOT INITIAL.
            CONDENSE gs_support-montoIvaRemb NO-GAPS.
            CONCATENATE xml '<montoIvaRemb>' gs_support-montoIvaRemb '</montoIvaRemb>' INTO xml.
          ENDIF.

          CONCATENATE xml '</reembolso>' INTO xml.

        ENDLOOP.

        CONCATENATE xml '</reembolsos>' INTO xml.

      ENDIF.

      CONCATENATE xml '</detalleCompras>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</compras>' INTO xml.

  ENDMETHOD.

  METHOD exportacionesxml.

    CONCATENATE xml '<exportaciones>' INTO xml.

    LOOP AT gt_export INTO gs_export.

      CONCATENATE xml '<detalleExportaciones>' INTO xml.

      IF gs_export-tpIdClienteEx IS NOT INITIAL.
        CONCATENATE xml '<tpIdClienteEx>' gs_export-tpIdClienteEx '</tpIdClienteEx>' INTO xml.
      ENDIF.

      IF gs_export-idClienteEx IS NOT INITIAL.
        CONCATENATE xml '<idClienteEx>' gs_export-idClienteEx '</idClienteEx>' INTO xml.
      ENDIF.

      IF gs_export-parterel IS NOT INITIAL.
        CONCATENATE xml '<parteRelExp>' gs_export-parteRel '</parteRelExp>' INTO xml.
      ENDIF.

      IF gs_export-tipoCli IS NOT INITIAL.
        CONCATENATE xml '<tipoCli>' gs_export-tipoCli '</tipoCli>' INTO xml.
      ENDIF.

      IF gs_export-denoExpCli IS NOT INITIAL.
        CONCATENATE xml '<denoExpCli>' gs_export-denoExpCli '</denoExpCli>' INTO xml.
      ENDIF.

      IF gs_export-tipoRegi IS NOT INITIAL.
        CONCATENATE xml '<tipoRegi>' gs_export-tipoRegi '</tipoRegi>' INTO xml.
      ENDIF.

      IF gs_export-paisEfecPagoGen IS NOT INITIAL.
        CONCATENATE xml '<paisEfecPagoGen>' gs_export-paisEfecPagoGen '</paisEfecPagoGen>' INTO xml.
      ENDIF.

      IF gs_export-paisEfecPagoParFis IS NOT INITIAL.
        CONCATENATE xml '<paisEfecPagoParFis>' gs_export-paisEfecPagoParFis '</paisEfecPagoParFis>' INTO xml.
      ENDIF.

      IF gs_export-paisEfecExp IS NOT INITIAL.
        CONCATENATE xml '<paisEfecExp>' gs_export-paisEfecExp '</paisEfecExp>' INTO xml.
      ENDIF.

      IF gs_export-exportacionDe IS NOT INITIAL.
        CONCATENATE xml '<exportacionDe>' gs_export-exportacionDe '</exportacionDe>' INTO xml.
      ENDIF.

      IF gs_export-exportacionde EQ '03'.

        IF gs_export-tipIngExt IS NOT INITIAL.
          CONCATENATE xml '<tipIngExt>' gs_export-tipIngExt '</tipIngExt>' INTO xml.
        ENDIF.

        IF gs_export-ingExtGravOtroPais IS NOT INITIAL.
          CONCATENATE xml '<ingExtGravOtroPais>' gs_export-ingExtGravOtroPais '</ingExtGravOtroPais>' INTO xml.
        ENDIF.

        IF gs_export-impuestoOtroPais IS NOT INITIAL.
          CONCATENATE xml '<impuestoOtroPais>' gs_export-impuestoOtroPais '</impuestoOtroPais>' INTO xml.
        ENDIF.

      ENDIF.

      IF gs_export-tipoComprobante IS NOT INITIAL.
        CONCATENATE xml '<tipoComprobante>' gs_export-tipoComprobante '</tipoComprobante>' INTO xml.
      ENDIF.

      IF gs_export-exportacionde EQ '01'.

        IF gs_export-distAduanero IS NOT INITIAL.
          CONCATENATE xml '<distAduanero>' gs_export-distAduanero '</distAduanero>' INTO xml.
        ENDIF.

        IF gs_export-anio IS NOT INITIAL.
          CONCATENATE xml '<anio>' gs_export-anio '</anio>' INTO xml.
        ENDIF.

        IF gs_export-regimen IS NOT INITIAL.
          CONCATENATE xml '<regimen>' gs_export-regimen '</regimen>' INTO xml.
        ENDIF.

        IF gs_export-correlativo IS NOT INITIAL.
          CONCATENATE xml '<correlativo>' gs_export-correlativo '</correlativo>' INTO xml.
        ENDIF.

        IF gs_export-docTransp IS NOT INITIAL.
          CONCATENATE xml '<docTransp>' gs_export-docTransp '</docTransp>' INTO xml.
        ENDIF.

      ENDIF.

      IF gs_export-fechaEmbarque IS NOT INITIAL.
        CONCATENATE xml '<fechaEmbarque>' gs_export-fechaEmbarque '</fechaEmbarque>' INTO xml.
      ENDIF.

      IF gs_export-valorFOB IS NOT INITIAL.
        CONDENSE gs_export-valorFOB NO-GAPS.
        CONCATENATE xml '<valorFOB>' gs_export-valorFOB '</valorFOB>' INTO xml.
      ENDIF.

      IF gs_export-valorFOBComprobante IS NOT INITIAL.
        CONDENSE gs_export-valorFOBComprobante NO-GAPS.
        CONCATENATE xml '<valorFOBComprobante>' gs_export-valorFOBComprobante '</valorFOBComprobante>' INTO xml.
      ENDIF.

      IF gs_export-establecimiento IS NOT INITIAL.
        CONCATENATE xml '<establecimiento>' gs_export-establecimiento '</establecimiento>' INTO xml.
      ENDIF.

      IF gs_export-puntoEmision IS NOT INITIAL.
        CONCATENATE xml '<puntoEmision>' gs_export-puntoEmision '</puntoEmision>' INTO xml.
      ENDIF.

      IF gs_export-secuencial IS NOT INITIAL.
        CONCATENATE xml '<secuencial>' gs_export-secuencial '</secuencial>' INTO xml.
      ENDIF.

      IF gs_export-autorizacion IS NOT INITIAL.
        CONCATENATE xml '<autorizacion>' gs_export-autorizacion '</autorizacion>' INTO xml.
      ENDIF.

      IF gs_export-fechaEmision IS NOT INITIAL.
        CONCATENATE xml '<fechaEmision>' gs_export-fechaEmision '</fechaEmision>' INTO xml.
      ENDIF.

      CONCATENATE xml '</detalleExportaciones>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</exportaciones>' INTO xml.

  ENDMETHOD.

  METHOD ventasxml.

    CONCATENATE xml '<ventas>' INTO xml.

    LOOP AT gt_sales INTO gs_sales.

      CONCATENATE xml '<detalleVentas>' INTO xml.

      IF gs_sales-tpIdCliente IS NOT INITIAL.
        CONCATENATE xml '<tpIdCliente>' gs_sales-tpIdCliente '</tpIdCliente>' INTO xml.
      ENDIF.

      IF gs_sales-idCliente IS NOT INITIAL.
        CONCATENATE xml '<idCliente>' gs_sales-idCliente '</idCliente>' INTO xml.
      ENDIF.

      IF gs_sales-parteRel IS NOT INITIAL.
        CONCATENATE xml '<parteRelVtas>' gs_sales-parteRel '</parteRelVtas>' INTO xml.
      ENDIF.

      IF gs_sales-tipoCliente IS NOT INITIAL.
        CONCATENATE xml '<tipoCliente>' gs_sales-tipoCliente '</tipoCliente>' INTO xml.
      ENDIF.

      IF gs_sales-denoCli IS NOT INITIAL.
        CONCATENATE xml '<denoCli>' gs_sales-denoCli '</denoCli>' INTO xml.
      ENDIF.

      IF gs_sales-tipoComprobante IS NOT INITIAL.
        CONCATENATE xml '<tipoComprobante>' gs_sales-tipoComprobante '</tipoComprobante>' INTO xml.
      ENDIF.

      IF gs_sales-tipoem IS NOT INITIAL.
        CONCATENATE xml '<tipoEmision>' gs_sales-tipoEm '</tipoEmision>' INTO xml.
      ENDIF.

      IF gs_sales-numeroComprobantes IS NOT INITIAL.
        CONDENSE gs_sales-numeroComprobantes NO-GAPS.
        CONCATENATE xml '<numeroComprobantes>' gs_sales-numeroComprobantes '</numeroComprobantes>' INTO xml.
      ENDIF.

      IF gs_sales-baseNoGraIva IS NOT INITIAL.
        CONDENSE gs_sales-baseNoGraIva NO-GAPS.
        CONCATENATE xml '<baseNoGraIva>' gs_sales-baseNoGraIva '</baseNoGraIva>' INTO xml.
      ENDIF.

      IF gs_sales-baseImponible IS NOT INITIAL.
        CONDENSE gs_sales-baseImponible NO-GAPS.
        CONCATENATE xml '<baseImponible>' gs_sales-baseImponible '</baseImponible>' INTO xml.
      ENDIF.

      IF gs_sales-baseImpGrav IS NOT INITIAL.
        CONDENSE gs_sales-baseImpGrav NO-GAPS.
        CONCATENATE xml '<baseImpGrav>' gs_sales-baseImpGrav '</baseImpGrav>' INTO xml.
      ENDIF.

      IF gs_sales-montoIva IS NOT INITIAL.
        CONDENSE gs_sales-montoIva NO-GAPS.
        CONCATENATE xml '<montoIva>' gs_sales-montoIva '</montoIva>' INTO xml.
      ENDIF.

      IF gs_sales-montoIce IS NOT INITIAL.
        CONDENSE gs_sales-montoIce NO-GAPS.
        CONCATENATE xml '<montoIce>' gs_sales-montoIce '</montoIce>' INTO xml.
      ENDIF.

      IF gs_sales-valorRetIva IS NOT INITIAL.
        CONDENSE gs_sales-valorRetIva NO-GAPS.
        CONCATENATE xml '<valorRetIva>' gs_sales-valorRetIva '</valorRetIva>' INTO xml.
      ENDIF.

      IF gs_sales-valorRetRenta IS NOT INITIAL.
        CONDENSE gs_sales-valorRetRenta NO-GAPS.
        CONCATENATE xml '<valorRetRenta>' gs_sales-valorRetRenta '</valorRetRenta>' INTO xml.
      ENDIF.

      IF gs_sales-tipoComprobante NE '04'.

        IF gs_sales-formaPago IS NOT INITIAL.
          CONCATENATE xml '<formasDePago>' INTO xml.
          CONCATENATE xml '<formaPago>' gs_sales-formaPago '</formaPago>' INTO xml.
          CONCATENATE xml '</formasDePago>' INTO xml.
        ENDIF.

      ENDIF.

      CONCATENATE xml '</detalleVentas>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</ventas>' INTO xml.

  ENDMETHOD.

  METHOD totalventasxml.

    CONCATENATE xml '<ventasEstablecimiento>' INTO xml.

    LOOP AT gt_tot_sales INTO gs_tot_sales.

      CONCATENATE xml '<ventaEst>' INTO xml.

      IF gs_tot_sales-establishment IS NOT INITIAL.
        CONCATENATE xml '<codEstab>' gs_tot_sales-establishment '</codEstab>' INTO xml.
      ENDIF.

      IF gs_tot_sales-salesestablishment IS NOT INITIAL.
        CONDENSE gs_tot_sales-salesestablishment NO-GAPS.
        CONCATENATE xml '<ventasEstab>' gs_tot_sales-salesestablishment '</ventasEstab>' INTO xml.
      ENDIF.

      IF gs_tot_sales-taxcompensated IS NOT INITIAL.
        CONDENSE gs_tot_sales-taxcompensated NO-GAPS.
        CONCATENATE xml '<ivaComp>' gs_tot_sales-taxcompensated '</ivaComp>' INTO xml.
      ENDIF.

      CONCATENATE xml '</ventaEst>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</ventasEstablecimiento>' INTO xml.

  ENDMETHOD.

  METHOD informantxml.

    IF gs_informant-typeid IS NOT INITIAL.
      CONCATENATE xml '<TipoIDInformante>' gs_informant-typeid '</TipoIDInformante>' INTO xml.
    ENDIF.

    IF gs_informant-idinformante IS NOT INITIAL.
      CONCATENATE xml '<IdInformante>' gs_informant-idinformante '</IdInformante>' INTO xml.
    ENDIF.

    IF gs_informant-razonsocial IS NOT INITIAL.
      CONCATENATE xml '<razonSocial>' gs_informant-razonsocial '</razonSocial>' INTO xml.
    ENDIF.

    IF gs_informant-anio IS NOT INITIAL.
      CONCATENATE xml '<Anio>' gs_informant-anio '</Anio>' INTO xml.
    ENDIF.

    IF gs_informant-mes IS NOT INITIAL.
      CONCATENATE xml '<Mes>' gs_informant-mes '</Mes>' INTO xml.
    ENDIF.

    IF gs_informant-numEstabRuc IS NOT INITIAL.
      CONCATENATE xml '<numEstabRuc>' gs_informant-numEstabRuc '</numEstabRuc>' INTO xml.
    ENDIF.

    IF gs_informant-totalVentas IS NOT INITIAL.
      CONDENSE gs_informant-totalVentas NO-GAPS.
      CONCATENATE xml '<totalVentas>' gs_informant-totalVentas '</totalVentas>' INTO xml.
    ENDIF.

    IF gs_informant-codigooperativo IS NOT INITIAL.
      CONCATENATE xml '<codigoOperativo>' gs_informant-codigooperativo '</codigoOperativo>' INTO xml.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
