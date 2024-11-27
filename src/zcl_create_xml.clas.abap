CLASS zcl_create_xml DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
           ty_imp_sust  TYPE STANDARD TABLE OF zts_total_imp_sust,
           ty_pagos     TYPE STANDARD TABLE OF zts_pago,
           ty_motivos   TYPE STANDARD TABLE OF zts_nc_motivo,
           ty_detalle_f TYPE STANDARD TABLE OF zts_fac_detalle,
           ty_detalle_g TYPE STANDARD TABLE OF zts_guia_detalle,
           ty_det_add   TYPE STANDARD TABLE OF zts_det_add,
           ty_det_imp   TYPE STANDARD TABLE OF zts_det_imp,
           ty_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
           ty_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
           ty_retencion TYPE STANDARD TABLE OF zts_retenciones,
           ty_sustento  TYPE STANDARD TABLE OF zts_rete_sustento,
           ty_head_add  TYPE STANDARD TABLE OF zts_head_add.

    DATA: lv_xml     TYPE string.

    DATA: gs_header_f  TYPE zts_fac_header,
          gs_header_g  TYPE zts_guia_header,
          gs_header_l  TYPE zts_liqd_header,
          gs_header_c  TYPE zts_nc_header,
          gs_header_d  TYPE zts_nd_header,
          gs_header_r  TYPE zts_rete_header,
          gs_inf_tribu TYPE zts_inf_tribu,
          gs_impuesto  TYPE zts_total_imp,
          gs_imp_sust  TYPE zts_total_imp_sust,
          gs_pagos     TYPE zts_pago,
          gs_motivos   TYPE zts_nc_motivo,
          gs_detalle_f TYPE zts_fac_detalle,
          gs_detalle_g TYPE zts_guia_detalle,
          gs_det_add   TYPE zts_det_add,
          gs_det_imp   TYPE zts_det_imp,
          gs_retencion TYPE zts_retenciones,
          gs_reembolso TYPE zts_fac_det_reembolso,
          gs_sustento  TYPE zts_rete_sustento,
          gs_reem_imp  TYPE zts_reem_imp,
          gs_head_add  TYPE zts_head_add.

    DATA: gt_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
          gt_imp_sust  TYPE STANDARD TABLE OF zts_total_imp_sust,
          gt_pagos     TYPE STANDARD TABLE OF zts_pago,
          gt_motivos   TYPE STANDARD TABLE OF zts_nc_motivo,
          gt_detalle_f TYPE STANDARD TABLE OF zts_fac_detalle,
          gt_detalle_g TYPE STANDARD TABLE OF zts_guia_detalle,
          gt_det_add   TYPE STANDARD TABLE OF zts_det_add,
          gt_det_imp   TYPE STANDARD TABLE OF zts_det_imp,
          gt_sustento  TYPE STANDARD TABLE OF zts_rete_sustento,
          gt_retencion TYPE STANDARD TABLE OF zts_retenciones,
          gt_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
          gt_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
          gt_head_add  TYPE STANDARD TABLE OF zts_head_add.

    INTERFACES if_oo_adt_classrun.

    METHODS infoTributaria    CHANGING  xml   TYPE string.

    METHODS total_con_impuesto CHANGING  xml   TYPE string.

    METHODS total_impuestos    CHANGING  xml   TYPE string.

    METHODS totalImpuestoSust IMPORTING numdocsustento TYPE zts_rete_sustento-numdocsustento
                              CHANGING  xml   TYPE string.

    METHODS pagos             CHANGING  xml    TYPE string.

    METHODS detail_impuestos  IMPORTING codigoprincipal TYPE zts_det_add-codigoprincipal
                              CHANGING  xml    TYPE string.

    METHODS detail_impuestos_r  IMPORTING estabdocreembolso      TYPE zts_fac_det_reembolso-estabdocreembolso
                                          ptoemidocreembolso     TYPE zts_fac_det_reembolso-ptoemidocreembolso
                                          secuencialdocreembolso TYPE zts_fac_det_reembolso-secuencialdocreembolso
                                CHANGING  xml    TYPE string.

    METHODS detail_addfields  IMPORTING codigoprincipal TYPE zts_det_add-codigoprincipal
                              CHANGING  xml    TYPE string.

    METHODS addfields         CHANGING  xml    TYPE string.

    METHODS Factura           IMPORTING header    TYPE zts_fac_header
                                        inf_tribu TYPE zts_inf_tribu
                                        impuesto  TYPE zcl_create_xml=>ty_impuesto
                                        pagos     TYPE zcl_create_xml=>ty_pagos
                                        detalle   TYPE zcl_create_xml=>ty_detalle_f
                                        det_add   TYPE zcl_create_xml=>ty_det_add
                                        det_imp   TYPE zcl_create_xml=>ty_det_imp
                                        reembolso TYPE zcl_create_xml=>ty_reembolso
                                        reem_imp  TYPE zcl_create_xml=>ty_reem_imp
                                        head_add  TYPE zcl_create_xml=>ty_head_add
                              EXPORTING xml       TYPE string.

    METHODS Factura_header    CHANGING  xml   TYPE string.

    METHODS Factura_detail    CHANGING  xml   TYPE string.

    METHODS Factura_exp_datil CHANGING  xml   TYPE string.

    METHODS Factura_reembolso CHANGING  xml   TYPE string.

    METHODS guiaremision      IMPORTING header    TYPE zts_guia_header
                                        inf_tribu TYPE zts_inf_tribu
                                        detalle   TYPE zcl_create_xml=>ty_detalle_g
                                        det_add   TYPE zcl_create_xml=>ty_det_add
                                        head_add  TYPE zcl_create_xml=>ty_head_add
                              EXPORTING xml       TYPE string.

    METHODS guiaremision_header    CHANGING  xml   TYPE string.

    METHODS guiaremision_detail    CHANGING  xml   TYPE string.

    METHODS liquidacionCompra IMPORTING header    TYPE zts_liqd_header
                                        inf_tribu TYPE zts_inf_tribu
                                        impuesto  TYPE zcl_create_xml=>ty_impuesto
                                        pagos     TYPE zcl_create_xml=>ty_pagos
                                        detalle   TYPE zcl_create_xml=>ty_detalle_f
                                        det_add   TYPE zcl_create_xml=>ty_det_add
                                        det_imp   TYPE zcl_create_xml=>ty_det_imp
                                        reembolso TYPE zcl_create_xml=>ty_reembolso
                                        reem_imp  TYPE zcl_create_xml=>ty_reem_imp
                                        head_add  TYPE zcl_create_xml=>ty_head_add
                              EXPORTING xml       TYPE string.

    METHODS liquidacionHeader    CHANGING  xml   TYPE string.

    METHODS notaCredito IMPORTING header    TYPE zts_nc_header
                                  inf_tribu TYPE zts_inf_tribu
                                  impuesto  TYPE zcl_create_xml=>ty_impuesto
                                  detalle   TYPE zcl_create_xml=>ty_detalle_f
                                  det_add   TYPE zcl_create_xml=>ty_det_add
                                  det_imp   TYPE zcl_create_xml=>ty_det_imp
                                  head_add  TYPE zcl_create_xml=>ty_head_add
                        EXPORTING xml       TYPE string.

    METHODS notaCreditoHeader    CHANGING  xml   TYPE string.

    METHODS notaDedito IMPORTING header    TYPE zts_nd_header
                                 inf_tribu TYPE zts_inf_tribu
                                 impuesto  TYPE zcl_create_xml=>ty_impuesto
                                 pagos     TYPE zcl_create_xml=>ty_pagos
                                 motivos   TYPE zcl_create_xml=>ty_motivos
                                 head_add  TYPE zcl_create_xml=>ty_head_add
                        EXPORTING xml      TYPE string.

    METHODS notaDeditoHeader    CHANGING  xml   TYPE string.

    METHODS retencion IMPORTING  header    TYPE zts_rete_header
                                 inf_tribu TYPE zts_inf_tribu
                                 sustento  TYPE zcl_create_xml=>ty_sustento
                                 impuesto  TYPE zcl_create_xml=>ty_imp_sust
                                 retencion TYPE zcl_create_xml=>ty_retencion
                                 reembolso TYPE zcl_create_xml=>ty_reembolso
                                 reem_imp  TYPE zcl_create_xml=>ty_reem_imp
                                 pagos     TYPE zcl_create_xml=>ty_pagos
                                 head_add  TYPE zcl_create_xml=>ty_head_add
                        EXPORTING xml      TYPE string.

    METHODS retencionHeader    CHANGING  xml   TYPE string.

    METHODS retencionDetalle   IMPORTING numdocsustento TYPE zts_rete_sustento-numdocsustento
                               CHANGING  xml   TYPE string.

    METHODS retencionSustento  CHANGING  xml   TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_CREATE_XML IMPLEMENTATION.


  METHOD addfields.

    IF me->gt_head_add[] IS NOT INITIAL.

      CONCATENATE xml '<infoAdicional>' INTO xml.
      LOOP AT me->gt_head_add INTO gs_head_add.

        IF gs_head_add-nombre IS NOT INITIAL AND gs_head_add-valor IS NOT INITIAL.
          CONCATENATE xml '<campoAdicional nombre="' gs_head_add-nombre '">' gs_head_add-valor '</campoAdicional>' INTO xml.
        ENDIF.

      ENDLOOP.
      CONCATENATE xml '</infoAdicional>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD detail_addfields.


    IF line_exists( me->gt_det_add[ codigoprincipal = codigoprincipal ] ).

      CONCATENATE xml '<detallesAdicionales>' INTO xml.

      LOOP AT me->gt_det_add INTO gs_det_add WHERE ( codigoprincipal = codigoprincipal ).

        IF gs_det_add-valor IS NOT INITIAL AND gs_det_add-titulo IS NOT INITIAL.
          CONCATENATE xml '<detAdicional nombre="' gs_det_add-titulo '" valor="' gs_det_add-valor '"/>' INTO xml.
        ENDIF.

      ENDLOOP.

      CONCATENATE xml '</detallesAdicionales>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD detail_impuestos.

    CONCATENATE xml '<impuestos>' INTO xml.

    LOOP AT me->gt_det_imp INTO gs_det_imp WHERE ( codigoprincipal = codigoprincipal ).
      CONCATENATE xml '<impuesto>' INTO xml.

      IF gs_det_imp-codigo IS NOT INITIAL.
        CONCATENATE xml '<codigo>' gs_det_imp-codigo  '</codigo>' INTO xml.
      ENDIF.

      IF gs_det_imp-codigoporcentaje IS NOT INITIAL.
        CONCATENATE xml '<codigoPorcentaje>' gs_det_imp-codigoporcentaje  '</codigoPorcentaje>' INTO xml.
      ENDIF.

      IF gs_det_imp-tarifa IS NOT INITIAL.
        CONCATENATE xml '<tarifa>' gs_det_imp-tarifa  '</tarifa>' INTO xml.
      ENDIF.

      IF gs_det_imp-baseimponible IS NOT INITIAL.
        CONCATENATE xml '<baseImponible>' gs_det_imp-baseimponible  '</baseImponible>' INTO xml.
      ENDIF.

      IF gs_det_imp-valor IS NOT INITIAL.
        CONCATENATE xml '<valor>' gs_det_imp-valor  '</valor>' INTO xml.
      ENDIF.

      CONCATENATE xml '</impuesto>' INTO xml.
    ENDLOOP.

    CONCATENATE xml '</impuestos>' INTO xml.

  ENDMETHOD.


  METHOD detail_impuestos_r.

    DATA: lv_secuencialdocreembolso TYPE zts_reem_imp-secuencialdocreembolso.

    lv_secuencialdocreembolso = |{ estabdocreembolso }{ ptoemidocreembolso }{ secuencialdocreembolso }|.

    IF line_exists( me->gt_reem_imp[ secuencialdocreembolso = lv_secuencialdocreembolso ] ).

      CONCATENATE xml '<detalleImpuestos>' INTO xml.

      LOOP AT me->gt_reem_imp INTO gs_reem_imp WHERE secuencialdocreembolso = lv_secuencialdocreembolso.
        CONCATENATE xml '<detalleImpuesto>' INTO xml.

        IF gs_reem_imp-codigo IS NOT INITIAL.
          CONCATENATE xml '<codigo>' gs_reem_imp-codigo '</codigo>' INTO xml.
        ENDIF.

        IF gs_reem_imp-codigoporcentaje IS NOT INITIAL.
          CONCATENATE xml '<codigoPorcentaje>' gs_reem_imp-codigoporcentaje '</codigoPorcentaje>' INTO xml.
        ENDIF.

        IF gs_reem_imp-tarifa IS NOT INITIAL.
          CONCATENATE xml '<tarifa>' gs_reem_imp-tarifa '</tarifa>' INTO xml.
        ENDIF.

        IF gs_reem_imp-baseimponiblereembolso IS NOT INITIAL.
          CONCATENATE xml '<baseImponibleReembolso>' gs_reem_imp-baseimponiblereembolso '</baseImponibleReembolso>' INTO xml.
        ENDIF.

        IF gs_reem_imp-impuestoreembolso IS NOT INITIAL.
          CONCATENATE xml '<impuestoReembolso>' gs_reem_imp-impuestoreembolso '</impuestoReembolso>' INTO xml.
        ENDIF.

        CONCATENATE xml '</detalleImpuesto>' INTO xml.
      ENDLOOP.

      CONCATENATE xml '</detalleImpuestos>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD factura.

    gs_header_f    = header.
    gs_inf_tribu   = inf_tribu.
    gt_impuesto[]  = impuesto[].
    gt_pagos[]     = pagos[].
    gt_detalle_f[] = detalle[].
    gt_det_add[]   = det_add[].
    gt_det_imp[]   = det_imp[].
    gt_reembolso[] = reembolso[].
    gt_reem_imp[]  = reem_imp[].
    gt_head_add[]  = head_add[].


    IF me->gs_header_f IS NOT INITIAL.

      xml = '<factura id="comprobante" version="1.1.0">'.

      IF me->gs_header_f IS NOT INITIAL."Cabecera de Factura

        me->factura_header( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_detalle_f[] IS NOT INITIAL."Detalles de Facturas

        me->factura_detail( CHANGING xml = xml ).

      ENDIF.

      IF me->gs_header_f-comercioexterior IS NOT INITIAL. "Datos de Exportacion Solo para el Proveedor Datil
        me->factura_exp_datil( CHANGING xml = xml ).
      ENDIF.

      IF me->gt_reembolso IS NOT INITIAL."Detalles Reembolsos de facturas

        me->factura_reembolso( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

      CONCATENATE xml '</factura>' INTO xml.
      "CONDENSE xml NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD factura_detail.

    IF me->gt_detalle_f[] IS NOT INITIAL.

      CONCATENATE xml '<detalles>' INTO xml.

      LOOP AT me->gt_detalle_f INTO gs_detalle_f.

        CONCATENATE xml '<detalle>' INTO xml.

        IF gs_detalle_f-codigoprincipal IS NOT INITIAL.
          CONCATENATE xml '<codigoPrincipal>' gs_detalle_f-codigoprincipal  '</codigoPrincipal>' INTO xml.
        ENDIF.

        IF gs_detalle_f-codigoauxiliar IS NOT INITIAL.
          CONCATENATE xml '<codigoAuxiliar>' gs_detalle_f-codigoauxiliar  '</codigoAuxiliar>' INTO xml.
        ENDIF.

        IF gs_detalle_f-descripcion IS NOT INITIAL.
          CONCATENATE xml '<descripcion>' gs_detalle_f-descripcion  '</descripcion>' INTO xml.
        ENDIF.

        IF gs_detalle_f-cantidad IS NOT INITIAL.
          CONCATENATE xml '<cantidad>' gs_detalle_f-cantidad  '</cantidad>' INTO xml.
        ENDIF.

        IF gs_detalle_f-preciounitario IS NOT INITIAL.
          CONCATENATE xml '<precioUnitario>' gs_detalle_f-preciounitario  '</precioUnitario>' INTO xml.
        ENDIF.

        IF gs_detalle_f-descuento IS NOT INITIAL.
          CONCATENATE xml '<descuento>' gs_detalle_f-descuento  '</descuento>' INTO xml.
        ENDIF.

        IF gs_detalle_f-totalsinimpuesto IS NOT INITIAL.
          CONCATENATE xml '<precioTotalSinImpuesto>' gs_detalle_f-totalsinimpuesto  '</precioTotalSinImpuesto>' INTO xml.
        ENDIF.

        me->detail_addfields( EXPORTING codigoprincipal = gs_detalle_f-codigoprincipal CHANGING xml = xml ).

        me->detail_impuestos( EXPORTING codigoprincipal = gs_detalle_f-codigoprincipal CHANGING xml = xml ).

        CONCATENATE xml '</detalle>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</detalles>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD factura_header.

    me->infoTributaria( CHANGING xml = xml ).

    CONCATENATE xml '<infoFactura>' INTO xml.

    IF me->gs_header_f-fechaemision IS NOT INITIAL.
      CONCATENATE xml '<fechaEmision>' me->gs_header_f-fechaemision '</fechaEmision>' INTO xml.
    ENDIF.

    IF me->gs_header_f-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_f-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_f-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_f-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_f-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_f-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

*        Factura de Exportacion no Datil
*    IF me->gs_header_f-comercioexterior IS NOT INITIAL.
*      CONCATENATE xml '<comercioExterior>' me->gs_header_f-comercioexterior '</comercioExterior>' INTO xml.
*      IF me->gs_header_f-incotermfactura IS NOT INITIAL.
*        CONCATENATE xml '<incoTermFactura>' me->gs_header_f-incotermfactura '</incoTermFactura>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-lugarincoterm IS NOT INITIAL.
*        CONCATENATE xml '<lugarIncoTerm>' me->gs_header_f-lugarincoterm '</lugarIncoTerm>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-paisorigen IS NOT INITIAL.
*        CONCATENATE xml '<paisOrigen>' me->gs_header_f-paisorigen '</paisOrigen>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-puertoembarque IS NOT INITIAL.
*        CONCATENATE xml '<puertoEmbarque>' me->gs_header_f-puertoembarque '</puertoEmbarque>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-puertodestino IS NOT INITIAL.
*        CONCATENATE xml '<puertoDestino>' me->gs_header_f-puertodestino '</puertoDestino>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-paisdestino IS NOT INITIAL.
*        CONCATENATE xml '<paisDestino>' me->gs_header_f-paisdestino '</paisDestino>' INTO xml.
*      ENDIF.
*      IF me->gs_header_f-paisadqusicion IS NOT INITIAL.
*        CONCATENATE xml '<paisAdquisicion>' me->gs_header_f-paisadqusicion '</paisAdquisicion>' INTO xml.
*      ENDIF.
*    ENDIF.

    IF me->gs_header_f-tipoidcomprador IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionComprador>' me->gs_header_f-tipoidcomprador '</tipoIdentificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_f-guiaremision IS NOT INITIAL.
      CONCATENATE xml '<guiaRemision>' me->gs_header_f-guiaremision '</guiaRemision>' INTO xml.
    ENDIF.

    IF me->gs_header_f-razonsocialcomprador IS NOT INITIAL.
      CONCATENATE xml '<razonSocialComprador>' me->gs_header_f-razonsocialcomprador '</razonSocialComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_f-idcomprador IS NOT INITIAL.
      CONCATENATE xml '<identificacionComprador>' me->gs_header_f-idcomprador '</identificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_f-direccioncomprador IS NOT INITIAL.
      CONCATENATE xml '<direccionComprador>' me->gs_header_f-direccioncomprador '</direccionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_f-totalSinImpuestos IS NOT INITIAL.
      CONCATENATE xml '<totalSinImpuestos>' me->gs_header_f-totalsinimpuestos '</totalSinImpuestos>' INTO xml.
    ENDIF.

*        Factura de Exportacion no Datil
*    IF me->gs_header_f-comercioexterior IS NOT INITIAL.
*      IF me->gs_header_f-incotermtotalsin IS NOT INITIAL.
*        CONCATENATE xml '<incoTermTotalSinImpuestos>' me->gs_header_f-incotermtotalsin '</incoTermTotalSinImpuestos>' INTO xml.
*      ENDIF.
*    ENDIF.

    IF me->gs_header_f-totaldescuento IS NOT INITIAL.
      CONCATENATE xml '<totalDescuento>' me->gs_header_f-totaldescuento '</totalDescuento>' INTO xml.
    ENDIF.

    IF me->gs_header_f-coddocreemb IS NOT INITIAL.
      CONCATENATE xml '<codDocReembolso>' me->gs_header_f-coddocreemb '</codDocReembolso>' INTO xml.
    ENDIF.

    IF me->gs_header_f-totalcomprobantesreembolso IS NOT INITIAL.
      CONCATENATE xml '<totalComprobantesReembolso>' me->gs_header_f-totalcomprobantesreembolso '</totalComprobantesReembolso>' INTO xml.
    ENDIF.

    IF me->gs_header_f-totalbaseimponiblereembolso IS NOT INITIAL.
      CONCATENATE xml '<totalBaseImponibleReembolso>' me->gs_header_f-totalbaseimponiblereembolso '</totalBaseImponibleReembolso>' INTO xml.
    ENDIF.

    IF me->gs_header_f-totalimpuestoreembolso IS NOT INITIAL.
      CONCATENATE xml '<totalImpuestoReembolso>' me->gs_header_f-totalimpuestoreembolso '</totalImpuestoReembolso>' INTO xml.
    ENDIF.

    me->total_con_impuesto( CHANGING xml = xml ).

    IF me->gs_header_f-propina IS NOT INITIAL.
      CONCATENATE xml '<propina>' me->gs_header_f-propina '</propina>' INTO xml.
    ENDIF.

**        Factura de Exportacion no Datil
*    IF me->gs_header_f-comercioexterior IS NOT INITIAL.
*
*      IF me->gs_header_f-fleteinternacional IS NOT INITIAL.
*        CONCATENATE xml '<fleteInternacional>' me->gs_header_f-fleteinternacional '</fleteInternacional>' INTO xml.
*      ENDIF.
*
*      IF me->gs_header_f-segurointernacional IS NOT INITIAL.
*        CONCATENATE xml '<seguroInternacional>' me->gs_header_f-segurointernacional '</seguroInternacional>' INTO xml.
*      ENDIF.
*
*      IF me->gs_header_f-gastosaduaneros IS NOT INITIAL.
*        CONCATENATE xml '<gastosAduaneros>' me->gs_header_f-gastosaduaneros '</gastosAduaneros>' INTO xml.
*      ENDIF.
*
*      IF me->gs_header_f-gastostransporteotros IS NOT INITIAL.
*        CONCATENATE xml '<gastosTransporteOtros>' me->gs_header_f-gastostransporteotros '</gastosTransporteOtros>' INTO xml.
*      ENDIF.
*
*    ENDIF.

    IF me->gs_header_f-importetotal IS NOT INITIAL.
      CONCATENATE xml '<importeTotal>' me->gs_header_f-importetotal '</importeTotal>' INTO xml.
    ENDIF.

    IF me->gs_header_f-moneda IS NOT INITIAL.
      CONCATENATE xml '<moneda>' me->gs_header_f-moneda  '</moneda>' INTO xml.
    ENDIF.

    me->pagos( CHANGING xml = xml ).

    CONCATENATE xml '</infoFactura>' INTO xml.

    "CONDENSE xml NO-GAPS.

  ENDMETHOD.


  METHOD factura_reembolso.

    IF me->gt_reembolso IS NOT INITIAL.

      CONCATENATE xml '<reembolsos>' INTO xml.
      LOOP AT me->gt_reembolso INTO gs_reembolso.
        CONCATENATE xml '<reembolsoDetalle>' INTO xml.

        IF gs_reembolso-tipoidentificacionproveedorree IS NOT INITIAL.
          CONCATENATE xml '<tipoIdentificacionProveedorReembolso>' gs_reembolso-tipoidentificacionproveedorree
                          '</tipoIdentificacionProveedorReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-identificacionproveedorreembol IS NOT INITIAL.
          CONCATENATE xml '<identificacionProveedorReembolso>' gs_reembolso-identificacionproveedorreembol
                          '</identificacionProveedorReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-codpaispagoproveedorreembolso IS NOT INITIAL.
          CONCATENATE xml '<codPaisPagoProveedorReembolso>' gs_reembolso-codpaispagoproveedorreembolso
                          '</codPaisPagoProveedorReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-tipoproveedorreembolso IS NOT INITIAL.
          CONCATENATE xml '<tipoProveedorReembolso>' gs_reembolso-tipoproveedorreembolso
                          '</tipoProveedorReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-coddocreembolso IS NOT INITIAL.
          CONCATENATE xml '<codDocReembolso>' gs_reembolso-coddocreembolso
                          '</codDocReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-estabdocreembolso IS NOT INITIAL.
          CONCATENATE xml '<estabDocReembolso>' gs_reembolso-estabdocreembolso
                          '</estabDocReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-ptoemidocreembolso IS NOT INITIAL.
          CONCATENATE xml '<ptoEmiDocReembolso>' gs_reembolso-ptoemidocreembolso
                          '</ptoEmiDocReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-secuencialdocreembolso IS NOT INITIAL.
          CONCATENATE xml '<secuencialDocReembolso>' gs_reembolso-secuencialdocreembolso
                          '</secuencialDocReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-fechaemisiondocreembolso IS NOT INITIAL.
          CONCATENATE xml '<fechaEmisionDocReembolso>' gs_reembolso-fechaemisiondocreembolso
                          '</fechaEmisionDocReembolso>' INTO xml.
        ENDIF.

        IF gs_reembolso-numeroautorizaciondocreemb IS NOT INITIAL.
          CONCATENATE xml '<numeroautorizacionDocReemb>' gs_reembolso-numeroautorizaciondocreemb
                          '</numeroautorizacionDocReemb>' INTO xml.
        ENDIF.

        me->detail_impuestos_r( EXPORTING   estabdocreembolso      = gs_reembolso-estabdocreembolso
                                            ptoemidocreembolso     = gs_reembolso-ptoemidocreembolso
                                            secuencialdocreembolso = gs_reembolso-secuencialdocreembolso CHANGING xml = xml ).

        CONCATENATE xml '</reembolsoDetalle>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</reembolsos>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD guiaremision.

    gs_header_g    = header.
    gs_inf_tribu   = inf_tribu.
    gt_detalle_g[] = detalle[].
    gt_det_add[]   = det_add[].
    gt_head_add[]  = head_add[].

     xml = '<guiaRemision id="comprobante" version="1.1.0">' .

    IF me->gs_header_g IS NOT INITIAL.

      IF me->gs_header_g IS NOT INITIAL."Cabecera de Guias de Remision

        me->guiaremision_header( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_detalle_g[] IS NOT INITIAL."Detalles de Guias de Remision

        me->guiaremision_detail( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

    ENDIF.

    CONCATENATE xml '</guiaRemision>' INTO xml.
    "CONDENSE xml NO-GAPS.

  ENDMETHOD.


  METHOD guiaremision_detail.

    IF me->gt_detalle_g[] IS NOT INITIAL.

      CONCATENATE xml '<detalles>' INTO xml.

      LOOP AT me->gt_detalle_g INTO gs_detalle_g.

        CONCATENATE xml '<detalle>' INTO xml.

        IF gs_detalle_g-codigoprincipal IS NOT INITIAL.
          CONCATENATE xml '<codigoInterno>' gs_detalle_g-codigoprincipal  '</codigoInterno>' INTO xml.
        ENDIF.

        IF gs_detalle_g-codigoauxiliar IS NOT INITIAL.
          CONCATENATE xml '<codigoAdicional>' gs_detalle_g-codigoauxiliar  '</codigoAdicional>' INTO xml.
        ENDIF.

        IF gs_detalle_g-descripcion  IS NOT INITIAL.
          CONCATENATE xml '<descripcion>' gs_detalle_g-descripcion  '</descripcion>' INTO xml.
        ENDIF.

        IF gs_detalle_g-cantidad  IS NOT INITIAL.
          CONCATENATE xml '<cantidad>' gs_detalle_g-cantidad  '</cantidad>' INTO xml.
        ENDIF.

        me->detail_addfields( EXPORTING codigoprincipal = gs_detalle_g-codigoprincipal CHANGING xml = xml ).

        CONCATENATE xml '</detalle>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</detalles>' INTO xml.

    ENDIF.

    CONCATENATE xml '</destinatario>' INTO xml.

    CONCATENATE xml '</destinatarios>' INTO xml.

  ENDMETHOD.


  METHOD guiaremision_header.

    me->infoTributaria( CHANGING xml = xml ).

    CONCATENATE xml '<infoGuiaRemision>' INTO xml.

    IF me->gs_header_g-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_g-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_g-dirpartida IS NOT INITIAL.
      CONCATENATE xml '<dirPartida>' me->gs_header_g-dirpartida '</dirPartida>' INTO xml.
    ENDIF.

    IF me->gs_header_g-raztranspor IS NOT INITIAL.
      CONCATENATE xml '<razonSocialTransportista>' me->gs_header_g-raztranspor '</razonSocialTransportista>' INTO xml.
    ENDIF.

    IF me->gs_header_g-tipo_id_trans IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionTransportista>' me->gs_header_g-tipo_id_trans '</tipoIdentificacionTransportista>' INTO xml.
    ENDIF.

    IF me->gs_header_g-ruc_transpor IS NOT INITIAL.
      CONCATENATE xml '<rucTransportista>' me->gs_header_g-ruc_transpor '</rucTransportista>' INTO xml.
    ENDIF.

    IF me->gs_header_g-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_g-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

    IF me->gs_header_g-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_g-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_g-fcha_initransp IS NOT INITIAL.
      CONCATENATE xml '<fechaIniTransporte>' me->gs_header_g-fcha_initransp '</fechaIniTransporte>' INTO xml.
    ENDIF.

    IF me->gs_header_g-fcha_fintransp IS NOT INITIAL.
      CONCATENATE xml '<fechaFinTransporte>' me->gs_header_g-fcha_fintransp '</fechaFinTransporte>' INTO xml.
    ENDIF.

    IF me->gs_header_g-placa IS NOT INITIAL.
      CONCATENATE xml '<placa>' me->gs_header_g-placa '</placa>' INTO xml.
    ENDIF.

    CONCATENATE xml '</infoGuiaRemision>' INTO xml.

    CONCATENATE xml '<destinatarios>' INTO xml.
    CONCATENATE xml '<destinatario>' INTO xml.

    IF me->gs_header_g-id_destinatario IS NOT INITIAL.
      CONCATENATE xml '<identificacionDestinatario>' me->gs_header_g-id_destinatario '</identificacionDestinatario>' INTO xml.
    ENDIF.

    IF me->gs_header_g-rz_destinatario IS NOT INITIAL.
      CONCATENATE xml '<razonSocialDestinatario>' me->gs_header_g-rz_destinatario '</razonSocialDestinatario>' INTO xml.
    ENDIF.

    IF me->gs_header_g-dir_destinatario IS NOT INITIAL.
      CONCATENATE xml '<dirDestinatario>' me->gs_header_g-dir_destinatario '</dirDestinatario>' INTO xml.
    ENDIF.

    IF me->gs_header_g-motivo_traslado IS NOT INITIAL.
      CONCATENATE xml '<motivoTraslado>' me->gs_header_g-motivo_traslado '</motivoTraslado>' INTO xml.
    ENDIF.

    IF me->gs_header_g-num_doc_sustento IS NOT INITIAL.

        IF me->gs_header_g-cod_doc_sustento IS NOT INITIAL.
          CONCATENATE xml '<codDocSustento>' me->gs_header_g-cod_doc_sustento '</codDocSustento>' INTO xml.
        ENDIF.

        IF me->gs_header_g-num_doc_sustento IS NOT INITIAL.
          CONCATENATE xml '<numDocSustento>' me->gs_header_g-num_doc_sustento '</numDocSustento>' INTO xml.
        ENDIF.

        IF me->gs_header_g-num_aut_sustento IS NOT INITIAL.
          CONCATENATE xml '<numAutDocSustento>' me->gs_header_g-num_aut_sustento '</numAutDocSustento>' INTO xml.
        ENDIF.

        IF me->gs_header_g-fch_emis_doc_sust IS NOT INITIAL.
          CONCATENATE xml '<fechaEmisionDocSustento>' me->gs_header_g-fch_emis_doc_sust '</fechaEmisionDocSustento>' INTO xml.
        ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

*    DATA: ls_header    TYPE zts_fac_header,
*          lt_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
*          lt_pagos     TYPE STANDARD TABLE OF zts_pago,
*          lt_detalle   TYPE STANDARD TABLE OF zts_fac_detalle,
*          lt_det_add   TYPE STANDARD TABLE OF zts_det_add,
*          lt_det_imp   TYPE STANDARD TABLE OF zts_det_imp,
*          lt_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
*          lt_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
*          lt_head_add  TYPE STANDARD TABLE OF zts_head_add.
*
*    me->factura( EXPORTING header   = ls_header
*                           impuesto = lt_impuesto
*                           pagos    = lt_pagos
*                           detalle  = lt_detalle
*                           det_add  = lt_det_add
*                           det_imp  = lt_det_imp
*                           reembolso = lt_reembolso
*                           reem_imp = lt_reem_imp
*                           head_add = lt_head_add
*                 IMPORTING xml = lv_xml ).
*
*    out->write( lv_xml ).
*
*    out->write( 'Sin Error' ).

  ENDMETHOD.


  METHOD infoTributaria.

    CONCATENATE xml '<infoTributaria>' INTO xml.

    IF me->gs_inf_tribu-ambiente      IS NOT INITIAL.
      CONCATENATE xml '<ambiente>' me->gs_inf_tribu-ambiente '</ambiente>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-tipoemision IS NOT INITIAL.
      CONCATENATE xml '<tipoEmision>' me->gs_inf_tribu-tipoemision '</tipoEmision>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-tipoemision IS NOT INITIAL.
      CONCATENATE xml '<razonSocial>' me->gs_inf_tribu-razonsocial '</razonSocial>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-nombrecomercial IS NOT INITIAL.
      CONCATENATE xml '<nombreComercial>' me->gs_inf_tribu-nombrecomercial '</nombreComercial>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-ruc IS NOT INITIAL.
      CONCATENATE xml '<ruc>' me->gs_inf_tribu-ruc '</ruc>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-claveacceso IS NOT INITIAL.
      CONCATENATE xml '<claveAcceso>' me->gs_inf_tribu-claveacceso '</claveAcceso>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-coddoc IS NOT INITIAL.
      CONCATENATE xml '<codDoc>' me->gs_inf_tribu-coddoc '</codDoc>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-estab IS NOT INITIAL.
      CONCATENATE xml '<estab>' me->gs_inf_tribu-estab '</estab>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-ptoemi IS NOT INITIAL.
      CONCATENATE xml '<ptoEmi>' me->gs_inf_tribu-ptoemi '</ptoEmi>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-secuencial IS NOT INITIAL.
      CONCATENATE xml '<secuencial>' me->gs_inf_tribu-secuencial '</secuencial>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-dirmatriz IS NOT INITIAL.
      CONCATENATE xml '<dirMatriz>' me->gs_inf_tribu-dirmatriz '</dirMatriz>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-regimenmicroempresas IS NOT INITIAL.
      CONCATENATE xml '<regimenMicroempresas>' me->gs_inf_tribu-regimenmicroempresas '</regimenMicroempresas>' INTO xml.
    ENDIF.

    IF me->gs_inf_tribu-agenteretencion IS NOT INITIAL.
      CONCATENATE xml '<agenteRetencion>' me->gs_inf_tribu-agenteretencion '</agenteRetencion>' INTO xml.
    ENDIF.

    CONCATENATE xml '</infoTributaria>' INTO xml.

  ENDMETHOD.


  METHOD liquidacioncompra.

    gs_header_l    = header.
    gs_inf_tribu   = inf_tribu.
    gt_impuesto[]  = impuesto[].
    gt_pagos[]     = pagos[].
    gt_detalle_f[]   = detalle[].
    gt_det_add[]   = det_add[].
    gt_det_imp[]   = det_imp[].
    gt_reembolso[] = reembolso[].
    gt_reem_imp[]  = reem_imp[].
    gt_head_add[]  = head_add[].


    IF me->gs_header_l IS NOT INITIAL.

      xml = '<liquidacionCompra id="comprobante" version="1.0.0">'.

      IF me->gs_header_l IS NOT INITIAL."Cabecera de liquidacion Compra

        me->liquidacionHeader( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_detalle_f[] IS NOT INITIAL."Detalles de Facturas

        me->factura_detail( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_reembolso IS NOT INITIAL."Detalles Reembolsos de facturas

        me->factura_reembolso( CHANGING xml = xml ).

      ENDIF.

*      IF me->gs_header_l-tipoidentificacionproveedor IS NOT INITIAL.
*        CONCATENATE xml '<maquinariafiscal>' INTO xml.
*        IF me->gs_header_l-marca IS NOT INITIAL.
*          CONCATENATE xml '<marca>' me->gs_header_l-marca '</marca>' INTO xml.
*        ENDIF.
*        IF me->gs_header_l-modelo IS NOT INITIAL.
*          CONCATENATE xml '<modelo>' me->gs_header_l-marca '</modelo>' INTO xml.
*        ENDIF.
*        IF me->gs_header_l-serie IS NOT INITIAL.
*          CONCATENATE xml '<serie>' me->gs_header_l-serie '</serie>' INTO xml.
*        ENDIF.
*        CONCATENATE xml '</maquinariafiscal>' INTO xml.
*      ENDIF.

      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

      CONCATENATE xml '</liquidacionCompra>' INTO xml.
      "CONDENSE xml NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD liquidacionHeader.

    me->infoTributaria( CHANGING xml = xml ).

    CONCATENATE xml '<infoLiquidacionCompra>' INTO xml.

    IF me->gs_header_l-fechaemision IS NOT INITIAL.
      CONCATENATE xml '<fechaEmision>' me->gs_header_l-fechaemision '</fechaEmision>' INTO xml.
    ENDIF.

    IF me->gs_header_l-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_l-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_l-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_l-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_l-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_l-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

    IF me->gs_header_l-tipoidentificacionproveedor IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionProveedor>' me->gs_header_l-tipoidentificacionproveedor '</tipoIdentificacionProveedor>' INTO xml.
    ENDIF.

    IF me->gs_header_l-razonsocialproveedor IS NOT INITIAL.
      CONCATENATE xml '<razonSocialProveedor>' me->gs_header_l-razonsocialproveedor '</razonSocialProveedor>' INTO xml.
    ENDIF.

    IF me->gs_header_l-identificacionproveedor IS NOT INITIAL.
      CONCATENATE xml '<identificacionProveedor>' me->gs_header_l-identificacionproveedor '</identificacionProveedor>' INTO xml.
    ENDIF.

    IF me->gs_header_l-direccionproveedor IS NOT INITIAL.
      CONCATENATE xml '<direccionProveedor>' me->gs_header_l-direccionproveedor '</direccionProveedor>' INTO xml.
    ENDIF.

    IF me->gs_header_l-totalsinimpuestos IS NOT INITIAL.
      CONCATENATE xml '<totalSinImpuestos>' me->gs_header_l-totalsinimpuestos '</totalSinImpuestos>' INTO xml.
    ENDIF.

    IF me->gs_header_l-totaldescuento IS NOT INITIAL.
      CONCATENATE xml '<totalDescuento>' me->gs_header_l-totaldescuento '</totalDescuento>' INTO xml.
    ENDIF.

    IF me->gs_header_l-coddocreembolso IS NOT INITIAL.

      IF me->gs_header_l-coddocreembolso IS NOT INITIAL.
        CONCATENATE xml '<codDocReembolso>' me->gs_header_l-coddocreembolso '</codDocReembolso>' INTO xml.
      ENDIF.
      IF me->gs_header_l-totalcomprobantesreembolso IS NOT INITIAL.
        CONCATENATE xml '<totalComprobantesReembolso>' me->gs_header_l-totalcomprobantesreembolso '</totalComprobantesReembolso>' INTO xml.
      ENDIF.
      IF me->gs_header_l-totalbaseimponiblereembolso IS NOT INITIAL.
        CONCATENATE xml '<totalBaseImponibleReembolso>' me->gs_header_l-totalbaseimponiblereembolso '</totalBaseImponibleReembolso>' INTO xml.
      ENDIF.
      IF me->gs_header_l-totalimpuestoreembolso IS NOT INITIAL.
        CONCATENATE xml '<totalImpuestoReembolso>' me->gs_header_l-totalimpuestoreembolso '</totalImpuestoReembolso>' INTO xml.
      ENDIF.

    ENDIF.

    me->total_con_impuesto( CHANGING xml = xml ).

    IF me->gs_header_l-importetotal IS NOT INITIAL.
      CONCATENATE xml '<importeTotal>' me->gs_header_l-importetotal '</importeTotal>' INTO xml.
    ENDIF.

    IF me->gs_header_l-moneda IS NOT INITIAL.
      CONCATENATE xml '<moneda>' me->gs_header_l-moneda '</moneda>' INTO xml.
    ENDIF.

    me->pagos( CHANGING xml = xml ).

    CONCATENATE xml '</infoLiquidacionCompra>' INTO xml.

  ENDMETHOD.


  METHOD notacredito.

    gs_header_c    = header.
    gs_inf_tribu   = inf_tribu.
    gt_impuesto[]  = impuesto[].
    gt_detalle_f[] = detalle[].
    gt_det_add[]   = det_add[].
    gt_det_imp[]   = det_imp[].
    gt_head_add[]  = head_add[].

    IF me->gs_header_c IS NOT INITIAL.

      xml = '<notaCredito id="comprobante" version="1.1.0">'.

      IF me->gs_header_c IS NOT INITIAL."Cabecera de Nota de Credito

        me->notacreditoHeader( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_detalle_f[] IS NOT INITIAL."Detalles de Facturas

        me->factura_detail( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

      CONCATENATE xml '</notaCredito>' INTO xml.
      "CONDENSE xml NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD notacreditoheader.

    me->infoTributaria( CHANGING xml = xml ).

    IF me->gs_header_c-fechaemision IS NOT INITIAL.
      CONCATENATE xml '<fechaEmision>' me->gs_header_c-fechaemision '</fechaEmision>' INTO xml.
    ENDIF.

    IF me->gs_header_c-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_c-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_c-tipoidcomprador IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionComprador>' me->gs_header_c-tipoidcomprador '</tipoIdentificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_c-razonsocialcomprador IS NOT INITIAL.
      CONCATENATE xml '<razonSocialComprador>' me->gs_header_c-razonsocialcomprador '</razonSocialComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_c-idcomprador IS NOT INITIAL.
      CONCATENATE xml '<identificacionComprador>' me->gs_header_c-idcomprador '</identificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_c-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_c-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_c-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_c-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

    IF me->gs_header_c-cod_docmod IS NOT INITIAL.
      CONCATENATE xml '<codDocModificado>' me->gs_header_c-cod_docmod '</codDocModificado>' INTO xml.
    ENDIF.

    IF me->gs_header_c-num_cod_modif IS NOT INITIAL.
      CONCATENATE xml '<numDocModificado>' me->gs_header_c-num_cod_modif '</numDocModificado>' INTO xml.
    ENDIF.

    IF me->gs_header_c-fech_emis_modif IS NOT INITIAL.
      CONCATENATE xml '<fechaEmisionDocSustento>' me->gs_header_c-fech_emis_modif '</fechaEmisionDocSustento>' INTO xml.
    ENDIF.

    IF me->gs_header_c-totalsinimpuestos IS NOT INITIAL.
      CONCATENATE xml '<totalSinImpuestos>' me->gs_header_c-totalsinimpuestos '</totalSinImpuestos>' INTO xml.
    ENDIF.

    IF me->gs_header_c-valor_mod IS NOT INITIAL.
      CONCATENATE xml '<valorModificacion>' me->gs_header_c-valor_mod '</valorModificacion>' INTO xml.
    ENDIF.

    IF me->gs_header_c-moneda IS NOT INITIAL.
      CONCATENATE xml '<moneda>' me->gs_header_c-moneda '</moneda>' INTO xml.
    ENDIF.

    me->total_con_impuesto( CHANGING xml = xml ).

    IF me->gs_header_c-motivo IS NOT INITIAL.
      CONCATENATE xml '<motivo>' me->gs_header_c-motivo '</motivo>' INTO xml.
    ENDIF.

  ENDMETHOD.


  METHOD notadedito.

    gs_header_d    = header.
    gs_inf_tribu   = inf_tribu.
    gt_impuesto[]  = impuesto[].
    gt_pagos[]     = pagos[].
    gt_motivos[]   = motivos[].
    gt_head_add[]  = head_add[].

    IF me->gs_header_d IS NOT INITIAL.

      xml = '<notaDebito id="comprobante" version="1.0.0">'.

      IF me->gs_header_d IS NOT INITIAL."Cabecera de Nota de Dedito

        me->notaDeditoHeader( CHANGING xml = xml ).

      ENDIF.


      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

      CONCATENATE xml '</notaDebito>' INTO xml.
      "CONDENSE xml NO-GAPS.

    ENDIF.

  ENDMETHOD.


  METHOD notadeditoheader.

    me->infoTributaria( CHANGING xml = xml ).

    IF me->gs_header_d-fechaemision IS NOT INITIAL.
      CONCATENATE xml '<fechaEmision>' me->gs_header_d-fechaemision '</fechaEmision>' INTO xml.
    ENDIF.

    IF me->gs_header_d-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_d-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_d-tipoidcomprador IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionComprador>' me->gs_header_d-tipoidcomprador '</tipoIdentificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_d-razonsocialcomprador IS NOT INITIAL.
      CONCATENATE xml '<razonSocialComprador>' me->gs_header_d-razonsocialcomprador '</razonSocialComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_d-idcomprador IS NOT INITIAL.
      CONCATENATE xml '<identificacionComprador>' me->gs_header_d-idcomprador '</identificacionComprador>' INTO xml.
    ENDIF.

    IF me->gs_header_d-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_d-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_d-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_d-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

    IF me->gs_header_d-cod_docmod IS NOT INITIAL.
      CONCATENATE xml '<codDocModificado>' me->gs_header_d-cod_docmod '</codDocModificado>' INTO xml.
    ENDIF.

    IF me->gs_header_d-num_cod_modif IS NOT INITIAL.
      CONCATENATE xml '<numDocModificado>' me->gs_header_d-num_cod_modif '</numDocModificado>' INTO xml.
    ENDIF.

    IF me->gs_header_d-fech_emis_modif IS NOT INITIAL.
      CONCATENATE xml '<fechaEmisionDocSustento>' me->gs_header_d-fech_emis_modif '</fechaEmisionDocSustento>' INTO xml.
    ENDIF.

    IF me->gs_header_d-totalsinimpuestos IS NOT INITIAL.
      CONCATENATE xml '<totalSinImpuestos>' me->gs_header_d-totalsinimpuestos '</totalSinImpuestos>' INTO xml.
    ENDIF.

    me->total_impuestos( CHANGING xml = xml ).

    IF me->gs_header_d-valor IS NOT INITIAL.
      CONCATENATE xml '<valorTotal>' me->gs_header_d-valor '</valorTotal>' INTO xml.
    ENDIF.

    me->pagos( CHANGING xml = xml ).

    IF me->gt_motivos[] IS NOT INITIAL.
        CONCATENATE xml '<motivos>' INTO xml.
        LOOP AT me->gt_motivos INTO gs_motivos.
          CONCATENATE xml '<motivo>' INTO xml.
          CONCATENATE xml '<razon>' gs_motivos-razon '</razon>' INTO xml.
          CONCATENATE xml '<valor>' gs_motivos-valor '</valor>' INTO xml.
          CONCATENATE xml '</motivo>' INTO xml.
        ENDLOOP.
        CONCATENATE xml '</motivos>' INTO xml.
    ENDIF.
  ENDMETHOD.


  METHOD pagos.

    IF me->gt_pagos[] IS NOT INITIAL.

      CONCATENATE xml '<pagos>' INTO xml.

      LOOP AT me->gt_pagos INTO gs_pagos.

        CONCATENATE xml '<pago>' INTO xml.

        IF gs_pagos-formapago IS NOT INITIAL.
          CONCATENATE xml '<formaPago>' gs_pagos-formapago  '</formaPago>' INTO xml.
        ENDIF.

        IF gs_pagos-total IS NOT INITIAL.
          CONCATENATE xml '<total>' gs_pagos-total  '</total>' INTO xml.
        ENDIF.

        IF gs_pagos-plazo IS NOT INITIAL.
          CONCATENATE xml '<plazo>' gs_pagos-plazo  '</plazo>' INTO xml.
        ENDIF.

        IF gs_pagos-unidadtiempo IS NOT INITIAL.
          CONCATENATE xml '<unidadTiempo>' gs_pagos-unidadtiempo  '</unidadTiempo>' INTO xml.
        ENDIF.

        CONCATENATE xml '</pago>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</pagos>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD retencion.

    gs_header_r     = header.
    gs_inf_tribu    = inf_tribu.
    gt_sustento[]   = sustento[].
    gt_impuesto[]   = impuesto[].
    gt_retencion[]  = retencion[].
    gt_reembolso[]  = reembolso[].
    gt_reem_imp[]   = reem_imp[].
    gt_reem_imp[]   = reem_imp[].
    gt_pagos[]      = pagos[].
    gt_head_add[]   = head_add[].

     xml = '<comprobanteRetencion id="comprobante" version="2.0.0">' .

    IF me->gs_header_r IS NOT INITIAL.

      IF me->gs_header_r IS NOT INITIAL."Cabecera de Retenciones

        me->retencionHeader( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_sustento[] IS NOT INITIAL."Detalles de Retenciones

        me->retencionSustento( CHANGING xml = xml ).

      ENDIF.

      IF me->gt_head_add IS NOT INITIAL."Detalles adicionales de Cabecera

        me->addfields( CHANGING xml = xml ).

      ENDIF.

    ENDIF.

    CONCATENATE xml '</comprobanteRetencion>' INTO xml.
    "CONDENSE xml NO-GAPS.

  ENDMETHOD.


  METHOD retencionDetalle.

    CONCATENATE xml '<retenciones>' INTO xml.

     LOOP AT me->gt_retencion INTO gs_retencion WHERE numdocsustento EQ numdocsustento.
       CONCATENATE xml '<retencion>' INTO xml.

       IF gs_retencion-codigo IS NOT INITIAL.
         CONCATENATE xml '<codigo>' gs_retencion-codigo '</codigo>' INTO xml.
       ENDIF.

       IF gs_retencion-codigoretencion IS NOT INITIAL.
         CONCATENATE xml '<codigoRetencion>' gs_retencion-codigoretencion '</codigoRetencion>' INTO xml.
       ENDIF.

       IF gs_retencion-baseimponible IS NOT INITIAL.
         CONCATENATE xml '<baseImponible>' gs_retencion-baseimponible '</baseImponible>' INTO xml.
       ENDIF.

       IF gs_retencion-porcentajeretener IS NOT INITIAL.
         CONCATENATE xml '<porcentajeRetener>' gs_retencion-porcentajeretener '</porcentajeRetener>' INTO xml.
       ENDIF.

       IF gs_retencion-valorretenido IS NOT INITIAL.
         CONCATENATE xml '<valorRetenido>' gs_retencion-valorretenido '</valorRetenido>' INTO xml.
       ENDIF.

       IF gs_retencion-fechapagodiv IS NOT INITIAL.
         CONCATENATE xml '<dividendos>' INTO xml.
         IF gs_retencion-fechapagodiv IS NOT INITIAL.
           CONCATENATE xml '<fechaPagoDiv>' gs_retencion-fechapagodiv '</fechaPagoDiv>' INTO xml.
         ENDIF.

         IF gs_retencion-imrentasoc IS NOT INITIAL.
            CONCATENATE xml '<imRentaSoc>' gs_retencion-imrentasoc '</imRentaSoc>' INTO xml.
         ENDIF.

         IF gs_retencion-valorretenido IS NOT INITIAL.
            CONCATENATE xml '<ejerFisUtDiv>' gs_retencion-ejerfisutdiv '</ejerFisUtDiv>' INTO xml.
         ENDIF.
         CONCATENATE xml '</dividendos>' INTO xml.
       ENDIF.

       IF gs_retencion-numcajban IS NOT INITIAL.
         CONCATENATE xml '<compraCajBanano>' INTO xml.

         IF gs_retencion-numcajban IS NOT INITIAL.
            CONCATENATE xml '<NumCajBan>' gs_retencion-numcajban '</NumCajBan>' INTO xml.
         ENDIF.

         IF gs_retencion-preccajban IS NOT INITIAL.
            CONCATENATE xml '<PrecCajBan>' gs_retencion-preccajban '</PrecCajBan>' INTO xml.
         ENDIF.
         CONCATENATE xml '</compraCajBanano>' INTO xml.
       ENDIF.

       CONCATENATE xml '</retencion>' INTO xml.
     ENDLOOP.

     CONCATENATE xml '</retenciones>' INTO xml.

  ENDMETHOD.

  METHOD retencionHeader.

    me->infoTributaria( CHANGING xml = xml ).

    CONCATENATE xml '<infoCompRetencion>' INTO xml.

    IF me->gs_header_r-fechaemision IS NOT INITIAL.
      CONCATENATE xml '<fechaEmision>' me->gs_header_r-fechaemision '</fechaEmision>' INTO xml.
    ENDIF.

    IF me->gs_header_r-direstablecimiento IS NOT INITIAL.
      CONCATENATE xml '<dirEstablecimiento>' me->gs_header_r-direstablecimiento '</dirEstablecimiento>' INTO xml.
    ENDIF.

    IF me->gs_header_r-contribuyenteespecial IS NOT INITIAL.
      CONCATENATE xml '<contribuyenteEspecial>' me->gs_header_r-contribuyenteespecial '</contribuyenteEspecial>' INTO xml.
    ENDIF.

    IF me->gs_header_r-obligadocontabilidad IS NOT INITIAL.
      CONCATENATE xml '<obligadoContabilidad>' me->gs_header_r-obligadocontabilidad '</obligadoContabilidad>' INTO xml.
    ENDIF.

    IF me->gs_header_r-tipoidentificacionsujetoreteni IS NOT INITIAL.
      CONCATENATE xml '<tipoIdentificacionSujetoRetenido>' me->gs_header_r-tipoidentificacionsujetoreteni
                      '</tipoIdentificacionSujetoRetenido>' INTO xml.
    ENDIF.

    IF me->gs_header_r-tiposujetoretenido IS NOT INITIAL.
      CONCATENATE xml '<tipoSujetoRetenido>' me->gs_header_r-tiposujetoretenido '</tipoSujetoRetenido>' INTO xml.
    ENDIF.

    IF me->gs_header_r-parterel IS NOT INITIAL.
      CONCATENATE xml '<parteRel>' me->gs_header_r-parterel '</parteRel>' INTO xml.
    ENDIF.

    IF me->gs_header_r-razonsocialsujetoretenido IS NOT INITIAL.
      CONCATENATE xml '<razonSocialSujetoRetenido>' me->gs_header_r-razonsocialsujetoretenido
                      '</razonSocialSujetoRetenido>' INTO xml.
    ENDIF.

    IF me->gs_header_r-identificacionsujetoretenido IS NOT INITIAL.
      CONCATENATE xml '<identificacionSujetoRetenido>' me->gs_header_r-identificacionsujetoretenido
                      '</identificacionSujetoRetenido>' INTO xml.
    ENDIF.

    IF me->gs_header_r-periodofiscal IS NOT INITIAL.
      CONCATENATE xml '<periodoFiscal>' me->gs_header_r-periodofiscal '</periodoFiscal>' INTO xml.
    ENDIF.

    CONCATENATE xml '</infoCompRetencion>' INTO xml.

  ENDMETHOD.


  METHOD retencionSustento.

    CONCATENATE xml '<docsSustento>' INTO xml.

    LOOP AT me->gt_sustento INTO gs_sustento.

      CONCATENATE xml '<docSustento>' INTO xml.

      IF gs_sustento-codsustento IS NOT INITIAL.
        CONCATENATE xml '<codSustento>' gs_sustento-codsustento '</codSustento>' INTO xml.
      ENDIF.

      IF gs_sustento-coddocsustento IS NOT INITIAL.
        CONCATENATE xml '<codDocSustento>' gs_sustento-coddocsustento '</codDocSustento>' INTO xml.
      ENDIF.

      IF gs_sustento-numdocsustento IS NOT INITIAL.
        CONCATENATE xml '<numDocSustento>' gs_sustento-numdocsustento '</numDocSustento>' INTO xml.
      ENDIF.

      IF gs_sustento-fechaemisiondocsustento IS NOT INITIAL.
        CONCATENATE xml '<fechaEmisionDocSustento>' gs_sustento-fechaemisiondocsustento '</fechaEmisionDocSustento>' INTO xml.
      ENDIF.

      IF gs_sustento-fecharegistrocontable IS NOT INITIAL.
        CONCATENATE xml '<fechaRegistroContable>' gs_sustento-fecharegistrocontable '</fechaRegistroContable>' INTO xml.
      ENDIF.

      IF gs_sustento-numautdocsustento IS NOT INITIAL.
        CONCATENATE xml '<numAutDocSustento>' gs_sustento-numautdocsustento '</numAutDocSustento>' INTO xml.
      ENDIF.

      IF gs_sustento-pagolocext IS NOT INITIAL.
        CONCATENATE xml '<pagoLocExt>' gs_sustento-pagolocext '</pagoLocExt>' INTO xml.
      ENDIF.

      IF gs_sustento-tiporegi IS NOT INITIAL.
        CONCATENATE xml '<tipoRegi>' gs_sustento-tiporegi '</tipoRegi>' INTO xml.
      ENDIF.

      IF gs_sustento-paisefecpago IS NOT INITIAL.
        CONCATENATE xml '<paisEfecPago>' gs_sustento-paisefecpago '</paisEfecPago>' INTO xml.
      ENDIF.

      IF gs_sustento-aplicconvdobtrib IS NOT INITIAL.
        CONCATENATE xml '<aplicConvDobTrib>' gs_sustento-aplicconvdobtrib '</aplicConvDobTrib>' INTO xml.
      ENDIF.

      IF gs_sustento-pagextsujretnorleg IS NOT INITIAL AND gs_sustento-aplicconvdobtrib EQ 'NO' AND
          gs_sustento-pagolocext EQ '02'.
        CONCATENATE xml '<pagExtSujRetNorLeg>' gs_sustento-pagextsujretnorleg '</pagExtSujRetNorLeg>' INTO xml.
      ENDIF.

      IF gs_sustento-pagoregfis IS NOT INITIAL.
        CONCATENATE xml '<pagoRegFis>' gs_sustento-pagoregfis '</pagoRegFis>' INTO xml.
      ENDIF.

      IF gs_sustento-totalcomprobantesreembolso IS NOT INITIAL.
        CONCATENATE xml '<totalComprobantesReembolso>' gs_sustento-totalcomprobantesreembolso '</totalComprobantesReembolso>' INTO xml.
      ENDIF.

      IF gs_sustento-totalbaseimponiblereembolso IS NOT INITIAL.
        CONCATENATE xml '<totalBaseImponibleReembolso>' gs_sustento-totalbaseimponiblereembolso '</totalBaseImponibleReembolso>' INTO xml.
      ENDIF.

      IF gs_sustento-totalimpuestoreembolso IS NOT INITIAL.
        CONCATENATE xml '<totalImpuestoReembolso>' gs_sustento-totalimpuestoreembolso '</totalImpuestoReembolso>' INTO xml.
      ENDIF.

      IF gs_sustento-totalsinimpuestos IS NOT INITIAL.
        CONCATENATE xml '<totalSinImpuestos>' gs_sustento-totalsinimpuestos '</totalSinImpuestos>' INTO xml.
      ENDIF.

      IF gs_sustento-importetotal IS NOT INITIAL.
        CONCATENATE xml '<importeTotal>' gs_sustento-importetotal '</importeTotal>' INTO xml.
      ENDIF.

      me->totalimpuestosust( EXPORTING numdocsustento = gs_sustento-numdocsustento CHANGING xml = xml ).

      me->retenciondetalle( EXPORTING numdocsustento = gs_sustento-numdocsustento CHANGING xml = xml ).

      IF me->gt_reembolso IS NOT INITIAL."Detalles Reembolsos de Retencion
        me->factura_reembolso( CHANGING xml = xml ).
      ENDIF.

      me->pagos( CHANGING xml = xml ).

      CONCATENATE xml '</docSustento>' INTO xml.

    ENDLOOP.

    CONCATENATE xml '</docsSustento>' INTO xml.

  ENDMETHOD.


  METHOD totalImpuestoSust.

    IF me->gt_imp_sust[] IS NOT INITIAL.

      CONCATENATE xml '<impuestosDocSustento>' INTO xml.

      LOOP AT me->gt_imp_sust INTO gs_imp_sust WHERE numdocsustento EQ numdocsustento.

        CONCATENATE xml '<impuestoDocSustento>' INTO xml.

        IF gs_impuesto-codigo IS NOT INITIAL.
          CONCATENATE xml '<codImpuestoDocSustento>' gs_impuesto-codigo '</codImpuestoDocSustento>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigoporcentaje  IS NOT INITIAL.
          CONCATENATE xml '<codigoPorcentaje>' gs_impuesto-codigoporcentaje '</codigoPorcentaje>' INTO xml.
        ENDIF.

        IF gs_impuesto-baseimponible IS NOT INITIAL.
          CONCATENATE xml '<baseImponible>' gs_impuesto-baseimponible '</baseImponible>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigo NE '1' AND gs_impuesto-tarifa IS NOT INITIAL .
          CONCATENATE xml '<tarifa>' gs_impuesto-tarifa '</tarifa>' INTO xml.
        ENDIF.

        IF gs_impuesto-valor IS NOT INITIAL.
          CONCATENATE xml '<valor>' gs_impuesto-valor '</valor>' INTO xml.
        ENDIF.

        CONCATENATE xml '</impuestoDocSustento>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</impuestosDocSustento>' INTO xml.

    ENDIF.

  ENDMETHOD.


  METHOD total_con_impuesto.

    IF me->gt_impuesto[] IS NOT INITIAL.

      CONCATENATE xml '<totalConImpuestos>' INTO xml.

      LOOP AT me->gt_impuesto INTO gs_impuesto.

        CONCATENATE xml '<totalImpuesto>' INTO xml.

        IF gs_impuesto-codigo IS NOT INITIAL.
          CONCATENATE xml '<codigo>' gs_impuesto-codigo '</codigo>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigoporcentaje  IS NOT INITIAL.
          CONCATENATE xml '<codigoPorcentaje>' gs_impuesto-codigoporcentaje '</codigoPorcentaje>' INTO xml.
        ENDIF.

        IF gs_impuesto-baseimponible IS NOT INITIAL.
          CONCATENATE xml '<baseImponible>' gs_impuesto-baseimponible '</baseImponible>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigo NE '1' AND gs_impuesto-tarifa IS NOT INITIAL .
          CONCATENATE xml '<tarifa>' gs_impuesto-tarifa '</tarifa>' INTO xml.
        ENDIF.

        IF gs_impuesto-valor IS NOT INITIAL.
          CONCATENATE xml '<valor>' gs_impuesto-valor '</valor>' INTO xml.
        ENDIF.

        CONCATENATE xml '</totalImpuesto>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</totalConImpuestos>' INTO xml.

    ENDIF.

  ENDMETHOD.

  METHOD total_impuestos.

    IF me->gt_impuesto[] IS NOT INITIAL.

      CONCATENATE xml '<impuestos>' INTO xml.

      LOOP AT me->gt_impuesto INTO gs_impuesto.

        CONCATENATE xml '<impuesto>' INTO xml.

        IF gs_impuesto-codigo IS NOT INITIAL.
          CONCATENATE xml '<codigo>' gs_impuesto-codigo '</codigo>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigoporcentaje  IS NOT INITIAL.
          CONCATENATE xml '<codigoPorcentaje>' gs_impuesto-codigoporcentaje '</codigoPorcentaje>' INTO xml.
        ENDIF.

        IF gs_impuesto-baseimponible IS NOT INITIAL.
          CONCATENATE xml '<baseImponible>' gs_impuesto-baseimponible '</baseImponible>' INTO xml.
        ENDIF.

        IF gs_impuesto-codigo NE '1' AND gs_impuesto-tarifa IS NOT INITIAL .
          CONCATENATE xml '<tarifa>' gs_impuesto-tarifa '</tarifa>' INTO xml.
        ENDIF.

        IF gs_impuesto-valor IS NOT INITIAL.
          CONCATENATE xml '<valor>' gs_impuesto-valor '</valor>' INTO xml.
        ENDIF.

        CONCATENATE xml '</impuesto>' INTO xml.

      ENDLOOP.

      CONCATENATE xml '</impuestos>' INTO xml.

    ENDIF.

  ENDMETHOD.

  METHOD FACTURA_EXP_DATIL.

*    Solo para Facturas de Exportacion de Datil

    IF me->gs_header_f-comercioexterior IS NOT INITIAL.

      CONCATENATE xml '<exportacion>' INTO xml.
*      CONCATENATE xml '<comercioExterior>' me->gs_header_f-comercioexterior '</comercioExterior>' INTO xml.

      IF me->gs_header_f-incotermfactura IS NOT INITIAL.
        CONCATENATE xml '<incoTerm>' me->gs_header_f-incotermfactura '</incoTerm>' INTO xml.
      ENDIF.

      IF me->gs_header_f-lugarincoterm IS NOT INITIAL.
        CONCATENATE xml '<incoTermLugar>' me->gs_header_f-lugarincoterm '</incoTermLugar>' INTO xml.
      ENDIF.

      IF me->gs_header_f-paisorigen IS NOT INITIAL.
        CONCATENATE xml '<paisOrigen>' me->gs_header_f-paisorigen '</paisOrigen>' INTO xml.
      ENDIF.

      IF me->gs_header_f-puertoembarque IS NOT INITIAL.
        CONCATENATE xml '<puertoOrigen>' me->gs_header_f-puertoembarque '</puertoOrigen>' INTO xml.
      ENDIF.

      IF me->gs_header_f-paisdestino IS NOT INITIAL.
        CONCATENATE xml '<paisDestino>' me->gs_header_f-paisdestino '</paisDestino>' INTO xml.
      ENDIF.

      IF me->gs_header_f-incotermtotalsin IS NOT INITIAL.
        CONCATENATE xml '<incoTermTotalSinImpuestos>' me->gs_header_f-incotermtotalsin '</incoTermTotalSinImpuestos>' INTO xml.
      ENDIF.

      IF me->gs_header_f-puertodestino IS NOT INITIAL.
        CONCATENATE xml '<puertoDestino>' me->gs_header_f-puertodestino '</puertoDestino>' INTO xml.
      ENDIF.

      IF me->gs_header_f-paisadqusicion IS NOT INITIAL.
        CONCATENATE xml '<paisAdquisicion>' me->gs_header_f-paisadqusicion '</paisAdquisicion>' INTO xml.
      ENDIF.

      IF me->gs_header_f-fleteinternacional IS NOT INITIAL.
        CONCATENATE xml '<fleteInternacional>' me->gs_header_f-fleteinternacional '</fleteInternacional>' INTO xml.
      ENDIF.

      IF me->gs_header_f-segurointernacional IS NOT INITIAL.
        CONCATENATE xml '<seguroInternacional>' me->gs_header_f-segurointernacional '</seguroInternacional>' INTO xml.
      ENDIF.

      IF me->gs_header_f-gastosaduaneros IS NOT INITIAL.
        CONCATENATE xml '<gastosAduaneros>' me->gs_header_f-gastosaduaneros '</gastosAduaneros>' INTO xml.
      ENDIF.

      IF me->gs_header_f-gastostransporteotros IS NOT INITIAL.
        CONCATENATE xml '<gastosTransporteOtros>' me->gs_header_f-gastostransporteotros '</gastosTransporteOtros>' INTO xml.
      ENDIF.

      CONCATENATE xml '</exportacion>' INTO xml.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
