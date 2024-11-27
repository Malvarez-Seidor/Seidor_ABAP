CLASS zcl_create_factura DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
           ty_pagos     TYPE STANDARD TABLE OF zts_pago,
           ty_detalle_f TYPE STANDARD TABLE OF zts_fac_detalle,
           ty_det_add   TYPE STANDARD TABLE OF zts_det_add,
           ty_det_imp   TYPE STANDARD TABLE OF zts_det_imp,
           ty_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
           ty_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
           ty_head_add  TYPE STANDARD TABLE OF zts_head_add.


    DATA: gv_companycode            TYPE bukrs,
          gv_documenttype           TYPE zde_trsri,
          gv_fiscalyear             TYPE gjahr,
          gv_accountingdocument     TYPE belnr_d,
          gv_accountingdocumenttype TYPE blart,
          gv_billingdocument        TYPE vbeln,
          gv_billingdocumenttype    TYPE fkart,
          gv_deliverydocument       TYPE vbeln,
          gv_usuname                TYPE c LENGTH 12,
          gv_deliverydocumenttype   TYPE fkart.

    METHODS constructor IMPORTING companycode            TYPE bukrs             "Sociedad
                                  fiscalyear             TYPE gjahr   OPTIONAL  "Ejercicio de Docuemnto
                                  accountingdocument     TYPE belnr_d OPTIONAL  "Documento de Financiero
                                  accountingdocumenttype TYPE blart   OPTIONAL  "Tipo Documento de Financiero
                                  billingdocument        TYPE vbeln   OPTIONAL  "Documento de Ventas
                                  billingdocumenttype    TYPE fkart   OPTIONAL. "Tipo Documento Ventas

    METHODS callDocumentType IMPORTING documenttype TYPE zde_trsri                        "Tipo de Documento SRI
                             EXPORTING inf_tribu    TYPE zts_inf_tribu                    "si cabecera
                                       factura      TYPE zts_fac_header                   "si cabecera
                                       t_impuesto   TYPE zcl_create_factura=>ty_impuesto  "si cabecera total impuesto
                                       t_pagos      TYPE zcl_create_factura=>ty_pagos     "si cabecera pagos
                                       t_detalle    TYPE zcl_create_factura=>ty_detalle_f "si detalles
                                       t_det_add    TYPE zcl_create_factura=>ty_det_add   "si detalles
                                       t_det_imp    TYPE zcl_create_factura=>ty_det_imp   "si detalles
                                       t_reembolso  TYPE zcl_create_factura=>ty_reembolso "si detalles
                                       t_reem_imp   TYPE zcl_create_factura=>ty_reem_imp  "si detalles de impuesto
                                       t_head_add   TYPE zcl_create_factura=>ty_head_add. "si cebecera datos adicionales

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_header_f  TYPE zts_fac_header,
          gs_inf_tribu TYPE zts_inf_tribu,
          gs_impuesto  TYPE zts_total_imp,
          gs_pagos     TYPE zts_pago,
          gs_detalle_f TYPE zts_fac_detalle,
          gs_det_add   TYPE zts_det_add,
          gs_det_imp   TYPE zts_det_imp,
          gs_reembolso TYPE zts_fac_det_reembolso,
          gs_reem_imp  TYPE zts_reem_imp,
          gs_head_add  TYPE zts_head_add,
          gs_ec_001    TYPE zdt_ec_001,
          gs_ec_002    TYPE zdt_ec_002,
          gs_ec_003    TYPE zdt_ec_003,
          gs_ec_004    TYPE zdt_ec_004,
          gs_ec_005    TYPE zdt_ec_005,
          gs_ec_006    TYPE zdt_ec_006,
          gs_ec_007    TYPE zdt_ec_007,
          gs_ec_008    TYPE zdt_ec_008,
          gs_ec_009    TYPE zdt_ec_009.


    DATA: gt_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
          gt_pagos     TYPE STANDARD TABLE OF zts_pago,
          gt_detalle_f TYPE STANDARD TABLE OF zts_fac_detalle,
          gt_det_add   TYPE STANDARD TABLE OF zts_det_add,
          gt_det_imp   TYPE STANDARD TABLE OF zts_det_imp,
          gt_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
          gt_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
          gt_head_add  TYPE STANDARD TABLE OF zts_head_add,
          gt_ec_001    TYPE STANDARD TABLE OF zdt_ec_001,
          gt_ec_002    TYPE STANDARD TABLE OF zdt_ec_002,
          gt_ec_003    TYPE STANDARD TABLE OF zdt_ec_003,
          gt_ec_004    TYPE STANDARD TABLE OF zdt_ec_004,
          gt_ec_005    TYPE STANDARD TABLE OF zdt_ec_005,
          gt_ec_006    TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_007    TYPE STANDARD TABLE OF zdt_ec_007,
          gt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          gt_ec_009    TYPE STANDARD TABLE OF zdt_ec_009.

    DATA: gs_factura                  TYPE zdt_sd_doc_fac,
          gs_SalesDocument            TYPE I_SalesDocument,
          gs_SalesDocumentItem        TYPE I_SalesDocumentItem,
          gs_BillingDocument          TYPE I_BillingDocument,
          gs_BillingDocumentItem      TYPE I_BillingDocumentItem,
          gs_BillingItemPrcgElmnt     TYPE I_BillingDocumentItemPrcgElmnt,
          gs_JournalEntry             TYPE I_JournalEntry,
          gs_JournalEntryItem         TYPE I_JournalEntryItem,
          gs_JournalEntryItemTAx      TYPE I_JournalEntryItem,
          gs_JournalEntryItemPartner  TYPE I_JournalEntryItem,
          gs_CompanyCode              TYPE I_CompanyCode,
          gs_AddlInformation          TYPE I_AddlCompanyCodeInformation,
          gs_BusinessPartner          TYPE I_BusinessPartner,
          gs_Address                  TYPE i_address_2,
          gs_email                    TYPE I_AddressEmailAddress_2,
          gs_telefono                 TYPE I_AddressPhoneNumber_2,
          gs_Businesspartnertaxnumber TYPE I_Businesspartnertaxnumber,
          gs_BusPartAddress           TYPE I_BusPartAddress,
          gs_PaymentTerms             TYPE I_PaymentTermsConditions.

    DATA: gt_SalesDocumentItem        TYPE STANDARD TABLE OF I_SalesDocumentItem,
          gt_BillingDocumentItem      TYPE STANDARD TABLE OF I_BillingDocumentItem,
          gt_BillingItemPrcgElmnt     TYPE STANDARD TABLE OF I_BillingDocumentItemPrcgElmnt,
          gt_JournalEntry             TYPE STANDARD TABLE OF I_JournalEntry,
          gt_JournalEntryItem         TYPE STANDARD TABLE OF I_JournalEntryItem,
          gt_JournalEntryItemTax      TYPE STANDARD TABLE OF I_JournalEntryItem,
          gt_JournalEntryItemPartner  TYPE STANDARD TABLE OF I_JournalEntryItem,
          gt_AddlInformation          TYPE STANDARD TABLE OF I_AddlCompanyCodeInformation,
          gt_BusinessPartner          TYPE STANDARD TABLE OF I_BusinessPartner,
          gt_ADDRESS                  TYPE STANDARD TABLE OF i_address_2,
          gt_email                    TYPE STANDARD TABLE OF I_AddressEmailAddress_2,
          gt_telefono                 TYPE STANDARD TABLE OF I_AddressPhoneNumber_2,
          gt_Businesspartnertaxnumber TYPE STANDARD TABLE OF I_Businesspartnertaxnumber,
          gt_PaymentTerms             TYPE STANDARD TABLE OF I_PaymentTermsConditions.

    METHODS get_data .

    METHODS infoTributaria  CHANGING inf_tribu   TYPE zts_inf_tribu.

    METHODS getClaveAcceso  IMPORTING inf_tribu   TYPE zts_inf_tribu
                                      fecha       TYPE string
                                      api         TYPE zde_type_api
                            CHANGING  estab       TYPE zts_inf_tribu-estab
                                      ptoemi      TYPE zts_inf_tribu-ptoemi
                                      secuencial  TYPE zts_inf_tribu-secuencial
                                      claveacceso TYPE zts_inf_tribu-claveacceso.

    METHODS getHeaderFactura   CHANGING header_f  TYPE zts_fac_header
                                        reembolso TYPE zcl_create_factura=>ty_reembolso
                                        reem_imp  TYPE zcl_create_factura=>ty_reem_imp.
    METHODS getDatosBP         CHANGING header_f   TYPE zts_fac_header.
    METHODS getTotalImpuestos  CHANGING impuesto   TYPE zcl_create_factura=>ty_impuesto.
    METHODS getImpotes         CHANGING header_f   TYPE zts_fac_header.
    METHODS getExportacion     CHANGING header_f   TYPE zts_fac_header.
    METHODS getDetalles        CHANGING detalle   TYPE zcl_create_factura=>ty_detalle_f
                                        detalle_i TYPE zcl_create_factura=>ty_det_imp
                                        detalle_a TYPE zcl_create_factura=>ty_det_add.
    METHODS getReembolso       CHANGING header_f  TYPE zts_fac_header
                                        reembolso TYPE zcl_create_factura=>ty_reembolso
                                        reem_imp  TYPE zcl_create_factura=>ty_reem_imp.
    METHODS getViaPago         CHANGING pagos      TYPE zcl_create_factura=>ty_pagos.
    METHODS getHeaderAdd       CHANGING header_add TYPE zcl_create_factura=>ty_head_add.

ENDCLASS.



CLASS ZCL_CREATE_FACTURA IMPLEMENTATION.


  METHOD calldocumenttype.

    gv_documenttype =  documenttype.

    me->get_data( ).

    me->infoTributaria( CHANGING inf_tribu = me->gs_inf_tribu ).

    me->getHeaderFactura(  CHANGING header_f      = me->gs_header_f
                                    reembolso     = me->gt_reembolso
                                    reem_imp      = me->gt_reem_imp ).
    me->gettotalimpuestos( CHANGING impuesto      = me->gt_impuesto ).
    me->getdetalles(       CHANGING detalle       = me->gt_detalle_f
                                    detalle_i     = me->gt_det_imp
                                    detalle_a     = me->gt_det_add ).
    me->getViaPago(        CHANGING pagos         = me->gt_pagos    ).
    me->getHeaderAdd(      CHANGING header_add    = me->gt_head_add ).

    factura     = me->gs_header_f.
    inf_tribu   = me->gs_inf_tribu.
    t_impuesto  = me->gt_impuesto.
    t_head_add  = me->gt_head_add.
    t_detalle   = me->gt_detalle_f.
    t_det_imp   = me->gt_det_imp.
    t_det_add   = me->gt_det_add.
    t_reembolso = me->gt_reembolso.
    t_reem_imp  = me->gt_reem_imp.
    t_pagos     = me->gt_pagos.

  ENDMETHOD.


  METHOD constructor.

    gv_companycode            = companycode.
    gv_fiscalyear             = fiscalyear.
    gv_accountingdocument     = accountingdocument.
    gv_accountingdocumenttype = accountingdocumenttype.
    gv_billingdocument        = billingdocument.
    gv_billingdocumenttype    = billingdocumenttype.

  ENDMETHOD.


  METHOD getclaveacceso.

    DATA: lv_num     TYPE i,
          lv_sum     TYPE p,
          lv_sumt    TYPE i,
          lv_val     TYPE p,
          lv_six     TYPE i,
          lv_mod     TYPE i,
          lv_rep     TYPE p,
          lv_rep2(2) TYPE c.

    CASE me->gv_documenttype.
      WHEN '01'.
        IF me->gs_factura IS NOT INITIAL.
          estab       = me->gs_factura-establishment.
          ptoemi      = me->gs_factura-emissionpoint.
          secuencial  = me->gs_factura-sequential.
          claveacceso = me->gs_factura-accesskey.
          EXIT.
        ENDIF.
    ENDCASE.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = api fieldname = 'COD_NUM'. "Codigo Numerico
    IF sy-subrc EQ 0.
      DATA(lv_cod) = gs_ec_007-low.
    ENDIF.

    IF me->gs_ec_008 IS NOT INITIAL.
      estab  = gs_ec_008-establishment.
      ptoemi = gs_ec_008-emissionpoint.
    ENDIF.

    IF me->gs_ec_002 IS NOT INITIAL.

      TRY.
          CALL METHOD cl_numberrange_runtime=>number_get
            EXPORTING
              nr_range_nr = '01'
              object      = me->gs_ec_002-Objet
            IMPORTING
              number      = DATA(lv_number)
              returncode  = DATA(lv_rcode).

          IF lv_number IS NOT INITIAL.
            DATA(lv_len)  = strlen( lv_number ).
            DATA(lv_cant) = lv_len - 9.
            IF lv_cant LE 0.
              lv_cant = 0.
            ENDIF.
            IF lv_len GT 9.
              lv_len = 9.
            ENDIF.
            secuencial = lv_number+lv_cant(lv_len).
          ENDIF.

        CATCH cx_number_ranges INTO DATA(lr_error).

      ENDTRY.

    ENDIF.

    CONCATENATE fecha+6(2) fecha+4(2) fecha+0(4)
                inf_tribu-coddoc
                inf_tribu-ruc
                inf_tribu-ambiente
                estab
                ptoemi
                secuencial
                lv_cod
                inf_tribu-tipoemision
                INTO claveacceso.

    lv_num = strlen( claveacceso ).

    WHILE lv_num GT 0.
      lv_six = 6.
      WHILE lv_six NE 0.
        IF lv_num NE 0.
          lv_num = lv_num - 1.
          lv_val = claveacceso+lv_num(1).
          CASE lv_six.
            WHEN 6.
              lv_val = lv_val * 2.
              lv_sum = lv_sum + lv_val.
            WHEN 5.
              lv_val = lv_val * 3.
              lv_sum = lv_sum + lv_val.
            WHEN 4.
              lv_val = lv_val * 4.
              lv_sum = lv_sum + lv_val.
            WHEN 3.
              lv_val = lv_val * 5.
              lv_sum = lv_sum + lv_val.
            WHEN 2.
              lv_val = lv_val * 6.
              lv_sum = lv_sum + lv_val.
            WHEN 1.
              lv_val = lv_val * 7.
              lv_sum = lv_sum + lv_val.
          ENDCASE.
        ENDIF.
        lv_six = lv_six - 1.
      ENDWHILE.
    ENDWHILE.

    lv_mod  = lv_sum MOD 11.
    lv_rep  = 11 - lv_mod.
    lv_rep2 = lv_rep.

    IF lv_rep = 11.
      lv_rep2 = 0.
    ELSEIF lv_rep = 10.
      lv_rep2 = 1.
    ENDIF.

    CONCATENATE claveacceso lv_rep2 INTO claveacceso.

  ENDMETHOD.


  METHOD getdatosbp.

    SELECT SINGLE *
    FROM I_BusinessPartner
     WHERE BusinessPartner EQ @me->gs_billingdocument-soldtoparty
     INTO @gs_BusinessPartner.

    IF sy-subrc EQ 0.

      SELECT *
       FROM I_Businesspartnertaxnumber
        WHERE BusinessPartner EQ @me->gs_billingdocument-soldtoparty
        INTO TABLE @gt_Businesspartnertaxnumber.

      SELECT SINGLE *
       FROM I_BusPartAddress
        WHERE BusinessPartner EQ @me->gs_billingdocument-soldtoparty
        INTO @me->gs_BusPartAddress.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM i_address_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
           INTO @me->gs_Address.

        IF sy-subrc EQ 0.
          header_f-direccioncomprador = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
        ENDIF.

        SELECT *
          FROM I_AddressEmailAddress_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
           INTO TABLE @me->gt_email.

        SELECT *
          FROM I_AddressPhoneNumber_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
          INTO TABLE @me->gt_telefono.

      ENDIF.

      header_f-razonsocialcomprador = me->gs_BusinessPartner-BusinessPartnerFullName.

      READ TABLE me->gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.

        IF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
          header_f-idcomprador = gs_Businesspartnertaxnumber-BPTaxNumber.
        ELSEIF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL .
          header_f-idcomprador = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ENDIF.

        READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = '02' companycode = me->gv_companycode.
        IF sy-subrc EQ 0.
          header_f-tipoidcomprador = gs_ec_004-typedi.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD getdetalles.

    DATA: lv_tarifa               TYPE i,
          lr_condition            TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition            LIKE LINE  OF lr_condition,
          lr_conditio2            TYPE RANGE OF zdt_ec_005-ccondition,
          lr_conditio3            TYPE RANGE OF zdt_ec_005-ccondition,
          lt_BillingItemPrcgElmnt TYPE STANDARD TABLE OF I_BillingDocumentItemPrcgElmnt,
          ls_BillingItemPrcgElmnt TYPE I_BillingDocumentItemPrcgElmnt,
          lv_navnw                TYPE navnw,
          lv_precio               TYPE navnw.

    CLEAR:lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '2'  "Impuestos
                                        OR typecondition EQ '3' ."ICE

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '4'."Descuento

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_conditio2.
      CLEAR: ls_condition.

    ENDLOOP.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '1' ."Bases

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_conditio3.
      CLEAR: ls_condition.

    ENDLOOP.

    lt_billingitemprcgelmnt[] = gt_billingitemprcgelmnt[].

    LOOP AT gt_billingdocumentitem INTO gs_billingdocumentitem.

      gs_detalle_f-codigoprincipal   = gs_billingdocumentitem-Product.
*      gs_detalle_f-codigoauxiliar    = gs_billingdocumentitem-Material.
      gs_detalle_f-descripcion       = gs_billingdocumentitem-BillingDocumentItemText.
      gs_detalle_f-cantidad          = gs_billingdocumentitem-BillingQuantity.
      gs_detalle_f-unidadmedida      = gs_billingdocumentitem-BillingQuantityUnit.

      SELECT SINGLE *
      FROM I_UnitOfMeasureText
      WHERE Language EQ @sy-langu
       AND UnitOfMeasure EQ @gs_billingdocumentitem-BillingQuantityUnit
       INTO @DATA(ls_UnitOfMeasureText).
      IF sy-subrc EQ 0.
        gs_det_add-titulo = 'UnidadMedida'.
        gs_det_add-valor  = ls_UnitOfMeasureText-UnitOfMeasureLongName.
        gs_det_add-codigoprincipal  = gs_billingdocumentitem-Product.
        APPEND gs_det_add TO detalle_a.
      ENDIF.

      LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE BillingDocument         EQ gs_billingdocumentitem-BillingDocument
                                                                     AND BillingDocumentItem     EQ gs_billingdocumentitem-BillingDocumentItem
                                                                     AND ConditionInactiveReason EQ space
                                                                     AND ConditionType IN lr_condition.

        READ TABLE gt_ec_005 INTO gs_ec_005 WITH KEY ccondition = gs_billingitemprcgelmnt-ConditionType.
        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_billingitemprcgelmnt-TaxCode taxsupportid = gs_ec_005-typecondition.
        IF sy-subrc EQ 0.

          CLEAR: lv_navnw.
          LOOP AT lt_BillingItemPrcgElmnt INTO ls_BillingItemPrcgElmnt WHERE ConditionType IN lr_conditio3
                                                                         AND ConditionInactiveReason EQ space
                                                                         AND BillingDocument         EQ gs_billingitemprcgelmnt-BillingDocument
                                                                         AND BillingDocumentItem     EQ gs_billingitemprcgelmnt-BillingDocumentItem.
            lv_navnw += ls_BillingItemPrcgElmnt-ConditionAmount.

          ENDLOOP.

          gs_det_imp-baseimponible      = lv_navnw.
          gs_detalle_f-totalsinimpuesto = lv_navnw.
          CLEAR: lv_navnw.

          LOOP AT lt_BillingItemPrcgElmnt INTO ls_BillingItemPrcgElmnt WHERE ConditionType IN lr_conditio2
                                                                         AND ConditionInactiveReason EQ space
                                                                         AND BillingDocument         EQ gs_billingitemprcgelmnt-BillingDocument
                                                                         AND BillingDocumentItem     EQ gs_billingitemprcgelmnt-BillingDocumentItem.
            lv_navnw = ls_billingitemprcgelmnt-ConditionAmount.
            gs_detalle_f-descuento += abs( lv_navnw ).

          ENDLOOP.

          READ TABLE detalle_i ASSIGNING FIELD-SYMBOL(<fs_impuesto>) WITH KEY codigo =  gs_ec_003-taxsupportid codigoporcentaje = gs_ec_003-taxsidrate codigoprincipal = gs_billingdocumentitem-Product.
          IF sy-subrc EQ 0.
            lv_navnw = gs_det_imp-baseimponible.
            <fs_impuesto>-baseimponible += lv_navnw.

            lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
            <fs_impuesto>-valor         += lv_navnw.
          ELSE.
            gs_det_imp-codigoprincipal   = gs_billingdocumentitem-Product.
            gs_det_imp-codigo            = gs_ec_003-taxsupportid.
            gs_det_imp-codigoporcentaje  = gs_ec_003-taxsidrate.

            lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
            gs_det_imp-valor             = lv_navnw.

            lv_tarifa                    = gs_billingitemprcgelmnt-ConditionRateValue.
            gs_det_imp-tarifa            = lv_tarifa.
            APPEND gs_det_imp TO detalle_i.
            CLEAR: gs_det_imp.
          ENDIF.

        ENDIF.

      ENDLOOP.

      READ TABLE detalle ASSIGNING FIELD-SYMBOL(<fs_detalle>) WITH KEY codigoprincipal = gs_detalle_f-codigoprincipal codigoauxiliar = gs_detalle_f-codigoauxiliar.
      IF sy-subrc EQ 0.
        <fs_detalle>-cantidad          += gs_detalle_f-cantidad.
        <fs_detalle>-descuento         += gs_detalle_f-descuento.
        <fs_detalle>-totalsinimpuesto  += gs_detalle_f-totalsinimpuesto.
        lv_precio = ( <fs_detalle>-totalsinimpuesto + <fs_detalle>-descuento ) / <fs_detalle>-cantidad.
        <fs_detalle>-preciounitario    = lv_precio.
      ELSE.
        IF gs_detalle_f-descuento IS INITIAL.
          gs_detalle_f-descuento = '0.00'.
        ENDIF.
        lv_precio = ( gs_detalle_f-totalsinimpuesto + gs_detalle_f-descuento ) / gs_detalle_f-cantidad.
        gs_detalle_f-preciounitario    = lv_precio.
        APPEND gs_detalle_f TO detalle.
      ENDIF.
      CLEAR: gs_detalle_f.

    ENDLOOP.

    SORT detalle   BY codigoprincipal.
    SORT detalle_a BY codigoprincipal.
    DELETE ADJACENT DUPLICATES FROM detalle_a COMPARING codigoprincipal.

  ENDMETHOD.


  METHOD getexportacion.

    DATA: lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          lv_navnw     TYPE navnw,
          ls_condition LIKE LINE  OF lr_condition.

    DATA: lv_api   TYPE zde_type_api.
    lv_api = 'IN'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'EXTERIOR'.
    IF sy-subrc EQ 0.
      header_f-comercioexterior = gs_ec_007-low.
    ENDIF.

    header_f-incotermtotalsin = header_f-incotermfactura = me->gs_billingdocument-IncotermsClassification.
    header_f-lugarincoterm    = me->gs_billingdocument-IncotermsLocation1.

    "DAtil
    header_f-paisadqusicion   = header_f-paisorigen = me->gs_billingdocument-TaxDepartureCountry. "PAis Origen
    header_f-paisdestino      = me->gs_billingdocument-VATRegistrationCountry. "PAis Destino

    "NO Datil
*    READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = me->gs_billingdocument-TaxDepartureCountry.
*    IF sy-subrc EQ 0.
*      header_f-paisadqusicion   = header_f-paisorigen = gs_ec_009-countrysri.
*    ENDIF.

*    READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = me->gs_billingdocument-VATRegistrationCountry.
*    IF sy-subrc EQ 0.
*      header_f-paisdestino  = gs_ec_009-countrysri.
*    ENDIF.

    header_f-puertoembarque = me->gs_salesdocument-yy1_ptodest_sdh. "Z003 A_BillingDocumentText
    header_f-puertodestino  = me->gs_salesdocument-yy1_ptodest_sdh. "Z004 A_BillingDocumentText


    CLEAR: ls_condition, lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '5'."Flete

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    CLEAR: lv_navnw.
    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.
      lv_navnw += abs( gs_billingitemprcgelmnt-ConditionAmount ).

    ENDLOOP.
    header_f-fleteinternacional  =  lv_navnw.

    CLEAR: ls_condition, lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '6'."Seguros

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    CLEAR: lv_navnw.
    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.
      lv_navnw += abs( gs_billingitemprcgelmnt-ConditionAmount ).

    ENDLOOP.

    header_f-segurointernacional  = lv_navnw.

    CLEAR: ls_condition, lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '7'."Gastos de transportes

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    CLEAR: lv_navnw.
    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.
      lv_navnw += abs( gs_billingitemprcgelmnt-ConditionAmount ).

    ENDLOOP.

    header_f-gastosaduaneros =  lv_navnw.

    CLEAR: ls_condition, lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '8'."Otros Gastos

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    CLEAR: lv_navnw.
    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.
      lv_navnw += abs( gs_billingitemprcgelmnt-ConditionAmount ).

    ENDLOOP.

    header_f-gastostransporteotros = lv_navnw.

    CLEAR: lv_navnw.
    lv_navnw = header_f-totalsinimpuestos.
    lv_navnw += me->gs_billingdocument-TotalTaxAmount.
    header_f-importetotal = lv_navnw.

    CLEAR: lv_navnw.
    lv_navnw = header_f-totalsinimpuestos - header_f-gastostransporteotros - header_f-gastosaduaneros
             - header_f-segurointernacional - header_f-fleteinternacional.

    header_f-totalsinimpuestos = lv_navnw.

  ENDMETHOD.


  METHOD getheaderadd.

    CLEAR: gs_head_add.
    gs_head_add-valor = me->gs_billingdocument-BillingDocument.
    gs_head_add-nombre = 'Documento SAP'.
    APPEND gs_head_add TO header_add.

    CLEAR: gs_head_add.
    IF me->gs_salesdocument-yy1_observ_sdh IS NOT INITIAL.
      gs_head_add-nombre = 'Observacion'.
      gs_head_add-valor = me->gs_salesdocument-yy1_observ_sdh.
      APPEND gs_head_add TO header_add. "Z001 A_BillingDocumentText
    ENDIF.

    CLEAR: gs_head_add.
    LOOP AT me->gt_email INTO gs_email.
      IF sy-tabix EQ 1.
        CONCATENATE gs_head_add-valor gs_email-EmailAddress INTO gs_head_add-valor.
      ELSE.
        CONCATENATE gs_head_add-valor  '; ' gs_email-EmailAddress INTO gs_head_add-valor.
      ENDIF.
    ENDLOOP.
    IF gs_head_add IS NOT INITIAL.
      gs_head_add-nombre = 'Email'.
      APPEND gs_head_add TO header_add.
    ENDIF.

    CLEAR: gs_head_add.
    LOOP AT me->gt_telefono INTO gs_telefono.
      IF sy-tabix EQ 1.
        CONCATENATE  gs_head_add-valor gs_telefono-PhoneAreaCodeSubscriberNumber INTO gs_head_add-valor.
      ELSE.
        CONCATENATE gs_head_add-valor '; ' gs_telefono-PhoneAreaCodeSubscriberNumber INTO gs_head_add-valor.
      ENDIF.
    ENDLOOP.
    IF gs_head_add IS NOT INITIAL.
      gs_head_add-nombre = 'Telefono'.
      APPEND gs_head_add TO header_add.
    ENDIF.

  ENDMETHOD.


  METHOD getheaderfactura.

    DATA: lv_fecha      TYPE string,
          lv_fiscalyear TYPE gjahr,
          lv_api        TYPE zde_type_api.

    lv_api = 'IN'.

    lv_fecha = me->gs_billingdocument-BillingDocumentDate.
    header_f-fecha = me->gs_billingdocument-BillingDocumentDate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_f-fechaemision SEPARATED BY '/'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'CON_ESPECI'.
    IF sy-subrc EQ 0.
      header_f-contribuyenteespecial = gs_ec_007-low.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'OBLIG_CONT'.
    IF sy-subrc EQ 0.
      header_f-obligadocontabilidad = gs_ec_007-low.
    ENDIF.

    me->getDatosBP( CHANGING header_f = header_f ).
    me->getImpotes( CHANGING header_f = header_f ).


    READ TABLE gt_ec_001 INTO gs_ec_001 WITH KEY  CompanyCode =  me->gv_companycode documenttype = me->gv_billingdocumenttype
                                                  documentsri  = me->gv_documenttype export = me->gs_billingdocument-DistributionChannel.
    IF sy-subrc EQ 0.
      me->getExportacion( CHANGING header_f = header_f ).
    ENDIF.

    READ TABLE gt_ec_001 INTO gs_ec_001 WITH KEY  CompanyCode =  me->gv_companycode documenttype = me->gv_billingdocumenttype
                                                  documentsri  = me->gv_documenttype refunds = me->gs_billingdocument-DistributionChannel.
    IF sy-subrc EQ 0.

      me->getReembolso( CHANGING header_f = header_f
                                 reembolso = reembolso
                                 reem_imp  = reem_imp  ).

    ENDIF.

    IF me->gs_billingdocument-TransactionCurrency EQ 'USD'.
      header_f-moneda = 'DOLAR'.
    ELSE.
      header_f-moneda = me->gs_billingdocument-TransactionCurrency.
    ENDIF.

    IF me->gs_ec_002 IS NOT INITIAL.
      header_f-direstablecimiento = me->gs_ec_002-address.
    ENDIF.

    READ TABLE gt_billingdocumentitem INTO gs_billingdocumentitem WITH KEY ReferenceSDDocumentCategory = 'J'.
    IF sy-subrc EQ 0.

      me->gv_deliverydocument = gs_billingdocumentitem-ReferenceSDDocument.

      SELECT SINGLE deliverydocument, establishment, emissionpoint, sequential
      FROM zdt_sd_doc_guia
      WHERE companycode EQ @me->gv_companycode
        AND fiscalyear  EQ @me->gv_fiscalyear
        AND deliverydocument EQ @me->gv_deliverydocument
        INTO @DATA(ls_guia).

      IF sy-subrc NE 0.

        lv_fiscalyear = me->gv_fiscalyear - 1.

        SELECT SINGLE deliverydocument, establishment, emissionpoint, sequential
          FROM zdt_sd_doc_guia
          WHERE companycode EQ @me->gv_companycode
            AND fiscalyear  EQ @lv_fiscalyear
            AND deliverydocument EQ @me->gv_deliverydocument
            INTO @ls_guia.

      ENDIF.

      IF ls_guia IS NOT INITIAL.
        header_f-guiaremision = |{ ls_guia-establishment } '-'{ ls_guia-emissionpoint } '-' { ls_guia-sequential }|.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD getimpotes.

    DATA: lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          lv_navnw     TYPE navnw,
          ls_condition LIKE LINE  OF lr_condition.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '4'."Descuento

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.

      lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
      header_f-totaldescuento  += abs( lv_navnw ).

    ENDLOOP.

    IF header_f-totaldescuento IS INITIAL.
      header_f-totaldescuento = '0.00'.
    ENDIF.

    CLEAR: ls_condition, lr_condition[].
    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '9'."Propina

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    IF lr_condition[] IS NOT INITIAL.
      LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                    AND ConditionInactiveReason EQ space.

        lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
        header_f-propina  +=  abs( lv_navnw ).

      ENDLOOP.
    ENDIF.
    IF header_f-propina IS INITIAL.
      header_f-propina = '0.00'.
    ENDIF.

    header_f-totalsinimpuestos = me->gs_billingdocument-TotalNetAmount.

    header_f-importetotal = me->gs_billingdocument-TotalNetAmount + me->gs_billingdocument-TotalTaxAmount.

  ENDMETHOD.


  METHOD getreembolso.

    DATA: lv_gjahr   TYPE gjahr,
          lv_belnr   TYPE belnr_d,
          lr_belnr   TYPE RANGE OF belnr_d,
          ls_belnr   LIKE LINE OF  lr_belnr,
          lv_awkey   TYPE awkey,
          lr_awkey   TYPE RANGE OF awkey,
          ls_awkey   LIKE LINE OF  lr_awkey,
          lr_partner TYPE RANGE OF lifnr,
          ls_partner LIKE LINE OF lr_partner,
          lv_navnw   TYPE navnw,
          lv_tarifa  TYPE i,
          lv_fecha   TYPE string.

    lv_gjahr = me->gv_fiscalyear - 1.

    LOOP AT gt_salesdocumentitem INTO gs_salesdocumentitem WHERE MaterialByCustomer IS NOT INITIAL.
      lv_awkey = gs_salesdocumentitem-MaterialByCustomer.
      ls_awkey-low    = lv_awkey.
      ls_awkey-sign   = 'I'.
      ls_awkey-option = 'EQ'.
      APPEND ls_awkey TO lr_awkey.
    ENDLOOP.

    SORT lr_awkey BY low.
    DELETE ADJACENT DUPLICATES FROM lr_awkey COMPARING low.

    LOOP AT lr_awkey INTO ls_awkey.
      lv_awkey = ls_awkey-low.

      SELECT SINGLE *
      FROM I_JournalEntry
      WHERE OriginalReferenceDocument   EQ @lv_awkey
        AND CompanyCode                 EQ @me->gv_companycode
        AND FiscalYear                  EQ @me->gv_fiscalyear
        AND IsReversal                  EQ @space
        AND IsReversed                  EQ @space
       INTO @gs_journalentry.

      IF sy-subrc NE 0.

        SELECT SINGLE *
        FROM I_JournalEntry
        WHERE OriginalReferenceDocument EQ @lv_awkey
          AND CompanyCode        EQ @me->gv_companycode
*          AND FiscalYear         EQ @lv_gjahr
          AND IsReversal         EQ @space
          AND IsReversed         EQ @space
         INTO @gs_journalentry.

        IF sy-subrc EQ 0.
          SELECT  *
          FROM I_JournalEntryItem
          WHERE AccountingDocument   EQ @gs_journalentry-AccountingDocument
            AND CompanyCode          EQ @gs_journalentry-CompanyCode
            AND FiscalYear           EQ @gs_journalentry-FiscalYear
            AND Ledger               EQ '0L'
          APPENDING TABLE @gt_JournalEntryItem.
        ENDIF.

      ELSE.

        SELECT  *
        FROM I_JournalEntryItem
        WHERE AccountingDocument   EQ @gs_journalentry-AccountingDocument
          AND CompanyCode          EQ @gs_journalentry-CompanyCode
          AND FiscalYear           EQ @gs_journalentry-FiscalYear
          AND Ledger               EQ '0L'
         APPENDING TABLE @gt_JournalEntryItem.

      ENDIF.

      IF gs_journalentry IS NOT INITIAL.
        APPEND gs_journalentry TO gt_journalentry.
      ENDIF.

      CLEAR: gs_journalentry.

    ENDLOOP.

    LOOP AT gt_JournalEntryItem INTO gs_JournalEntryItem WHERE Supplier IS NOT INITIAL
                                                            OR Customer IS NOT INITIAL.

      IF gs_JournalEntryItem-Supplier IS NOT INITIAL.
        ls_partner-low    = gs_JournalEntryItem-Supplier.
      ENDIF.
      IF gs_JournalEntryItem-Customer IS NOT INITIAL.
        ls_partner-low    = gs_JournalEntryItem-Customer.
      ENDIF.
      ls_partner-sign   = 'I'.
      ls_partner-option = 'EQ'.
      APPEND ls_partner TO lr_partner.

    ENDLOOP.

    SORT lr_partner BY low.
    DELETE ADJACENT DUPLICATES FROM lr_partner COMPARING low.

    IF lr_partner[] IS NOT INITIAL.

      SELECT  *
      FROM I_BusinessPartner
      WHERE BusinessPartner IN @lr_partner
       INTO TABLE @gt_BusinessPartner.

      IF sy-subrc EQ 0.

        SELECT *
        FROM I_Businesspartnertaxnumber
        WHERE BusinessPartner IN @lr_partner
        INTO TABLE @gt_Businesspartnertaxnumber.

      ENDIF.

    ENDIF.

    header_f-coddocreemb   = '41'.

    IF gt_journalentry[] IS NOT INITIAL.

      gt_JournalEntryItemPartner[] = gt_JournalEntryItemTax[] = gt_JournalEntryItem[].

      DELETE gt_JournalEntryItemTax WHERE TransactionTypeDetermination NE 'VST' "Impuestos
                                       OR TaxCode IS INITIAL.

      DELETE gt_JournalEntryItem    WHERE TransactionTypeDetermination NE 'KBS' "Bases Imponibles
                                      AND TransactionTypeDetermination NE 'WRX'.

      DELETE gt_JournalEntryItemPartner WHERE Customer IS INITIAL
                                          AND Supplier IS INITIAL.

      LOOP AT gt_JournalEntry INTO gs_JournalEntry.

        READ TABLE gt_JournalEntryItemPartner INTO gs_JournalEntryItemPartner WITH KEY  AccountingDocument =  gs_JournalEntry-AccountingDocument.
        IF sy-subrc EQ 0.
          gs_reembolso-numeroautorizaciondocreemb = gs_JournalEntryItemPartner-DocumentItemText.
          IF gs_JournalEntryItemPartner-Supplier IS NOT INITIAL.
            READ TABLE gt_BusinessPartner INTO gs_BusinessPartner WITH KEY BusinessPartner = gs_JournalEntryItemPartner-Supplier.
            IF sy-subrc EQ 0.

              IF gs_BusinessPartner-IsNaturalPerson IS NOT INITIAL.
                gs_reembolso-tipoproveedorreembolso = '01'.
              ELSE.
                gs_reembolso-tipoproveedorreembolso = '02'.
              ENDIF.

              READ TABLE gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber WITH KEY BusinessPartner = gs_BusinessPartner-BusinessPartner.
              IF sy-subrc EQ 0.

                IF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL.
                  gs_reembolso-identificacionproveedorreembol = gs_Businesspartnertaxnumber-BPTaxLongNumber.
                ELSEIF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
                  gs_reembolso-identificacionproveedorreembol = gs_Businesspartnertaxnumber-BPTaxNumber.
                ENDIF.

                READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = '02' companycode = me->gv_companycode.
                IF sy-subrc EQ 0.
                  gs_reembolso-tipoidentificacionproveedorree = gs_ec_004-typedi.
                ENDIF.

              ENDIF.
            ENDIF.
          ELSEIF gs_JournalEntryItemPartner-Customer IS NOT INITIAL.
            READ TABLE gt_BusinessPartner INTO gs_BusinessPartner WITH KEY BusinessPartner = gs_JournalEntryItemPartner-Customer.
            IF sy-subrc EQ 0.

              IF gs_BusinessPartner-IsNaturalPerson IS NOT INITIAL.
                gs_reembolso-tipoproveedorreembolso = '01'.
              ELSE.
                gs_reembolso-tipoproveedorreembolso = '02'.
              ENDIF.

              READ TABLE gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber WITH KEY BusinessPartner = gs_BusinessPartner-BusinessPartner.
              IF sy-subrc EQ 0.

                IF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL.
                  gs_reembolso-identificacionproveedorreembol = gs_Businesspartnertaxnumber-BPTaxLongNumber.
                ELSEIF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
                  gs_reembolso-identificacionproveedorreembol = gs_Businesspartnertaxnumber-BPTaxNumber.
                ENDIF.

                READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = me->gv_documenttype companycode = me->gv_companycode.
                IF sy-subrc EQ 0.
                  gs_reembolso-tipoidentificacionproveedorree = gs_ec_004-typedi.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = me->gs_billingdocument-TaxDepartureCountry.
        IF sy-subrc EQ 0.
          gs_reembolso-codpaispagoproveedorreembolso  = gs_ec_009-countrysri.
        ENDIF.

        gs_reembolso-coddocreembolso        = '01'.

        IF strlen( gs_journalentry-DocumentReferenceID ) GE 15.
          gs_reembolso-estabdocreembolso      = gs_journalentry-DocumentReferenceID+0(3).
          gs_reembolso-ptoemidocreembolso     = gs_journalentry-DocumentReferenceID+3(3).
          gs_reembolso-secuencialdocreembolso = gs_journalentry-DocumentReferenceID+6(9).
          gs_reem_imp-secuencialdocreembolso  = |{ gs_reembolso-estabdocreembolso }{ gs_reembolso-ptoemidocreembolso }{ gs_reembolso-secuencialdocreembolso }|.
        ENDIF.

        lv_fecha = gs_journalentry-DocumentDate.
        CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_reembolso-fechaemisiondocreembolso SEPARATED BY '/'.

        LOOP AT gt_JournalEntryItemTax INTO gs_JournalEntryItemTax WHERE AccountingDocument   EQ gs_journalentry-AccountingDocument
                                                                     AND FinancialAccountType EQ 'S'
                                                                     AND TaxCode              IS NOT INITIAL.

          READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_JournalEntryItemTax-TaxCode.
          IF sy-subrc EQ 0.
            gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
            gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
          ELSE.
            CONTINUE.
          ENDIF.

          gs_reem_imp-impuestoreembolso += gs_JournalEntryItemTax-AmountInCompanyCodeCurrency.

          CLEAR: lv_navnw.
          LOOP AT gt_JournalEntryItem INTO gs_JournalEntryItem WHERE AccountingDocument   EQ gs_JournalEntryItemTax-AccountingDocument
                                                                 AND FinancialAccountType EQ 'S'
                                                                 AND TaxCode              EQ gs_JournalEntryItemTax-TaxCode.

            lv_navnw  += abs( gs_JournalEntryItem-AmountInCompanyCodeCurrency ).
          ENDLOOP.

          gs_reem_imp-baseimponiblereembolso = lv_navnw.

          CLEAR: lv_tarifa.
          IF gs_reem_imp-impuestoreembolso IS NOT INITIAL AND gs_reem_imp-impuestoreembolso GT 0.
            lv_tarifa = ( gs_reem_imp-impuestoreembolso / gs_reem_imp-baseimponiblereembolso ) * 100.
          ENDIF.

          gs_reem_imp-tarifa = lv_tarifa.

          CLEAR: lv_navnw.
          lv_navnw = gs_reem_imp-impuestoreembolso.
          header_f-totalimpuestoreembolso      += lv_navnw.

          CLEAR: lv_navnw.
          lv_navnw = gs_reem_imp-baseimponiblereembolso.
          header_f-totalbaseimponiblereembolso += lv_navnw.

          APPEND gs_reem_imp TO gt_reem_imp.
          CLEAR: gs_reem_imp-tarifa, gs_reem_imp-baseimponiblereembolso, gs_reem_imp-codigoporcentaje, gs_reem_imp-codigo,
                 gs_reem_imp-impuestoreembolso.

        ENDLOOP.

        APPEND gs_reembolso TO reembolso.
        CLEAR: gs_reem_imp, gs_reembolso.

      ENDLOOP.

    ENDIF.

    CLEAR: lv_navnw.
    lv_navnw = header_f-totalimpuestoreembolso.
    header_f-totalimpuestoreembolso = lv_navnw.

    CLEAR: lv_navnw.
    lv_navnw = header_f-totalbaseimponiblereembolso.
    header_f-totalbaseimponiblereembolso = lv_navnw.

    CLEAR: lv_navnw.
    lv_navnw = header_f-totalimpuestoreembolso + header_f-totalbaseimponiblereembolso.
    header_f-totalcomprobantesreembolso = lv_navnw.

  ENDMETHOD.


  METHOD gettotalimpuestos.

    DATA: lv_tarifa               TYPE i,
          lv_navnw                TYPE navnw,
          lr_condition            TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition            LIKE LINE  OF lr_condition,
          lr_condition2           TYPE RANGE OF zdt_ec_005-ccondition,
          lt_BillingItemPrcgElmnt TYPE STANDARD TABLE OF I_BillingDocumentItemPrcgElmnt,
          ls_BillingItemPrcgElmnt TYPE I_BillingDocumentItemPrcgElmnt.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '2'  "Impuestos
                                        OR typecondition EQ '3' ."ICE

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '1' ."Bases

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition2.
      CLEAR: ls_condition.

    ENDLOOP.

    lt_BillingItemPrcgElmnt[] = gt_BillingItemPrcgElmnt[].

    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.

      READ TABLE gt_ec_005 INTO gs_ec_005 WITH KEY ccondition = gs_billingitemprcgelmnt-ConditionType.

      READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_billingitemprcgelmnt-TaxCode taxsupportid = gs_ec_005-typecondition.
      IF sy-subrc EQ 0.

        CLEAR: lv_navnw.
        LOOP AT lt_BillingItemPrcgElmnt INTO ls_BillingItemPrcgElmnt WHERE ConditionType IN lr_condition2
                                                                       AND ConditionInactiveReason EQ space
                                                                       AND BillingDocument         EQ gs_billingitemprcgelmnt-BillingDocument
                                                                       AND BillingDocumentItem     EQ gs_billingitemprcgelmnt-BillingDocumentItem.
          lv_navnw += ls_BillingItemPrcgElmnt-ConditionAmount.

        ENDLOOP.

        gs_impuesto-baseimponible     = lv_navnw.

        READ TABLE impuesto ASSIGNING FIELD-SYMBOL(<fs_impuesto>) WITH KEY codigo =  gs_ec_003-taxsupportid codigoporcentaje =  gs_ec_003-taxsidrate.
        IF sy-subrc EQ 0.
          lv_navnw = gs_impuesto-baseimponible.
          <fs_impuesto>-baseimponible += lv_navnw.
          lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
          <fs_impuesto>-valor         += lv_navnw.
        ELSE.
          gs_impuesto-codigo            = gs_ec_003-taxsupportid.
          gs_impuesto-codigoporcentaje  = gs_ec_003-taxsidrate.

          lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
          gs_impuesto-valor             = lv_navnw.

          lv_tarifa                     = gs_billingitemprcgelmnt-ConditionRateValue.
          gs_impuesto-tarifa            = lv_tarifa.
          APPEND gs_impuesto TO impuesto.
          CLEAR: gs_impuesto.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD getviapago.

    DATA: lv_cantidad TYPE i,
          lv_navnw    TYPE navnw,
          lv_texto    TYPE c LENGTH 10.

    SELECT *
    FROM I_PaymentTermsConditions
    WHERE PaymentTerms EQ @me->gs_billingdocument-CustomerPaymentTerms
    INTO TABLE @gt_PaymentTerms.

    READ TABLE gt_ec_006 INTO gs_ec_006 WITH KEY paymentmethod = me->gs_billingdocument-paymentmethod.
    IF sy-subrc EQ 0.
      gs_pagos-formapago = gs_ec_006-paymentsri.
      gs_pagos-unidadtiempo = 'DÍAS'.
    ELSE.
      gs_pagos-formapago = '20'.
      gs_pagos-unidadtiempo = 'DÍAS'.
    ENDIF.

    SORT me->gt_PaymentTerms BY CashDiscount2AdditionalMonths DESCENDING.

    READ TABLE me->gt_PaymentTerms INTO gs_PaymentTerms INDEX 1.
    IF sy-subrc EQ 0.
      IF gs_PaymentTerms-CashDiscount2AdditionalMonths EQ 0 OR gs_PaymentTerms-CashDiscount1Days IS INITIAL.
        lv_cantidad = 1.
      ELSE.
        lv_cantidad = gs_PaymentTerms-CashDiscount2AdditionalMonths.
      ENDIF.
    ENDIF.

    SORT me->gt_PaymentTerms BY CashDiscount1Days DESCENDING.
    LOOP AT gt_PaymentTerms INTO gs_PaymentTerms.
      gs_pagos-plazo = gs_PaymentTerms-CashDiscount1Days. "Days from Baseline Date for
      lv_navnw = me->gs_header_f-importetotal / lv_cantidad.
      gs_pagos-total = lv_navnw.
      APPEND gs_pagos TO pagos.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_data.

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
    FROM zdt_ec_001
    WHERE companycode  EQ @me->gv_companycode
      AND documenttype EQ @me->gv_billingdocumenttype
      AND documentsri  EQ @me->gv_documenttype
    INTO TABLE @gt_ec_001.

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    WHERE companycode  EQ @me->gv_companycode
      AND documentsri  EQ @me->gv_documenttype
    INTO TABLE @gt_ec_002.

    SELECT client, companycode, taxcode, notax, tax0, exempttax, tax, taxsupportid, taxsidrate, taxratepercent
    FROM zdt_ec_003
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_003.

    SELECT client, companycode, bptaxtype, typedoccument, typedi
    FROM zdt_ec_004
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_004.

    SELECT client, companycode, ccondition, conditionapplication, typecondition
    FROM zdt_ec_005
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_005.

    SELECT client, companycode, paymentmethod, paymentsri
    FROM zdt_ec_006
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_006.

    SELECT client, companycode, api, fieldname, sign, options, sequence, low, high
    FROM zdt_ec_007
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_007.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_008.

    SELECT client, country, countrysri, taxhavencountry, pais_conv
    FROM zdt_ec_009
    INTO TABLE @gt_ec_009.

    SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, billingdocument, billingdocumenttype,
                  soldtoparty, businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_sd_doc_fac
      WHERE companycode             EQ @me->gv_companycode
        AND fiscalyear              EQ @me->gv_fiscalyear
        AND accountingdocument      EQ @me->gv_accountingdocument
        AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
        AND billingdocument         EQ @me->gv_billingdocument
        AND billingdocumenttype     EQ @me->gv_billingdocumenttype
       INTO @gs_factura.

    SELECT SINGLE *
    FROM I_BillingDocument
    WHERE BillingDocument     = @me->gv_billingdocument
      AND BillingDocumentType = @me->gv_billingdocumenttype
      AND fiscalyear          = @me->gv_fiscalyear
      AND companycode         = @me->gv_companycode
    INTO @me->gs_billingdocument.

    SELECT  *
    FROM I_BillingDocumentItem
    WHERE BillingDocument     = @me->gv_billingdocument
    INTO TABLE @me->gt_BillingDocumentItem.
    IF sy-subrc EQ 0.

      READ TABLE me->gt_BillingDocumentItem INTO gs_BillingDocumentItem INDEX 1.
      SELECT SINGLE *
      FROM I_SalesDocument
      WHERE SalesDocument  = @me->gs_BillingDocumentItem-SalesDocument
      INTO @me->gs_SalesDocument.

      IF sy-subrc EQ 0.

        SELECT  *
          FROM I_SalesDocumentItem
          WHERE SalesDocument  = @me->gs_SalesDocument-SalesDocument
          INTO TABLE @me->gt_salesdocumentitem.

      ENDIF.

    ENDIF.

    SELECT  *
    FROM I_BillingDocumentItemPrcgElmnt
    WHERE BillingDocument     = @me->gv_billingdocument
    INTO TABLE @me->gt_billingitemprcgelmnt.

    SELECT SINGLE *
    FROM I_CompanyCode
    WHERE companycode = @me->gv_companycode
    INTO @me->gs_CompanyCode.

    SELECT *
    FROM I_AddlCompanyCodeInformation
    WHERE companycode = @me->gv_companycode
    INTO TABLE @me->gt_AddlInformation.

  ENDMETHOD.


  METHOD infoTributaria.

    DATA: lv_api          TYPE zde_type_api,
          lv_fecha        TYPE string,
          lv_documenttype TYPE zdt_ec_001-documenttype.

    CASE me->gv_documenttype.
      WHEN '01'.
        lv_api = 'IN'.
        lv_documenttype = me->gv_billingdocumenttype.
    ENDCASE.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'AMBIENTE' low = sy-sysid. "Ambiente
    IF sy-subrc EQ 0.
      inf_tribu-ambiente = gs_ec_007-high(1).
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'TIPO_EMISI'. "Tipo Emision
    IF sy-subrc EQ 0.
      inf_tribu-tipoemision = gs_ec_007-low.
    ENDIF.

    IF me->gs_companycode IS NOT INITIAL."Razon Social
      inf_tribu-razonsocial = me->gs_companycode-CompanyCodeName.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'NOMB_COMER'.
    IF sy-subrc EQ 0.
      inf_tribu-nombrecomercial = gs_ec_007-low.
    ELSE.
      IF me->gs_companycode IS NOT INITIAL.
        inf_tribu-nombrecomercial = me->gs_companycode-CompanyCodeName.
      ENDIF.
    ENDIF.

    IF me->gt_AddlInformation[] IS NOT INITIAL."Ruc
      READ TABLE me->gt_AddlInformation INTO gs_AddlInformation WITH KEY CompanyCode =  me->gv_companycode CompanyCodeParameterType = 'CGIID'.
      IF sy-subrc EQ 0.
        inf_tribu-ruc = gs_AddlInformation-CompanyCodeParameterValue.
      ENDIF.
    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Direccion Matriz
      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'DIR_MATRIZ'.
      IF sy-subrc EQ 0.
        inf_tribu-dirmatriz = gs_ec_007-low.
      ENDIF.
    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Micro Empresa

      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'MICROEMPRE'.
      IF sy-subrc EQ 0.
        inf_tribu-regimenmicroempresas = gs_ec_007-low.
      ENDIF.

    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Agente de Retencion

      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'AG_RETENCI'.
      IF sy-subrc EQ 0.
        inf_tribu-agenteretencion = gs_ec_007-low.
      ENDIF.

    ENDIF.

    IF me->gt_ec_001[] IS NOT INITIAL."Tipo de Documento

      READ TABLE me->gt_ec_001 INTO gs_ec_001 WITH KEY CompanyCode =  me->gv_companycode documenttype = lv_documenttype.
      IF sy-subrc EQ 0.
        inf_tribu-coddoc = gs_ec_001-documentsri.
      ENDIF.

    ENDIF.

    IF gs_ec_001 IS NOT INITIAL.

      lv_fecha = me->gs_billingdocument-BillingDocumentDate.

      LOOP AT me->gt_billingdocumentitem INTO me->gs_billingdocumentitem WHERE plant IS NOT INITIAL.
        EXIT.
      ENDLOOP.

      READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_billingdocument-CompanyCode
                                                   billingdocumenttype = me->gs_billingdocument-billingdocumenttype
                                                   salesorganization   = me->gs_billingdocument-salesorganization
                                                   plant               = me->gs_billingdocumentitem-plant
                                                   documentsri         = me->gs_ec_001-documentsri
                                                   users               = sy-uname.

      IF sy-subrc NE 0.

        READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_billingdocument-CompanyCode
                                                     billingdocumenttype = me->gs_billingdocument-billingdocumenttype
                                                     plant               = me->gs_billingdocumentitem-plant
                                                     documentsri         = me->gs_ec_001-documentsri
                                                     users               = sy-uname.
        IF sy-subrc NE 0.

          READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_billingdocument-CompanyCode
                                                       billingdocumenttype = me->gs_billingdocument-billingdocumenttype
                                                       salesorganization   = me->gs_billingdocument-salesorganization
                                                       documentsri         = me->gs_ec_001-documentsri
                                                       users               = sy-uname.

        ENDIF.

      ENDIF.

      IF gs_ec_008 IS NOT INITIAL.

        READ TABLE me->gt_ec_002 INTO gs_ec_002 WITH KEY companycode    = me->gs_billingdocument-CompanyCode
                                                         establishment  = gs_ec_008-establishment
                                                         emissionpoint  = gs_ec_008-emissionpoint
                                                         documentsri    = me->gs_ec_001-documentsri.
        IF sy-subrc EQ 0.

          me->getclaveacceso( EXPORTING inf_tribu   = inf_tribu
                                        fecha       = lv_fecha
                                        api         = lv_api
                               CHANGING estab       = inf_tribu-estab
                                        ptoemi      = inf_tribu-ptoemi
                                        secuencial  = inf_tribu-secuencial
                                        claveacceso = inf_tribu-claveacceso ).
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
