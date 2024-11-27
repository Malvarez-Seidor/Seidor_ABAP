CLASS zcl_create_nota_debito DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_impuesto  TYPE STANDARD TABLE OF zts_total_imp,
           ty_pagos     TYPE STANDARD TABLE OF zts_pago,
           ty_motivos   TYPE STANDARD TABLE OF zts_nc_motivo,
           ty_head_add  TYPE STANDARD TABLE OF zts_head_add.

    DATA: gv_companycode            TYPE bukrs,
          gv_documenttype           TYPE zde_trsri,
          gv_fiscalyear             TYPE gjahr,
          gv_accountingdocument     TYPE belnr_d,
          gv_accountingdocumenttype TYPE blart,
          gv_billingdocument        TYPE vbeln,
          gv_billingdocumenttype    TYPE fkart,
          gv_usuname                TYPE c LENGTH 12.

    METHODS constructor IMPORTING companycode            TYPE bukrs             "Sociedad
                                  fiscalyear             TYPE gjahr   OPTIONAL  "Ejercicio de Docuemnto
                                  accountingdocument     TYPE belnr_d OPTIONAL  "Documento de Financiero
                                  accountingdocumenttype TYPE blart   OPTIONAL  "Tipo Documento de Financiero
                                  billingdocument        TYPE vbeln   OPTIONAL  "Documento de Ventas
                                  billingdocumenttype    TYPE fkart   OPTIONAL  "Tipo Documento Ventas
                                  deliverydocument       TYPE vbeln   OPTIONAL  "Numero Documento de Entrega Ventas
                                  deliverydocumenttype   TYPE fkart   OPTIONAL. "Tipo Documento de Entrega Ventas

    METHODS callDocumentType IMPORTING documenttype TYPE zde_trsri             "Tipo de Documento SRI
                             EXPORTING inf_tribu    TYPE zts_inf_tribu
                                       notaDebito   TYPE zts_nd_header
                                       t_impuesto   TYPE zcl_create_nota_debito=>ty_impuesto
                                       t_pagos      TYPE zcl_create_nota_debito=>ty_pagos
                                       t_motivos    TYPE zcl_create_nota_debito=>ty_motivos
                                       t_head_add   TYPE zcl_create_nota_debito=>ty_head_add.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_header_d    TYPE zts_nd_header,
          gs_inf_tribu   TYPE zts_inf_tribu,
          gs_impuesto    TYPE zts_total_imp,
          gs_pagos       TYPE zts_pago,
          gs_motivos     TYPE zts_nc_motivo,
          gs_head_add    TYPE zts_head_add,
          gs_ec_001      TYPE zdt_ec_001,
          gs_ec_002      TYPE zdt_ec_002,
          gs_ec_003      TYPE zdt_ec_003,
          gs_ec_004      TYPE zdt_ec_004,
          gs_ec_005      TYPE zdt_ec_005,
          gs_ec_006      TYPE zdt_ec_006,
          gs_ec_007      TYPE zdt_ec_007,
          gs_ec_008      TYPE zdt_ec_008.

    DATA: gt_impuesto    TYPE STANDARD TABLE OF zts_total_imp,
          gt_pagos       TYPE STANDARD TABLE OF zts_pago,
          gt_motivos     TYPE STANDARD TABLE OF zts_nc_motivo,
          gt_head_add    TYPE STANDARD TABLE OF zts_head_add,
          gt_ec_001      TYPE STANDARD TABLE OF zdt_ec_001,
          gt_ec_002      TYPE STANDARD TABLE OF zdt_ec_002,
          gt_ec_003      TYPE STANDARD TABLE OF zdt_ec_003,
          gt_ec_004      TYPE STANDARD TABLE OF zdt_ec_004,
          gt_ec_005      TYPE STANDARD TABLE OF zdt_ec_005,
          gt_ec_006      TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_007      TYPE STANDARD TABLE OF zdt_ec_007,
          gt_ec_008      TYPE STANDARD TABLE OF zdt_ec_008.

    DATA: gs_debito                   TYPE zdt_sd_doc_ndd,
          gs_factura                  TYPE zdt_sd_doc_fac,
          gs_SalesDocument            TYPE I_SalesDocument,
          gs_SalesDocumentItem        TYPE I_SalesDocumentItem,
          gs_BillingDocument          TYPE I_BillingDocument,
          gs_BillingDocumentItem      TYPE I_BillingDocumentItem,
          gs_BillingItemPrcgElmnt     TYPE I_BillingDocumentItemPrcgElmnt,
          gs_CompanyCode              TYPE I_CompanyCode,
          gs_AddlInformation          TYPE I_AddlCompanyCodeInformation,
          gs_BusinessPartner          TYPE I_BusinessPartner,
          gs_Address                  TYPE i_address_2,
          gs_email                    TYPE I_AddressEmailAddress_2,
          gs_telefono                 TYPE I_AddressPhoneNumber_2,
          gs_Businesspartnertaxnumber TYPE I_Businesspartnertaxnumber,
          gs_BusPartAddress           TYPE I_BusPartAddress,
          gs_PaymentTerms             TYPE I_PaymentTermsConditions.

    DATA: gt_SalesDocument            TYPE STANDARD TABLE OF I_SalesDocument,
          gt_SalesDocumentItem        TYPE STANDARD TABLE OF I_SalesDocumentItem,
          gt_BillingDocument          TYPE STANDARD TABLE OF I_BillingDocument,
          gt_BillingDocumentItem      TYPE STANDARD TABLE OF I_BillingDocumentItem,
          gt_BillingItemPrcgElmnt     TYPE STANDARD TABLE OF I_BillingDocumentItemPrcgElmnt,
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

    METHODS getHeaderND        CHANGING  header_d  TYPE zts_nd_header
                                         motivos   TYPE zcl_create_nota_debito=>ty_motivos.

    METHODS getDatosBP         CHANGING header_d   TYPE zts_nd_header.

    METHODS getTotalImpuestos  CHANGING impuesto   TYPE zcl_create_nota_debito=>ty_impuesto.

    METHODS getViaPago         CHANGING pagos      TYPE zcl_create_nota_debito=>ty_pagos.

    METHODS getHeaderAdd       CHANGING header_add TYPE zcl_create_nota_debito=>ty_head_add.

ENDCLASS.



CLASS ZCL_CREATE_NOTA_DEBITO IMPLEMENTATION.


  METHOD calldocumenttype.

    gv_documenttype =  documenttype.

    me->get_data( ).

    me->infoTributaria( CHANGING inf_tribu = me->gs_inf_tribu ).

    me->getheadernd(  CHANGING header_d           = me->gs_header_d
                               motivos            = me->gt_motivos ).

    me->gettotalimpuestos( CHANGING impuesto      = me->gt_impuesto ).

    me->getViaPago(        CHANGING pagos         = me->gt_pagos    ).

    me->getHeaderAdd(      CHANGING header_add    = me->gt_head_add ).

    notadebito  = me->gs_header_d.
    inf_tribu   = me->gs_inf_tribu.
    t_impuesto  = me->gt_impuesto.
    t_head_add  = me->gt_head_add.
    t_motivos   = me->gt_motivos.
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
      WHEN '05'.
        IF me->gs_debito IS NOT INITIAL.
          estab       = me->gs_debito-establishment.
          ptoemi      = me->gs_debito-emissionpoint.
          secuencial  = me->gs_debito-sequential.
          claveacceso = me->gs_debito-accesskey.
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

*        IF sy-subrc EQ 0.
*          header_d- = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
*        ENDIF.

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

      header_d-razonsocialcomprador = me->gs_BusinessPartner-BusinessPartnerFullName.

      READ TABLE me->gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.

        IF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
          header_d-idcomprador = gs_Businesspartnertaxnumber-BPTaxNumber.
        ELSEIF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL .
          header_d-idcomprador = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ENDIF.

        READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = '02' companycode = me->gv_companycode.
        IF sy-subrc EQ 0.
          header_d-tipoidcomprador = gs_ec_004-typedi.
        ENDIF.

      ENDIF.

    ENDIF.

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
        CONCATENATE gs_head_add-valor '; ' gs_email-EmailAddress INTO gs_head_add-valor.
      ENDIF.
    ENDLOOP.
    IF gs_head_add IS NOT INITIAL.
      gs_head_add-nombre = 'Email'.
      APPEND gs_head_add TO header_add.
    ENDIF.

    CLEAR: gs_head_add.
    LOOP AT me->gt_telefono INTO gs_telefono.
      IF sy-tabix EQ 1.
        CONCATENATE  gs_head_add-valor gs_telefono-PhoneAreaCodeSubscriberNumber  INTO gs_head_add-valor.
      ELSE.
        CONCATENATE gs_head_add-valor '; ' gs_telefono-PhoneAreaCodeSubscriberNumber INTO gs_head_add-valor.
      ENDIF.
    ENDLOOP.
    IF gs_head_add IS NOT INITIAL.
      gs_head_add-nombre = 'Telefono'.
      APPEND gs_head_add TO header_add.
    ENDIF.

  ENDMETHOD.


  METHOD getheadernd.

    DATA: lv_fecha TYPE string,
          lv_billingdocument        TYPE vbeln,
          lv_api   TYPE zde_type_api.

    lv_api = 'DN'.

    lv_fecha = me->gs_billingdocument-BillingDocumentDate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_d-fechaemision SEPARATED BY '/'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'CON_ESPECI'.
    IF sy-subrc EQ 0.
      header_d-contribuyenteespecial = gs_ec_007-low.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'OBLIG_CONT'.
    IF sy-subrc EQ 0.
      header_d-obligadocontabilidad = gs_ec_007-low.
    ENDIF.

    me->getDatosBP( CHANGING header_d = header_d ).


    IF me->gs_billingdocument-TransactionCurrency EQ 'USD'.
      header_d-moneda = 'DOLAR'.
    ELSE.
      header_d-moneda = me->gs_billingdocument-TransactionCurrency.
    ENDIF.

    READ TABLE gt_SalesDocumentItem INTO gs_SalesDocumentItem INDEX 1.
    IF sy-subrc EQ 0 AND gs_SalesDocumentItem-ReferenceSDDocument IS NOT INITIAL.
      lv_billingdocument = gs_SalesDocumentItem-ReferenceSDDocument.

      SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, billingdocument, billingdocumenttype,
                  soldtoparty, businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_sd_doc_fac
      WHERE companycode             EQ @me->gv_companycode
        AND billingdocument         EQ @lv_billingdocument
       INTO @gs_factura.

      IF sy-subrc EQ 0.

        header_d-cod_docmod      = gs_factura-documenttype.
        CONCATENATE gs_factura-issuedate+6(2) gs_factura-issuedate+4(2) gs_factura-issuedate(4) INTO header_d-fech_emis_modif SEPARATED BY '/'.
        header_d-num_cod_modif = |{ gs_factura-establishment }-{ gs_factura-emissionpoint }-{ gs_factura-sequential }|.
      ELSEIF gs_SalesDocument-PurchaseOrderByCustomer IS NOT INITIAL.

        header_d-cod_docmod      = '01'.
        header_d-num_cod_modif = gs_SalesDocument-PurchaseOrderByCustomer.
        CONCATENATE gs_SalesDocument-CustomerPurchaseOrderDate+6(2) gs_SalesDocument-CustomerPurchaseOrderDate+4(2) gs_SalesDocument-CustomerPurchaseOrderDate(4) INTO header_d-fech_emis_modif SEPARATED BY '/'.

      ENDIF.

    ELSEIF gs_SalesDocument-PurchaseOrderByCustomer IS NOT INITIAL.

        header_d-cod_docmod      = '01'.
        header_d-num_cod_modif = gs_SalesDocument-PurchaseOrderByCustomer.
        CONCATENATE gs_SalesDocument-CustomerPurchaseOrderDate+6(2) gs_SalesDocument-CustomerPurchaseOrderDate+4(2) gs_SalesDocument-CustomerPurchaseOrderDate(4) INTO header_d-fech_emis_modif SEPARATED BY '/'.

    ENDIF.

    IF me->gs_ec_002 IS NOT INITIAL.
      header_d-direstablecimiento = me->gs_ec_002-address.
    ENDIF.

    SELECT SINGLE *
    FROM I_SDDocumentReasonText
    WHERE SDDocumentReason = @gs_SalesDocument-SDDocumentReason
      AND Language         = @sy-langu
    INTO @DATA(ls_SDDocumentReasonText).
    IF sy-subrc EQ 0.
      gs_motivos-razon = ls_SDDocumentReasonText-SDDocumentReasonText.
    ENDIF.
    header_d-totalsinimpuestos = me->gs_billingdocument-TotalNetAmount.
    gs_motivos-valor = header_d-valor = me->gs_billingdocument-TotalNetAmount + me->gs_billingdocument-TotalTaxAmount.

    APPEND gs_motivos TO motivos.

  ENDMETHOD.


  METHOD gettotalimpuestos.

    DATA: lv_tarifa    TYPE i,
          lv_navnw     TYPE navnw,
          lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition LIKE LINE  OF lr_condition.

    LOOP AT gt_ec_005 INTO gs_ec_005 WHERE typecondition EQ '2'  "Impuestos
                                        OR typecondition EQ '3' ."ICE

      ls_condition-low    = gs_ec_005-ccondition.
      ls_condition-sign   = 'I'.
      ls_condition-option = 'EQ'.
      APPEND ls_condition TO lr_condition.
      CLEAR: ls_condition.

    ENDLOOP.

    LOOP AT gt_billingitemprcgelmnt INTO gs_billingitemprcgelmnt WHERE ConditionType IN lr_condition
                                                                   AND ConditionInactiveReason EQ space.

      READ TABLE gt_ec_005 INTO gs_ec_005 WITH KEY ccondition = gs_billingitemprcgelmnt-ConditionType.

      READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_billingitemprcgelmnt-TaxCode taxsupportid = gs_ec_005-typecondition.
      IF sy-subrc EQ 0.

        READ TABLE impuesto ASSIGNING FIELD-SYMBOL(<fs_impuesto>) WITH KEY codigo =  gs_ec_003-taxsupportid codigoporcentaje =  gs_ec_003-taxsidrate.
        IF sy-subrc EQ 0.
          lv_navnw = gs_billingitemprcgelmnt-ConditionBaseValue.
          <fs_impuesto>-baseimponible += lv_navnw.
          lv_navnw = gs_billingitemprcgelmnt-ConditionAmount.
          <fs_impuesto>-valor         += lv_navnw.
        ELSE.
          gs_impuesto-codigo            = gs_ec_003-taxsupportid.
          gs_impuesto-codigoporcentaje  = gs_ec_003-taxsidrate.

          lv_navnw = gs_billingitemprcgelmnt-ConditionBaseValue.
          gs_impuesto-baseimponible     = lv_navnw.

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
      gs_pagos-total = ( me->gs_billingdocument-TotalNetAmount + me->gs_billingdocument-TotalTaxAmount ) / lv_cantidad.
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

    SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, billingdocument, billingdocumenttype,
                  soldtoparty, businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_sd_doc_ndd
      WHERE companycode             EQ @me->gv_companycode
        AND fiscalyear              EQ @me->gv_fiscalyear
        AND accountingdocument      EQ @me->gv_accountingdocument
        AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
        AND billingdocument         EQ @me->gv_billingdocument
        AND billingdocumenttype     EQ @me->gv_billingdocumenttype
       INTO @gs_debito.

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
      WHEN '05'.
        lv_api = 'DN'.
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
