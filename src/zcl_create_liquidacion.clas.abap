CLASS zcl_create_liquidacion DEFINITION

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
          gv_deliverydocument       TYPE vbeln,
          gv_usuname                TYPE c LENGTH 12,
          gv_deliverydocumenttype   TYPE fkart.

    METHODS constructor IMPORTING companycode            TYPE bukrs             "Sociedad
                                  fiscalyear             TYPE gjahr   OPTIONAL  "Ejercicio de Docuemnto
                                  accountingdocument     TYPE belnr_d OPTIONAL  "Documento de Financiero
                                  accountingdocumenttype TYPE blart   OPTIONAL. "Tipo Documento de Financiero

    METHODS callDocumentType IMPORTING documenttype TYPE zde_trsri                        "Tipo de Documento SRI
                             EXPORTING inf_tribu    TYPE zts_inf_tribu                    "si cabecera
                                       liquida      TYPE zts_liqd_header                  "si cabecera
                                       t_impuesto   TYPE zcl_create_liquidacion=>ty_impuesto  "si cabecera total impuesto
                                       t_pagos      TYPE zcl_create_liquidacion=>ty_pagos     "si cabecera pagos
                                       t_detalle    TYPE zcl_create_liquidacion=>ty_detalle_f "si detalles
                                       t_det_add    TYPE zcl_create_liquidacion=>ty_det_add   "si detalles
                                       t_det_imp    TYPE zcl_create_liquidacion=>ty_det_imp   "si detalles
                                       t_reembolso  TYPE zcl_create_liquidacion=>ty_reembolso "si detalles
                                       t_reem_imp   TYPE zcl_create_liquidacion=>ty_reem_imp  "si detalles de impuesto
                                       t_head_add   TYPE zcl_create_liquidacion=>ty_head_add. "si cebecera datos adicionales

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_header_l  TYPE zts_liqd_header,
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
          gs_ec_006    TYPE zdt_ec_006,
          gs_ec_007    TYPE zdt_ec_007,
          gs_ec_008    TYPE zdt_ec_008,
          gs_ec_009    TYPE zdt_ec_009,
          gs_ec_012    TYPE zdt_ec_012,
          gs_ec_013    TYPE zdt_ec_013.

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
          gt_ec_006    TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_007    TYPE STANDARD TABLE OF zdt_ec_007,
          gt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          gt_ec_009    TYPE STANDARD TABLE OF zdt_ec_009,
          gt_ec_012    TYPE STANDARD TABLE OF zdt_ec_012,
          gt_ec_013    TYPE STANDARD TABLE OF zdt_ec_013.

    DATA: gs_liqudacion               TYPE zdt_fi_doc_liq,
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

    DATA: gt_JournalEntry             TYPE STANDARD TABLE OF I_JournalEntry,
          gt_JournalEntryItem         TYPE STANDARD TABLE OF I_JournalEntryItem,
          gt_JournalEntryItembase     TYPE STANDARD TABLE OF I_JournalEntryItem,
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

    METHODS getHeaderFactura   CHANGING header_l  TYPE zts_liqd_header
                                        reembolso TYPE zcl_create_liquidacion=>ty_reembolso
                                        reem_imp  TYPE zcl_create_liquidacion=>ty_reem_imp.
    METHODS getDatosBP         CHANGING header_l   TYPE zts_liqd_header.
    METHODS getTotalImpuestos  CHANGING impuesto   TYPE zcl_create_liquidacion=>ty_impuesto.
    METHODS getImpotes         CHANGING header_l   TYPE zts_liqd_header.
    METHODS getDetalles        CHANGING detalle   TYPE zcl_create_liquidacion=>ty_detalle_f
                                        detalle_i TYPE zcl_create_liquidacion=>ty_det_imp
                                        detalle_a TYPE zcl_create_liquidacion=>ty_det_add.
    METHODS getReembolso       CHANGING header_l  TYPE zts_liqd_header
                                        reembolso TYPE zcl_create_liquidacion=>ty_reembolso
                                        reem_imp  TYPE zcl_create_liquidacion=>ty_reem_imp.
    METHODS getViaPago         CHANGING pagos      TYPE zcl_create_liquidacion=>ty_pagos.
    METHODS getHeaderAdd       CHANGING header_add TYPE zcl_create_liquidacion=>ty_head_add.

ENDCLASS.



CLASS zcl_create_liquidacion IMPLEMENTATION.

  METHOD constructor.

    gv_companycode            = companycode.
    gv_fiscalyear             = fiscalyear.
    gv_accountingdocument     = accountingdocument.
    gv_accountingdocumenttype = accountingdocumenttype.

  ENDMETHOD.

  METHOD calldocumenttype.

    gv_documenttype =  documenttype.

    me->get_data( ).

    me->infoTributaria( CHANGING inf_tribu = me->gs_inf_tribu ).

    me->gettotalimpuestos( CHANGING impuesto      = me->gt_impuesto ).

    me->getHeaderFactura(  CHANGING header_l      = me->gs_header_l
                                    reembolso     = me->gt_reembolso
                                    reem_imp      = me->gt_reem_imp ).

    me->getdetalles(       CHANGING detalle       = me->gt_detalle_f
                                    detalle_i     = me->gt_det_imp
                                    detalle_a     = me->gt_det_add ).
    me->getViaPago(        CHANGING pagos         = me->gt_pagos    ).
    me->getHeaderAdd(      CHANGING header_add    = me->gt_head_add ).

    liquida     = me->gs_header_l.
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

  METHOD get_data.

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
    FROM zdt_ec_001
    WHERE companycode  EQ @me->gv_companycode
      AND documenttype EQ @me->gv_accountingdocumenttype
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

    SELECT client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, documentitem,
           typeid, idnumber, documenttype, establishment, emissionpoint, sequential, accesskey, issuedate,
           taxcode, amountbasetax, amountbasetax0, amountbasenotax, amountbaseexetax, amounttax, amountice,
           total_price, currency
    FROM zdt_ec_013
    WHERE companycode             EQ @me->gv_companycode
      AND fiscalyear              EQ @me->gv_fiscalyear
      AND accountingdocument      EQ @me->gv_accountingdocument
      AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
    INTO TABLE @gt_ec_013.

    SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, supplier,
                  businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_fi_doc_liq
      WHERE companycode             EQ @me->gv_companycode
        AND fiscalyear              EQ @me->gv_fiscalyear
        AND accountingdocument      EQ @me->gv_accountingdocument
        AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
       INTO @gs_liqudacion.

    SELECT SINGLE *
      FROM I_JournalEntry
      WHERE companycode            EQ @me->gv_companycode
        AND AccountingDocument     EQ @me->gv_accountingdocument
        AND FiscalYear             EQ @me->gv_fiscalyear
        AND AccountingDocumentType EQ @me->gv_accountingdocumenttype
        AND IsReversal             EQ @space
        AND IsReversed             EQ @space
       INTO @gs_journalentry.

    IF sy-subrc EQ 0.
      SELECT  *
        FROM I_JournalEntryItem
        WHERE companycode          EQ @me->gv_companycode
          AND AccountingDocument   EQ @me->gv_accountingdocument
          AND FiscalYear           EQ @me->gv_fiscalyear
          AND Ledger               EQ '0L'
      INTO TABLE @gt_JournalEntryItem.
    ENDIF.

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

    lv_api = 'LP'.
    lv_documenttype = me->gv_accountingdocumenttype.

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

      lv_fecha = me->gs_journalentry-PostingDate.

      READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode            = me->gs_journalentry-CompanyCode
                                                   accountingdocumenttype = me->gs_journalentry-AccountingDocumentType
                                                   documentsri            = me->gs_ec_001-documentsri
                                                   users                  = sy-uname.

      IF sy-subrc EQ 0.

        READ TABLE me->gt_ec_002 INTO gs_ec_002 WITH KEY companycode    = me->gv_CompanyCode
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
      WHEN '03'.
        IF me->gs_liqudacion IS NOT INITIAL.
          estab       = me->gs_liqudacion-establishment.
          ptoemi      = me->gs_liqudacion-emissionpoint.
          secuencial  = me->gs_liqudacion-sequential.
          claveacceso = me->gs_liqudacion-accesskey.
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

  METHOD getheaderfactura.

    DATA: lv_fecha      TYPE string,
          lv_fiscalyear TYPE gjahr,
          lv_api        TYPE zde_type_api.

    lv_api = 'LP'.

    lv_fecha = me->gs_journalentry-PostingDate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_l-fechaemision SEPARATED BY '/'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'CON_ESPECI'.
    IF sy-subrc EQ 0.
      header_l-contribuyenteespecial = gs_ec_007-low.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'OBLIG_CONT'.
    IF sy-subrc EQ 0.
      header_L-obligadocontabilidad = gs_ec_007-low.
    ENDIF.

    me->getDatosBP( CHANGING header_l = header_l ).
    me->getImpotes( CHANGING header_l = header_l ).

    READ TABLE gt_ec_001 INTO gs_ec_001 WITH KEY  CompanyCode =  me->gs_journalentry-companycode documenttype = me->gs_journalentry-AccountingDocumentType
                                                  documentsri  = me->gv_documenttype.

    IF sy-subrc EQ 0 AND gs_ec_001-refunds IS NOT INITIAL.

      header_l-coddocreembolso   = '41'.
      me->getReembolso( CHANGING header_l = header_l
                                reembolso = reembolso
                                reem_imp  = reem_imp  ).

    ENDIF.

    IF me->gs_journalentry-TransactionCurrency EQ 'USD'.
      header_l-moneda = 'DOLAR'.
    ELSE.
      header_l-moneda = me->gs_journalentry-TransactionCurrency.
    ENDIF.

    IF me->gs_ec_002 IS NOT INITIAL.
      header_l-direstablecimiento = me->gs_ec_002-address.
    ENDIF.

  ENDMETHOD.

  METHOD getdatosbp.

    SELECT SINGLE *
    FROM I_BusinessPartner
     WHERE BusinessPartner EQ @me->gs_journalentryitempartner-Supplier
     INTO @gs_BusinessPartner.

    IF sy-subrc EQ 0.

      SELECT *
       FROM I_Businesspartnertaxnumber
        WHERE BusinessPartner EQ @me->gs_journalentryitempartner-Supplier
        INTO TABLE @gt_Businesspartnertaxnumber.

      SELECT SINGLE *
       FROM I_BusPartAddress
        WHERE BusinessPartner EQ @me->gs_journalentryitempartner-Supplier
        INTO @me->gs_BusPartAddress.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM i_address_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
           INTO @me->gs_Address.

        IF sy-subrc EQ 0.
          header_l-direccionproveedor = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
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

      header_l-razonsocialproveedor = me->gs_BusinessPartner-BusinessPartnerFullName.

      READ TABLE me->gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.

        IF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
          header_l-identificacionproveedor = gs_Businesspartnertaxnumber-BPTaxNumber.
        ELSEIF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL .
          header_l-identificacionproveedor = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ENDIF.

        READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = '02' companycode = me->gv_companycode.
        IF sy-subrc EQ 0.
          header_l-tipoidentificacionproveedor = gs_ec_004-typedi.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD getheaderadd.

    CLEAR: gs_head_add.
    gs_head_add-valor = me->gs_journalentry-AccountingDocument.
    gs_head_add-nombre = 'Documento SAP'.
    APPEND gs_head_add TO header_add.

*    CLEAR: gs_head_add.
*    IF me->gs_salesdocument-yy1_observ_sdh IS NOT INITIAL.
*        gs_head_add-nombre = 'Observacion'.
*        gs_head_add-valor = me->gs_salesdocument-yy1_observ_sdh.
*      APPEND gs_head_add TO header_add. "Z001 A_BillingDocumentText
*    ENDIF.

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

  METHOD gettotalimpuestos.

    DATA: lv_tarifa    TYPE i,
          lv_navnw     TYPE navnw,
          lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition LIKE LINE  OF lr_condition.

    IF gt_journalentryitem[] IS NOT INITIAL.

      gt_JournalEntryItemPartner[] = gt_JournalEntryItembase[] = gt_JournalEntryItemTax[] = gt_JournalEntryItem[].

      DELETE gt_JournalEntryItemTax WHERE TransactionTypeDetermination NE 'VST' "Impuestos
                                       OR TaxCode IS INITIAL.

      DELETE gt_JournalEntryItembase    WHERE TransactionTypeDetermination NE 'KBS' "Bases Imponibles
                                          AND TransactionTypeDetermination NE 'WRX'.

      DELETE gt_JournalEntryItembase    WHERE Supplier IS NOT INITIAL."Bases Imponibles

      DELETE gt_JournalEntryItemPartner WHERE Supplier IS INITIAL."Partner

      READ TABLE gt_JournalEntryItemPartner INTO gs_JournalEntryItemPartner INDEX 1.

    ENDIF.

    LOOP AT gt_JournalEntryItemTax INTO gs_JournalEntryItemTax.

      READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_JournalEntryItemTax-TaxCode.
      IF sy-subrc EQ 0.

        gs_impuesto-codigo            = gs_ec_003-taxsupportid.
        gs_impuesto-codigoporcentaje  = gs_ec_003-taxsidrate.

        CLEAR: lv_navnw.
        LOOP AT gt_JournalEntryItembase INTO gs_JournalEntryItem WHERE taxcode = gs_JournalEntryItemTax-TaxCode.

          lv_navnw += abs( gs_JournalEntryItem-AmountInCompanyCodeCurrency ).

        ENDLOOP.

        gs_impuesto-baseimponible     = lv_navnw.

        lv_navnw = gs_JournalEntryItemTax-AmountInCompanyCodeCurrency.
        gs_impuesto-valor             = lv_navnw.

        IF lv_navnw IS NOT INITIAL.
          lv_tarifa                     = gs_ec_003-taxratepercent.
          gs_impuesto-tarifa            = lv_tarifa.
        ENDIF.

        APPEND gs_impuesto TO impuesto.
        CLEAR: gs_impuesto.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD getimpotes.

    DATA: lv_navnw     TYPE navnw.

    IF header_l-totaldescuento IS INITIAL.
      header_l-totaldescuento = '0.00'.
    ENDIF.

    LOOP AT gt_journalentryitembase INTO gs_journalentryitem.
     lv_navnw  += abs( gs_journalentryitem-AmountInCompanyCodeCurrency ).
    ENDLOOP.

    header_l-totalsinimpuestos = lv_navnw.

    LOOP AT gt_journalentryitemtax INTO gs_journalentryitemtax.
      lv_navnw += abs( gs_journalentryitemtax-AmountInCompanyCodeCurrency ).
    ENDLOOP.

    header_l-importetotal = lv_navnw.

  ENDMETHOD.

  METHOD getviapago.

    DATA: ls_SupplierPurchasingOrg TYPE I_SupplierPurchasingOrg.

    DATA: lv_cantidad TYPE i,
          lv_texto    TYPE c LENGTH 10.

    SELECT SINGLE *
      FROM I_SupplierPurchasingOrg
      WHERE Supplier EQ @me->gs_journalentryitempartner-Supplier
      INTO @ls_SupplierPurchasingOrg.

    SELECT *
      FROM I_PaymentTermsConditions
      WHERE PaymentTerms EQ @ls_SupplierPurchasingOrg-PaymentTerms
      INTO TABLE @gt_PaymentTerms.

    READ TABLE gt_ec_006 INTO gs_ec_006 WITH KEY paymentmethod = ls_SupplierPurchasingOrg-PaymentTerms.
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
      gs_pagos-total = me->gs_header_l-importetotal / lv_cantidad.
      APPEND gs_pagos TO pagos.
    ENDLOOP.

    SORT pagos BY formapago unidadtiempo plazo total.
    DELETE ADJACENT DUPLICATES FROM pagos COMPARING ALL FIELDS.

  ENDMETHOD.

  METHOD getdetalles.

    DATA: lt_JournalEntryItem TYPE STANDARD TABLE OF I_JournalEntryItem,
          ls_JournalEntryItem TYPE I_JournalEntryItem.

    DATA: lv_tarifa    TYPE i,
          lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition LIKE LINE  OF lr_condition,
          lr_conditio2 TYPE RANGE OF zdt_ec_005-ccondition,
          lv_navnw     TYPE navnw,
          lv_precio    TYPE navnw.


    lt_JournalEntryItem[] = gt_journalentryitem[].
    SORT lt_JournalEntryItem BY Product.
    DELETE ADJACENT DUPLICATES FROM lt_JournalEntryItem COMPARING Product.

    LOOP AT lt_journalentryitem INTO ls_journalentryitem WHERE Product IS NOT INITIAL.

      gs_detalle_f-codigoprincipal   = ls_journalentryitem-Product.

*      IF ls_journalentryitem-DocumentItemText IS INITIAL.
        SELECT SINGLE *
        FROM I_ProductDescription
        WHERE Language EQ @sy-langu
         AND  Product  EQ @ls_journalentryitem-Product
         INTO @DATA(ls_Material).
        IF sy-subcs EQ 0.
          gs_detalle_f-descripcion = ls_Material-ProductDescription.
        ENDIF.
*      ELSE.
*        gs_detalle_f-descripcion       = ls_journalentryitem-DocumentItemText.
*      ENDIF.

      gs_detalle_f-cantidad          = ls_journalentryitem-Quantity.
      gs_detalle_f-unidadmedida      = ls_journalentryitem-BaseUnit.

      SELECT SINGLE *
      FROM I_UnitOfMeasureText
      WHERE Language EQ @sy-langu
        AND UnitOfMeasure EQ @ls_journalentryitem-BaseUnit
       INTO @DATA(ls_UnitOfMeasureText).
      IF sy-subrc EQ 0.
        gs_det_add-titulo = 'UnidadMedida'.
        gs_det_add-valor  = ls_UnitOfMeasureText-UnitOfMeasureLongName.
        gs_det_add-codigoprincipal  = ls_journalentryitem-Product.
        APPEND gs_det_add TO detalle_a.
      ENDIF.

      LOOP AT gt_JournalEntryItemTax INTO gs_JournalEntryItemTax WHERE taxcode = ls_JournalEntryItem-TaxCode.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_JournalEntryItemTax-TaxCode.
        IF sy-subrc EQ 0.

          gs_det_imp-codigoprincipal   = ls_JournalEntryItem-Product.
          gs_det_imp-codigo            = gs_ec_003-taxsupportid.
          gs_det_imp-codigoporcentaje  = gs_ec_003-taxsidrate.

          CLEAR: lv_navnw.
          LOOP AT gt_JournalEntryItem INTO gs_JournalEntryItem WHERE Product = ls_JournalEntryItem-Product
                                                                 AND taxcode  = gs_JournalEntryItemTax-TaxCode.

            lv_navnw += abs( gs_JournalEntryItem-AmountInCompanyCodeCurrency ).

          ENDLOOP.
          gs_det_imp-baseimponible      = lv_navnw.
          gs_detalle_f-totalsinimpuesto = lv_navnw.

          lv_navnw = gs_JournalEntryItemTax-AmountInCompanyCodeCurrency.
          gs_det_imp-valor             = lv_navnw.

          IF lv_navnw IS NOT INITIAL.
            lv_tarifa                     = gs_ec_003-taxratepercent.
            gs_det_imp-tarifa            = lv_tarifa.
          ENDIF.

          APPEND gs_det_imp TO detalle_i.
          CLEAR: gs_det_imp.

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

  METHOD getreembolso.

    DATA: lv_tarifa TYPE i,
          lv_navnw  TYPE navnw.

    LOOP AT gt_ec_013 INTO gs_ec_013.

      gs_reembolso-coddocreembolso        = '01'.
      IF gs_BusinessPartner-BusinessPartnerCategory EQ '1'.
        gs_reembolso-tipoproveedorreembolso = '01'.
      ELSE.
        gs_reembolso-tipoproveedorreembolso = '02'.
      ENDIF.

      READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = 'EC'.
      IF sy-subrc EQ 0.
        gs_reembolso-codpaispagoproveedorreembolso  = gs_ec_009-countrysri.
      ENDIF.

      gs_reembolso-identificacionproveedorreembol = gs_ec_013-idnumber.
      gs_reembolso-tipoidentificacionproveedorree = gs_ec_013-typeid.

      gs_reembolso-estabdocreembolso      = gs_EC_013-establishment.
      gs_reembolso-ptoemidocreembolso     = gs_EC_013-emissionpoint.
      gs_reembolso-secuencialdocreembolso = gs_EC_013-sequential.

      gs_reem_imp-secuencialdocreembolso  = |{ gs_reembolso-estabdocreembolso }{ gs_reembolso-ptoemidocreembolso }{ gs_reembolso-secuencialdocreembolso }|.

      CLEAR: lv_navnw.
      IF gs_reem_imp-impuestoreembolso IS NOT INITIAL AND gs_reem_imp-impuestoreembolso GT 0.


        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_JournalEntryItemTax-TaxCode tax = abap_true.
        IF sy-subrc EQ 0.
          gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
          gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
        ENDIF.

        lv_tarifa = ( gs_reem_imp-impuestoreembolso / gs_reem_imp-baseimponiblereembolso ) * 100.
        gs_reem_imp-tarifa = lv_tarifa.

        lv_navnw = gs_ec_013-amountbasetax.
        gs_reem_imp-baseimponiblereembolso = lv_navnw.
        lv_navnw = gs_ec_013-amounttax.
        gs_reem_imp-impuestoreembolso      = lv_navnw.
        header_l-totalimpuestoreembolso      += lv_navnw.

      ENDIF.

      CLEAR: lv_navnw.
      IF gs_ec_013-amountbasetax0 IS NOT INITIAL AND gs_ec_013-amountbasetax0 GT 0.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY tax0 = abap_true.
        IF sy-subrc EQ 0.
          gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
          gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
        ENDIF.

        gs_reem_imp-tarifa = '0'.
        gs_reem_imp-impuestoreembolso = '0.00'.
        lv_navnw = gs_ec_013-amountbasetax0.
        gs_reem_imp-baseimponiblereembolso = lv_navnw.
        header_l-totalbaseimponiblereembolso += lv_navnw.
        APPEND gs_reem_imp TO gt_reem_imp.
        CLEAR: gs_reem_imp-tarifa, gs_reem_imp-baseimponiblereembolso, gs_reem_imp-codigoporcentaje, gs_reem_imp-codigo,
               gs_reem_imp-impuestoreembolso.
      ENDIF.

      CLEAR: lv_navnw.
      IF gs_ec_013-amountbaseexetax IS NOT INITIAL.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY exempttax = abap_true.
        IF sy-subrc EQ 0.
          gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
          gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
        ENDIF.

        gs_reem_imp-tarifa = '0'.
        gs_reem_imp-impuestoreembolso = '0.00'.
        lv_navnw = gs_ec_013-amountbaseexetax.
        gs_reem_imp-baseimponiblereembolso = lv_navnw.
        header_l-totalbaseimponiblereembolso += lv_navnw.
        APPEND gs_reem_imp TO gt_reem_imp.
        CLEAR: gs_reem_imp-tarifa, gs_reem_imp-baseimponiblereembolso, gs_reem_imp-codigoporcentaje, gs_reem_imp-codigo,
               gs_reem_imp-impuestoreembolso.
      ENDIF.

      CLEAR: lv_navnw.
      IF gs_ec_013-amountbasenotax IS NOT INITIAL.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY notax = abap_true.
        IF sy-subrc EQ 0.
          gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
          gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
        ENDIF.

        gs_reem_imp-tarifa = '0'.
        gs_reem_imp-impuestoreembolso = '0.00'.
        lv_navnw = gs_ec_013-amountbasenotax.
        gs_reem_imp-baseimponiblereembolso = lv_navnw.
        header_l-totalbaseimponiblereembolso += lv_navnw.
        APPEND gs_reem_imp TO gt_reem_imp.
        CLEAR: gs_reem_imp-tarifa, gs_reem_imp-baseimponiblereembolso, gs_reem_imp-codigoporcentaje, gs_reem_imp-codigo,
               gs_reem_imp-impuestoreembolso.
      ENDIF.

      APPEND gs_reembolso TO reembolso.
      CLEAR: gs_reem_imp, gs_reembolso.

    ENDLOOP.

    header_l-totalcomprobantesreembolso = header_l-totalimpuestoreembolso + header_l-totalbaseimponiblereembolso.

  ENDMETHOD.

ENDCLASS.
