CLASS zcl_create_retencion DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_pagos     TYPE STANDARD TABLE OF zts_pago,
           ty_reembolso TYPE STANDARD TABLE OF zts_fac_det_reembolso,
           ty_reem_imp  TYPE STANDARD TABLE OF zts_reem_imp,
           ty_retencion TYPE STANDARD TABLE OF zts_retenciones,
           ty_sustento  TYPE STANDARD TABLE OF zts_rete_sustento,
           ty_imp_sust  TYPE STANDARD TABLE OF zts_total_imp_sust,
           ty_head_add  TYPE STANDARD TABLE OF zts_head_add.""

    DATA: gv_companycode            TYPE bukrs,
          gv_documenttype           TYPE zde_trsri,
          gv_fiscalyear             TYPE gjahr,
          gv_accountingdocument     TYPE belnr_d,
          gv_accountingdocumenttype TYPE blart.

    METHODS constructor IMPORTING companycode            TYPE bukrs             "Sociedad
                                  fiscalyear             TYPE gjahr   OPTIONAL  "Ejercicio de Docuemnto
                                  accountingdocument     TYPE belnr_d OPTIONAL  "Documento de Financiero
                                  accountingdocumenttype TYPE blart   OPTIONAL.  "Tipo Documento de Financiero

    METHODS callDocumentType IMPORTING documenttype TYPE zde_trsri             "Tipo de Documento SRI
                             EXPORTING inf_tribu    TYPE zts_inf_tribu
                                       retencion    TYPE zts_rete_header
                                       t_pagos      TYPE zcl_create_retencion=>ty_pagos
                                       t_reembolso  TYPE zcl_create_retencion=>ty_reembolso
                                       t_reem_imp   TYPE zcl_create_retencion=>ty_reem_imp
                                       t_sustento   TYPE zcl_create_retencion=>ty_sustento
                                       t_impuesto_s TYPE zcl_create_retencion=>ty_imp_sust
                                       t_retencion  TYPE zcl_create_retencion=>ty_retencion
                                       t_head_add   TYPE zcl_create_retencion=>ty_head_add.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_header_l    TYPE zts_liqd_header,
          gs_header_r    TYPE zts_rete_header,
          gs_inf_tribu   TYPE zts_inf_tribu,
          gs_impuesto    TYPE zts_total_imp,
          gs_imp_sust    TYPE zts_total_imp_sust,
          gs_pagos       TYPE zts_pago,
          gs_retenciones TYPE zts_retenciones,
          gs_reembolso   TYPE zts_fac_det_reembolso,
          gs_sustento    TYPE zts_rete_sustento,
          gs_reem_imp    TYPE zts_reem_imp,
          gs_head_add    TYPE zts_head_add,
          gs_ec_001      TYPE zdt_ec_001,
          gs_ec_002      TYPE zdt_ec_002,
          gs_ec_003      TYPE zdt_ec_003,
          gs_ec_004      TYPE zdt_ec_004,
          gs_ec_005      TYPE zdt_ec_005,
          gs_ec_006      TYPE zdt_ec_006,
          gs_ec_007      TYPE zdt_ec_007,
          gs_ec_008      TYPE zdt_ec_008,
          gs_ec_009      TYPE zdt_ec_009,
          gs_ec_012      TYPE zdt_ec_012,
          gs_ec_013      TYPE zdt_ec_013,
          gs_ec_014      TYPE zdt_ec_014.

    DATA: gt_impuesto    TYPE STANDARD TABLE OF zts_total_imp,
          gt_imp_sust    TYPE STANDARD TABLE OF zts_total_imp_sust,
          gt_pagos       TYPE STANDARD TABLE OF zts_pago,
          gt_sustento    TYPE STANDARD TABLE OF zts_rete_sustento,
          gt_retenciones TYPE STANDARD TABLE OF zts_retenciones,
          gt_reembolso   TYPE STANDARD TABLE OF zts_fac_det_reembolso,
          gt_reem_imp    TYPE STANDARD TABLE OF zts_reem_imp,
          gt_head_add    TYPE STANDARD TABLE OF zts_head_add,
          gt_ec_001      TYPE STANDARD TABLE OF zdt_ec_001,
          gt_ec_002      TYPE STANDARD TABLE OF zdt_ec_002,
          gt_ec_003      TYPE STANDARD TABLE OF zdt_ec_003,
          gt_ec_004      TYPE STANDARD TABLE OF zdt_ec_004,
          gt_ec_005      TYPE STANDARD TABLE OF zdt_ec_005,
          gt_ec_006      TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_007      TYPE STANDARD TABLE OF zdt_ec_007,
          gt_ec_008      TYPE STANDARD TABLE OF zdt_ec_008,
          gt_ec_009      TYPE STANDARD TABLE OF zdt_ec_009,
          gt_ec_012      TYPE STANDARD TABLE OF zdt_ec_012,
          gt_ec_013      TYPE STANDARD TABLE OF zdt_ec_013,
          gt_ec_014      TYPE STANDARD TABLE OF zdt_ec_014.

    DATA: gs_retencion                TYPE zdt_fi_doc_ret,
          gs_liquida                  TYPE zdt_fi_doc_liq,
          gs_JournalEntry             TYPE I_JournalEntry,
          gs_JournalEntryItem         TYPE I_OperationalAcctgDocItem,
          gs_JournalEntryItemPartner  TYPE I_OperationalAcctgDocItem,
          gs_AcctgDocTaxItem          TYPE I_OperationalAcctgDocTaxItem,
          gs_PaymentMethod            TYPE I_PaymentMethod,
          gs_PricingCondition         TYPE I_PricingConditionType,
          gs_CompanyCode              TYPE I_CompanyCode,
          gs_AddlInformation          TYPE I_AddlCompanyCodeInformation,
          gs_Country                  TYPE I_Country,
          gs_TaxCode                  TYPE I_TaxCode,
          gs_TaxType                  TYPE I_TaxType,
          gs_Withholdingtaxitem       TYPE I_Withholdingtaxitem,
          gs_BusinessPartner          TYPE I_BusinessPartner,
          gs_SupplierCompany          TYPE I_SupplierCompany,"I_SupplierCompany-SupplierAccountNote Parte Relacionada
          gs_Supplier                 TYPE I_Supplier,
          gs_Address                  TYPE i_address_2,
          gs_email                    TYPE I_AddressEmailAddress_2,
          gs_telefono                 TYPE I_AddressPhoneNumber_2,
          gs_Businesspartnertaxnumber TYPE I_Businesspartnertaxnumber,
          gs_BusPartAddress           TYPE I_BusPartAddress,
          gs_PaymentTerms             TYPE I_PaymentTermsConditions.

    DATA: gt_JournalEntry             TYPE STANDARD TABLE OF I_JournalEntry,
          gt_JournalEntryItem         TYPE STANDARD TABLE OF I_OperationalAcctgDocItem,
          gt_JournalEntryItemPartner  TYPE STANDARD TABLE OF I_OperationalAcctgDocItem,
          gt_AcctgDocTaxItem          TYPE STANDARD TABLE OF I_OperationalAcctgDocTaxItem,
          gt_PaymentMethod            TYPE STANDARD TABLE OF I_PaymentMethod,
          gt_PricingCondition         TYPE STANDARD TABLE OF I_PricingConditionType,
          gt_CompanyCode              TYPE STANDARD TABLE OF I_CompanyCode,
          gt_AddlInformation          TYPE STANDARD TABLE OF I_AddlCompanyCodeInformation,
          gt_Country                  TYPE STANDARD TABLE OF I_Country,
          gt_TaxCode                  TYPE STANDARD TABLE OF I_TaxCode,
          gt_TaxType                  TYPE STANDARD TABLE OF I_TaxType,
          gt_Withholdingtaxitem       TYPE STANDARD TABLE OF I_Withholdingtaxitem,
          gt_BusinessPartner          TYPE STANDARD TABLE OF I_BusinessPartner,
          gt_ADDRESS                  TYPE STANDARD TABLE OF i_address_2,
          gt_email                    TYPE STANDARD TABLE OF I_AddressEmailAddress_2,
          gt_telefono                 TYPE STANDARD TABLE OF I_AddressPhoneNumber_2,
          gt_Businesspartnertaxnumber TYPE STANDARD TABLE OF I_Businesspartnertaxnumber,
          gt_PaymentTerms             TYPE STANDARD TABLE OF I_PaymentTermsConditions.

    METHODS get_data .
    METHODS infoTributaria      CHANGING inf_tribu   TYPE zts_inf_tribu.

    METHODS getClaveAcceso     IMPORTING inf_tribu   TYPE zts_inf_tribu
                                         fecha       TYPE string
                                         api         TYPE zde_type_api
                                CHANGING estab       TYPE zts_inf_tribu-estab
                                         ptoemi      TYPE zts_inf_tribu-ptoemi
                                         secuencial  TYPE zts_inf_tribu-secuencial
                                         claveacceso TYPE zts_inf_tribu-claveacceso.

    METHODS getHeaderRetencion  CHANGING header_r   TYPE zts_rete_header
                                         reembolso  TYPE zcl_create_retencion=>ty_reembolso
                                         reem_imp   TYPE zcl_create_retencion=>ty_reem_imp.

    METHODS getDatosBP          CHANGING header_r   TYPE zts_rete_header.

    METHODS getTotalImpuestos  IMPORTING sustento   TYPE xblnr
                                CHANGING impuesto   TYPE zcl_create_retencion=>ty_imp_sust.

    METHODS getSustento         CHANGING sustento   TYPE zcl_create_retencion=>ty_sustento
                                         impuesto_s TYPE zcl_create_retencion=>ty_imp_sust
                                         retencion  TYPE zcl_create_retencion=>ty_retencion.

    METHODS getReembolso        CHANGING header_r   TYPE zts_rete_header
                                         reembolso  TYPE zcl_create_retencion=>ty_reembolso
                                         reem_imp   TYPE zcl_create_retencion=>ty_reem_imp.

    METHODS getViaPago          CHANGING pagos      TYPE zcl_create_retencion=>ty_pagos.

    METHODS getHeaderAdd        CHANGING header_add TYPE zcl_create_retencion=>ty_head_add.

ENDCLASS.

CLASS ZCL_CREATE_RETENCION IMPLEMENTATION.


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

    me->gettotalimpuestos( EXPORTING sustento = gs_journalentry-DocumentReferenceID CHANGING impuesto        = me->gt_imp_sust ).

    me->getheaderretencion(  CHANGING header_r      = me->gs_header_r
                                      reembolso     = me->gt_reembolso
                                      reem_imp      = me->gt_reem_imp ).

    me->getViaPago(        CHANGING pagos           = me->gt_pagos    ).

    me->getHeaderAdd(      CHANGING header_add      = me->gt_head_add ).

    me->getSustento( CHANGING   sustento = gt_sustento
                              impuesto_s = gt_imp_sust
                               retencion = gt_retenciones ).

    inf_tribu    = me->gs_inf_tribu.
    retencion    = me->gs_header_r.
    t_sustento   = me->gt_sustento.
    t_impuesto_s = me->gt_imp_sust.
    t_reembolso  = me->gt_reembolso.
    t_reem_imp   = me->gt_reem_imp.
    t_retencion  = me->gt_retenciones.
    t_pagos      = me->gt_pagos.
    t_head_add   = me->gt_head_add.

  ENDMETHOD.

  METHOD get_data.

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
    FROM zdt_ec_001
    WHERE companycode    EQ @me->gv_companycode
      AND documenttype   EQ @me->gv_accountingdocumenttype
      AND ( documentsri  EQ @me->gv_documenttype
       OR   documentsri  EQ '03' )
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

    SELECT client, withholdingtaxtype, withholdingtaxcode, officialwhldgtaxcode, withholdingtype
    FROM zdt_ec_014
    INTO TABLE @gt_ec_014.

    SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, supplier,
                  businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_fi_doc_ret
      WHERE companycode             EQ @me->gv_companycode
        AND fiscalyear              EQ @me->gv_fiscalyear
        AND accountingdocument      EQ @me->gv_accountingdocument
        AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
       INTO @gs_retencion.

    SELECT SINGLE client, companycode, fiscalyear, accountingdocument, accountingdocumenttype, supplier,
                  businessname,typeid, idnumber, establishment, emissionpoint, sequential, accesskey,
                  documenttype, issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename
      FROM zdt_fi_doc_liq
      WHERE companycode             EQ @me->gv_companycode
        AND fiscalyear              EQ @me->gv_fiscalyear
        AND accountingdocument      EQ @me->gv_accountingdocument
        AND accountingdocumenttype  EQ @me->gv_accountingdocumenttype
       INTO @gs_liquida.

    IF sy-subrc EQ 0.

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

    ENDIF.

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
        FROM I_OperationalAcctgDocItem
        WHERE companycode          EQ @me->gv_companycode
          AND AccountingDocument   EQ @me->gv_accountingdocument
          AND FiscalYear           EQ @me->gv_fiscalyear
      INTO TABLE @gt_JournalEntryItem.

      SELECT  *
        FROM I_OperationalAcctgDocTaxItem
        WHERE companycode          EQ @me->gv_companycode
          AND AccountingDocument   EQ @me->gv_accountingdocument
          AND FiscalYear           EQ @me->gv_fiscalyear
      INTO TABLE @gt_AcctgDocTaxItem.

      SELECT *
        FROM I_Withholdingtaxitem
        WHERE companycode          EQ @me->gv_companycode
          AND AccountingDocument   EQ @me->gv_accountingdocument
          AND FiscalYear           EQ @me->gv_fiscalyear
        INTO TABLE @gt_Withholdingtaxitem.

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

  METHOD getheaderretencion.

     DATA: lv_fecha      TYPE string,
           lv_fiscalyear TYPE gjahr,
           lv_api        TYPE zde_type_api.

    lv_api = 'WH'.

    lv_fecha = me->gs_journalentry-PostingDate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_r-fechaemision SEPARATED BY '/'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'CON_ESPECI'.
    IF sy-subrc EQ 0.
      header_r-contribuyenteespecial = gs_ec_007-low.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'OBLIG_CONT'.
    IF sy-subrc EQ 0.
      header_r-obligadocontabilidad = gs_ec_007-low.
    ENDIF.

    me->getDatosBP( CHANGING header_r = header_r ).

    READ TABLE gt_ec_001 INTO gs_ec_001 WITH KEY  CompanyCode =  me->gs_journalentry-companycode documenttype = me->gs_journalentry-AccountingDocumentType
                                                  documentsri  = me->gv_documenttype.

    IF sy-subrc EQ 0 AND gs_ec_001-refunds IS NOT INITIAL.

*      header_r-coddocreembolso   = '41'.
      me->getReembolso( CHANGING header_r = header_r
                                reembolso = reembolso
                                reem_imp  = reem_imp  ).

    ENDIF.

    header_r-periodofiscal = |{ me->gs_journalentry-FiscalPeriod+1(2) }/{ me->gs_journalentry-FiscalYear }|.

    IF me->gs_ec_002 IS NOT INITIAL.
      header_r-direstablecimiento = me->gs_ec_002-address.
    ENDIF.

  ENDMETHOD.

  METHOD infoTributaria.

    DATA: lv_api          TYPE zde_type_api,
          lv_fecha        TYPE string,
          lv_documenttype TYPE zdt_ec_001-documenttype.

    lv_api = 'WH'.
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
        IF gs_ec_001-documentsri EQ '03'.
          gs_ec_001-documentsri = inf_tribu-coddoc = '07'.
        ELSE.
          inf_tribu-coddoc = gs_ec_001-documentsri.
        ENDIF.
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
      WHEN '07'.
        IF me->gs_retencion IS NOT INITIAL.
          estab       = me->gs_retencion-establishment.
          ptoemi      = me->gs_retencion-emissionpoint.
          secuencial  = me->gs_retencion-sequential.
          claveacceso = me->gs_retencion-accesskey.
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

    CLEAR: gs_head_add.
    IF me->gs_Address IS NOT INITIAL.
      gs_head_add-valor = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
      gs_head_add-nombre = 'Direccion'.
      APPEND gs_head_add TO header_add.
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
       FROM I_suppliercompany
        WHERE Supplier    EQ @me->gs_journalentryitempartner-Supplier
          AND CompanyCode EQ @me->gs_journalentryitempartner-CompanyCode
        INTO @gs_suppliercompany.

      SELECT SINGLE *
       FROM I_Supplier
        WHERE Supplier    EQ @me->gs_journalentryitempartner-Supplier
        INTO @gs_supplier.

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

*        IF sy-subrc EQ 0.
*          header_r-direccionproveedor = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
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

      IF gs_BusinessPartner-BusinessPartnerCategory EQ '1'.
        header_r-tiposujetoretenido = '01'.
      ELSE.
        header_r-tiposujetoretenido = '02'.
      ENDIF.

      header_r-razonsocialsujetoretenido = me->gs_BusinessPartner-BusinessPartnerFullName.

      READ TABLE me->gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.

        IF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
          header_r-identificacionsujetoretenido = gs_Businesspartnertaxnumber-BPTaxNumber.
        ELSEIF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL .
          header_r-identificacionsujetoretenido = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ENDIF.

        READ TABLE me->gt_ec_004 INTO gs_ec_004 WITH KEY bptaxtype =  gs_Businesspartnertaxnumber-BPTaxType typedoccument = '02' companycode = me->gv_companycode.
        IF sy-subrc EQ 0.
          header_r-tipoidentificacionsujetoreteni = gs_ec_004-typedi.
        ENDIF.

      ENDIF.

      IF gs_suppliercompany-SupplierAccountNote IS NOT INITIAL.
        header_r-parterel = 'SI'.
      ELSE.
        header_r-parterel = 'NO'.
      ENDIF.

    ENDIF.

    IF gs_supplier IS NOT INITIAL.

      READ TABLE gt_ec_009 INTO  gs_ec_009 WITH KEY country = gs_supplier-Country.
      IF sy-subrc EQ 0.

        IF gs_supplier-Country EQ 'EC'.
          gs_sustento-pagolocext           = '01'.
          gs_sustento-pagextsujretnorleg   = 'NO'.
        ELSE.
          gs_sustento-pagolocext           = '02'.
          gs_sustento-pagoregfis           = 'NO'.
          gs_sustento-pagextsujretnorleg   = 'SI'.

          IF gs_ec_009-taxhavencountry IS NOT INITIAL.
            gs_sustento-paisefecpago         = gs_ec_009-taxhavencountry.
            gs_sustento-tiporegi           = '02'.
            IF gs_ec_009-pais_conv IS NOT INITIAL.
              gs_sustento-aplicconvdobtrib = 'SI'.
            ELSE.
              gs_sustento-aplicconvdobtrib = 'NO'.
            ENDIF.
          ELSE.
            gs_sustento-paisefecpago         = gs_ec_009-countrysri.
            gs_sustento-tiporegi           = '01'.
            gs_sustento-aplicconvdobtrib   = 'NO'.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD getreembolso.

    DATA: lv_tarifa   TYPE i,
          lv_cantidad TYPE i,
          lv_navnw    TYPE navnw,
          lv_bases    TYPE navnw,
          lv_impuesto TYPE navnw.

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


        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_acctgdoctaxitem-TaxCode tax = abap_true.
        IF sy-subrc EQ 0.
          gs_reem_imp-codigo           = gs_ec_003-taxsupportid.
          gs_reem_imp-codigoporcentaje = gs_ec_003-taxsidrate.
        ENDIF.

        lv_tarifa = ( gs_reem_imp-impuestoreembolso / gs_reem_imp-baseimponiblereembolso ) * 100.
        gs_reem_imp-tarifa = lv_tarifa.

        lv_navnw = gs_ec_013-amountbasetax.
        gs_reem_imp-baseimponiblereembolso = lv_navnw.
        lv_bases      += lv_navnw.
        lv_navnw = gs_ec_013-amounttax.
        gs_reem_imp-impuestoreembolso      = lv_navnw.
        lv_impuesto   += lv_navnw.

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
        lv_bases += lv_navnw.
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
        lv_bases += lv_navnw.
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
        lv_bases += lv_navnw.
        APPEND gs_reem_imp TO gt_reem_imp.
        CLEAR: gs_reem_imp-tarifa, gs_reem_imp-baseimponiblereembolso, gs_reem_imp-codigoporcentaje, gs_reem_imp-codigo,
               gs_reem_imp-impuestoreembolso.
      ENDIF.


      APPEND gs_reembolso TO reembolso.
      CLEAR: gs_reem_imp, gs_reembolso.

      lv_cantidad += 1.

    ENDLOOP.

    gs_sustento-totalbaseimponiblereembolso = lv_bases.
    gs_sustento-totalcomprobantesreembolso  = lv_bases + lv_impuesto.
    gs_sustento-totalimpuestoreembolso      = lv_impuesto.

  ENDMETHOD.

  METHOD gettotalimpuestos.

    DATA: lv_tarifa    TYPE i,
          lv_navnw     TYPE navnw,
          lv_impuesto  TYPE navnw,
          lv_base      TYPE navnw,
          lr_condition TYPE RANGE OF zdt_ec_005-ccondition,
          ls_condition LIKE LINE  OF lr_condition.

    IF gt_journalentryitem[] IS NOT INITIAL.

      gt_JournalEntryItemPartner[]  = gt_JournalEntryItem[].

      DELETE gt_JournalEntryItemPartner WHERE Supplier IS INITIAL."Partner

      READ TABLE gt_JournalEntryItemPartner INTO gs_JournalEntryItemPartner INDEX 1.

    ENDIF.

    LOOP AT gt_acctgdoctaxitem INTO gs_acctgdoctaxitem.

      gs_imp_sust-numdocsustento = sustento.
      READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_acctgdoctaxitem-TaxCode.
      IF sy-subrc EQ 0.

        gs_imp_sust-codigo            = gs_ec_003-taxsupportid.
        gs_imp_sust-codigoporcentaje  = gs_ec_003-taxsidrate.

        CLEAR: lv_navnw.
        lv_navnw += abs( gs_acctgdoctaxitem-TaxBaseAmountInCoCodeCrcy ).
        gs_imp_sust-baseimponible     = lv_navnw.
        lv_base += lv_navnw.

        CLEAR: lv_navnw.
        lv_navnw = gs_acctgdoctaxitem-TaxAmountInCoCodeCrcy.
        gs_imp_sust-valor             = lv_navnw.
        lv_impuesto += lv_navnw.

        lv_tarifa                     = gs_ec_003-taxratepercent.
        gs_imp_sust-tarifa            = lv_tarifa.

        APPEND gs_imp_sust TO impuesto.
        CLEAR: gs_imp_sust.

      ENDIF.

    ENDLOOP.

    gs_sustento-importetotal      = lv_base + lv_impuesto.
    gs_sustento-totalsinimpuestos = lv_base.

  ENDMETHOD.

  METHOD getSustento.

    DATA: lv_tarifa    TYPE i,
          lv_sustentos TYPE i,
          lv_navnw     TYPE navnw,
          lv_baseimp   TYPE navnw,
          lv_impuesto  TYPE navnw,
          lv_fecha     TYPE string.

    DATA: ls_ec_003                   TYPE zdt_ec_003,
          lt_ec_003                   TYPE STANDARD TABLE OF zdt_ec_003.

    LOOP AT gt_AcctgDocTaxItem INTO gs_AcctgDocTaxItem WHERE CompanyCode        EQ gs_journalentry-CompanyCode
                                                         AND FiscalYear         EQ gs_journalentry-FiscalYear
                                                         AND AccountingDocument EQ gs_journalentry-AccountingDocument.

      READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_AcctgDocTaxItem-TaxCode CompanyCode = gs_AcctgDocTaxItem-CompanyCode.
      IF sy-subrc EQ 0.
        APPEND gs_ec_003 TO lt_ec_003.
      ENDIF.

    ENDLOOP.

    gt_ec_003[] = lt_ec_003[]. "Solo Codigos de Impuestos que se aplican en el documento

    SORT lt_ec_003 BY supporttaxcode.
    DELETE ADJACENT DUPLICATES FROM lt_ec_003 COMPARING supporttaxcode. "Solo Codigos de Sustentos Distintos

    lv_sustentos = LINES( lt_ec_003 ).

    LOOP AT lt_ec_003 INTO ls_ec_003 WHERE CompanyCode        EQ gs_journalentry-CompanyCode.

      CLEAR: lv_baseimp, lv_impuesto.

      LOOP AT lt_ec_003 INTO gs_ec_003 WHERE CompanyCode        EQ ls_ec_003-CompanyCode
                                         AND supporttaxcode     EQ ls_ec_003-supporttaxcode.

          LOOP AT gt_AcctgDocTaxItem INTO gs_AcctgDocTaxItem WHERE CompanyCode        EQ gs_journalentry-CompanyCode
                                                               AND FiscalYear         EQ gs_journalentry-FiscalYear
                                                               AND AccountingDocument EQ gs_journalentry-AccountingDocument
                                                               AND TaxCode            EQ gs_ec_003-taxcode.

            lv_baseimp  += gs_AcctgDocTaxItem-TaxBaseAmountInCoCodeCrcy.
            lv_impuesto += gs_AcctgDocTaxItem-TaxAmountInCoCodeCrcy.

          ENDLOOP.

      ENDLOOP.

      gs_sustento-codsustento       = gs_ec_003-supporttaxcode.

      IF gs_liquida IS NOT INITIAL.

        gs_sustento-coddocsustento    = gs_liquida-documenttype.
        gs_sustento-numautdocsustento = |{ gs_liquida-establishment }{ gs_liquida-emissionpoint }{ gs_liquida-sequential }|.
        gs_sustento-numdocsustento    = gs_liquida-accesskey.

        lv_fecha = me->gs_liquida-issuedate.

      ELSE.

        gs_sustento-coddocsustento    = '01'.
        gs_sustento-numautdocsustento = gs_journalentryitempartner-DocumentItemText.
        gs_sustento-numdocsustento    = gs_journalentry-DocumentReferenceID.

        lv_fecha = me->gs_journalentry-DocumentDate.

      ENDIF.

      CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_sustento-fechaemisiondocsustento SEPARATED BY '/'.

      CLEAR: lv_fecha.
      lv_fecha = me->gs_journalentry-PostingDate.
      CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_sustento-fecharegistrocontable SEPARATED BY '/'.

      APPEND gs_sustento TO sustento.

      LOOP AT gt_withholdingtaxitem INTO gs_withholdingtaxitem WHERE CompanyCode        EQ gs_journalentry-CompanyCode
                                                                 AND FiscalYear         EQ gs_journalentry-FiscalYear
                                                                 AND AccountingDocument EQ gs_journalentry-AccountingDocument.

        READ TABLE gt_ec_014 INTO gs_ec_014 WITH KEY withholdingtaxcode = gs_withholdingtaxitem-withholdingtaxcode
                                                     withholdingtaxtype = gs_withholdingtaxitem-withholdingtaxtype.
        IF sy-subrc EQ 0.
          gs_retenciones-codigo           = gs_ec_014-withholdingtype. "1- Withholding at Source  OR 2- Withholding at Tax OR 6- Withholding at ISD
          gs_retenciones-codigoretencion  = gs_ec_014-officialwhldgtaxcode.
        ENDIF.

        gs_retenciones-numdocsustento = gs_sustento-numdocsustento.

        CLEAR: lv_navnw.
        IF gs_retenciones-codigo EQ '1'. "Withholding at Source
          lv_navnw = lv_baseimp.
        ELSEIF gs_retenciones-codigo EQ '2'. "Withholding at Tax
          lv_navnw = lv_impuesto.
        ELSE."Withholding at ISD
          lv_navnw = gs_withholdingtaxitem-WhldgTaxBaseAmtInCoCodeCrcy.
        ENDIF.
        gs_retenciones-baseimponible = lv_navnw.

        CLEAR: lv_navnw.
        lv_navnw = ( gs_retenciones-baseimponible * gs_withholdingtaxitem-WithholdingTaxPercent ) / 100.
        gs_retenciones-valorretenido = lv_navnw.

        CLEAR: lv_navnw.
        lv_navnw = gs_withholdingtaxitem-WithholdingTaxPercent.
        gs_retenciones-porcentajeretener = lv_navnw.

      "Dividendos
*      IF gs_journalentryitempartner-Reference1IDByBusinessPartner IS NOT INITIAL or gs_journalentry-JrnlEntryCntrySpecificRef1 .
*        IF gs_journalentry-JrnlEntryCntrySpecificRef1.
*          gs_retenciones-ejerfisutdiv.
*          gs_retenciones-fechapagodiv.
*          gs_retenciones-imrentasoc.
*        ENDIF.
*      ENDIF.

       "Banana
*      IF gs_journalentryitempartner-Reference1IDByBusinessPartner IS NOT INITIAL or gs_journalentry-JrnlEntryCntrySpecificRef1 .
*        IF gs_journalentry-JrnlEntryCntrySpecificRef1.
*          gs_retenciones-numcajban.
*          gs_retenciones-preccajban.
*        ENDIF.
*      ENDIF.

        APPEND gs_retenciones TO retencion.
        CLEAR: gs_retenciones.

      ENDLOOP.

      CLEAR: gs_sustento.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
