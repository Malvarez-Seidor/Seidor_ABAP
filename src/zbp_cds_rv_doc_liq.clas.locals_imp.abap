CLASS lhc_LiquidationPurchase DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR LiquidationPurchase RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION LiquidationPurchase~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION LiquidationPurchase~UpdateStatus RESULT result.

ENDCLASS.

CLASS lhc_LiquidationPurchase IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_liq  IN LOCAL MODE
      ENTITY LiquidationPurchase
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Documentstatus ReverseDocument )
      WITH CORRESPONDING #( keys )
      RESULT DATA(LiqPurchases)
      FAILED failed.

    result = VALUE #( FOR LiqPurchase IN LiqPurchases
                    ( %tky = LiqPurchase-%tky
                      %features-%action-SendDocument
          = COND #( WHEN LiqPurchase-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN LiqPurchase-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN LiqPurchase-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE LiqPurchases INTO DATA(ls_LiqPurchase) WITH KEY companycode = <fs_result>-Companycode
                                                               fiscalyear = <fs_result>-Fiscalyear
                                                       accountingdocument = <fs_result>-Accountingdocument
                                                   accountingdocumenttype = <fs_result>-Accountingdocumenttype.

      IF ls_LiqPurchase-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_LiqPurchase-ReverseDocument IS NOT INITIAL.
        <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD SendDocument.

    DATA: lv_flag    TYPE c,
          lv_xml     TYPE string,
          lv_raw     TYPE xstring,
          lv_base64  TYPE string,
          lv_mensaje TYPE string,
          lv_id      TYPE sgtxt,
          lv_user    TYPE sy-uname,
          lv_date    TYPE datum.

    DATA: lt_ec_008  TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008  TYPE zdt_ec_008,
          lt_ec_002  TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002  TYPE zdt_ec_002,
          lt_ec_001  TYPE STANDARD TABLE OF zdt_ec_001,
          ls_ec_001  TYPE zdt_ec_001,
          lt_ec_012  TYPE STANDARD TABLE OF zdt_ec_012,
          ls_ec_012  TYPE zdt_ec_012,
          lt_cre_fac TYPE TABLE FOR CREATE zcds_rv_doc_liq,
          lt_upd_fac TYPE TABLE FOR UPDATE zcds_rv_doc_liq,
          ls_ec_liq  TYPE zdt_fi_doc_liq.

    DATA: lo_liquidacion TYPE REF TO zcl_create_liquidacion,
          lo_xml         TYPE REF TO zcl_create_xml,
          lo_emision     TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu TYPE zts_inf_tribu,
          ls_liquida   TYPE zts_liqd_header,
          lt_impuesto  TYPE zcl_create_liquidacion=>ty_impuesto,
          lt_pagos     TYPE zcl_create_liquidacion=>ty_pagos,
          lt_detalle   TYPE zcl_create_liquidacion=>ty_detalle_f,
          lt_det_add   TYPE zcl_create_liquidacion=>ty_det_add,
          lt_det_imp   TYPE zcl_create_liquidacion=>ty_det_imp,
          lt_reembolso TYPE zcl_create_liquidacion=>ty_reembolso,
          lt_reem_imp  TYPE zcl_create_liquidacion=>ty_reem_imp,
          lt_head_add  TYPE zcl_create_liquidacion=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_liq  IN LOCAL MODE
      ENTITY LiquidationPurchase
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Supplier
               Businessname Typeid Idnumber Establishment Emissionpoint Sequential
               Accesskey Documenttype Issuedate Documentstatus Messagedocument
               Authorizationdate Xml Mimetype Filename Documentsupplier PostingDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(LiqPurchases).

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
    FROM zdt_ec_001
    INTO TABLE @lt_ec_001.

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    INTO TABLE @lt_ec_008.

    LOOP AT LiqPurchases ASSIGNING FIELD-SYMBOL(<fs_LiqPurchases>).


      READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode          = <fs_LiqPurchases>-CompanyCode
                                                 accountingdocumenttype = <fs_LiqPurchases>-Accountingdocumenttype
                                                 documentsri            = <fs_LiqPurchases>-Documenttype
                                                 users                  = sy-uname.

      IF sy-subrc EQ 0.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_LiqPurchases>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_LiqPurchases>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter

          <fs_LiqPurchases>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_LiqPurchases>-%tky ) TO failed-liquidationpurchase.

          APPEND VALUE #(  %tky        = <fs_LiqPurchases>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-liquidationpurchase.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_LiqPurchases>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_LiqPurchases>-%tky ) TO failed-liquidationpurchase.

        APPEND VALUE #(  %tky        = <fs_LiqPurchases>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-liquidationpurchase.

      ENDIF.

      READ TABLE lt_ec_001 INTO ls_ec_001 WITH KEY  CompanyCode = <fs_LiqPurchases>-companycode documenttype = <fs_LiqPurchases>-AccountingDocumentType
                                                    documentsri  = <fs_LiqPurchases>-documenttype.

      IF sy-subrc EQ 0 AND ls_ec_001-refunds IS NOT INITIAL.

        SELECT SINGLE mandt, companycode, fiscalyear, accountingdocument, accountingdocumenttype, filestatus, attachment, mimetype, filename, criticality
          FROM zdt_ec_012
         WHERE companycode            EQ @<fs_LiqPurchases>-Companycode
           AND fiscalyear             EQ @<fs_LiqPurchases>-Fiscalyear
           AND accountingdocument     EQ @<fs_LiqPurchases>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_LiqPurchases>-Accountingdocumenttype
           AND filestatus             EQ 'COMPLETE'
        INTO @ls_ec_012.

        IF sy-subrc NE 0.

          lv_flag = 'X'.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '007' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error ). "type of message

          <fs_LiqPurchases>-Messagedocument = 'Se debe Ingresar el Susntento de Liquidcion de Reembolso'.

          APPEND VALUE #(  %tky = <fs_LiqPurchases>-%tky ) TO failed-liquidationpurchase.

          APPEND VALUE #(  %tky        = <fs_LiqPurchases>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        =  lo_msg )
          TO reported-liquidationpurchase.

        ENDIF.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT LiqPurchases ASSIGNING <fs_LiqPurchases>.

        CREATE OBJECT lo_liquidacion
          EXPORTING
            companycode            = <fs_LiqPurchases>-Companycode
            fiscalyear             = <fs_LiqPurchases>-Fiscalyear
            accountingdocument     = <fs_LiqPurchases>-Accountingdocument
            accountingdocumenttype = <fs_LiqPurchases>-Accountingdocumenttype.

        CLEAR: ls_inf_tribu, ls_liquida, lt_impuesto, lt_pagos, lt_detalle,
               lt_det_add, lt_det_imp, lt_reembolso, lt_reem_imp, lt_head_add.

        CALL METHOD lo_liquidacion->callDocumentType
          EXPORTING
            documenttype = <fs_LiqPurchases>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            liquida      = ls_liquida
            t_impuesto   = lt_impuesto
            t_pagos      = lt_pagos
            t_detalle    = lt_detalle
            t_det_add    = lt_det_add
            t_det_imp    = lt_det_imp
            t_reembolso  = lt_reembolso
            t_reem_imp   = lt_reem_imp
            t_head_add   = lt_head_add.

        <fs_LiqPurchases>-Idnumber      = ls_liquida-identificacionproveedor.
        <fs_LiqPurchases>-Typeid        = ls_liquida-tipoidentificacionproveedor.
        <fs_LiqPurchases>-Businessname  = ls_liquida-razonsocialproveedor.
        <fs_LiqPurchases>-Establishment = ls_inf_tribu-estab.
        <fs_LiqPurchases>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_LiqPurchases>-Sequential    = ls_inf_tribu-secuencial.
        <fs_LiqPurchases>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_LiqPurchases>-issuedate     = <fs_LiqPurchases>-PostingDate.
*        <fs_LiqPurchases>-issuedate     = |{ <fs_LiqPurchases>-BillingDocumentDate DATE = ENVIRONMENT }|.

        CLEAR: lv_xml.

        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->liquidacioncompra
          EXPORTING
            header    = ls_liquida
            inf_tribu = ls_inf_tribu
            impuesto  = lt_impuesto
            pagos     = lt_pagos
            detalle   = lt_detalle
            det_add   = lt_det_add
            det_imp   = lt_det_imp
            reembolso = lt_reembolso
            reem_imp  = lt_reem_imp
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_LiqPurchases>-xml  = lv_raw.

        <fs_LiqPurchases>-Mimetype  = 'text/xml'.
        <fs_LiqPurchases>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            documenttype     = '06'
            companycode      = <fs_LiqPurchases>-Companycode
            xml              = lv_base64.

        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier = lv_id
            estado           = <fs_LiqPurchases>-Documentstatus
            messagedocument  = lv_mensaje ).

        <fs_LiqPurchases>-Documentsupplier = lv_id.
        <fs_LiqPurchases>-Messagedocument  = lv_mensaje.

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_LiqPurchases>-Accountingdocument } { <fs_LiqPurchases>-Documentstatus } { <fs_LiqPurchases>-Messagedocument } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-liquidationpurchase.

        FREE: lo_emision, lo_xml, lo_liquidacion.

      ENDLOOP.

      LOOP AT LiqPurchases ASSIGNING <fs_LiqPurchases>.

        SELECT SINGLE *
         FROM zdt_fi_doc_liq
         WHERE companycode            EQ @<fs_LiqPurchases>-Companycode
           AND fiscalyear             EQ @<fs_LiqPurchases>-Fiscalyear
           AND accountingdocument     EQ @<fs_LiqPurchases>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_LiqPurchases>-Accountingdocumenttype
         INTO @ls_ec_liq.

        IF sy-subrc NE 0.
          MOVE-CORRESPONDING <fs_LiqPurchases> TO ls_ec_liq.
          INSERT zdt_fi_doc_liq FROM @ls_ec_liq.
        ELSE.
          MOVE-CORRESPONDING <fs_LiqPurchases> TO ls_ec_liq.
          UPDATE zdt_fi_doc_liq FROM @ls_ec_liq.
        ENDIF.

      ENDLOOP.

    ENDIF.

    result = VALUE #( FOR LiqPurchase IN LiqPurchases
                    ( %tky        = LiqPurchase-%tky
                      %param      = LiqPurchase ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_liq  TYPE zdt_fi_doc_liq.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_liq  IN LOCAL MODE
      ENTITY LiquidationPurchase
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype
               Supplier Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(LiqPurchases)
      FAILED failed.

    LOOP AT LiqPurchases ASSIGNING FIELD-SYMBOL(<fs_LiqPurchase>).

      CLEAR: lv_date.

      lv_id = <fs_LiqPurchase>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_LiqPurchase>-Documenttype
          companycode      = <fs_LiqPurchase>-Companycode
          xml              = lv_xml.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_LiqPurchase>-Documentstatus
          messagedocument   = lv_mensaje ).

      IF lv_date IS NOT INITIAL.
        <fs_LiqPurchase>-Authorizationdate = lv_date.
      ENDIF.

      <fs_LiqPurchase>-Messagedocument  = lv_mensaje.

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_LiqPurchase>-Accountingdocument } { <fs_LiqPurchase>-Documentstatus } { <fs_LiqPurchase>-Messagedocument }{ <fs_LiqPurchase>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-liquidationpurchase.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT LiqPurchases ASSIGNING <fs_LiqPurchase>.

      SELECT SINGLE *
       FROM zdt_fi_doc_liq
       WHERE companycode            EQ @<fs_LiqPurchase>-Companycode
         AND fiscalyear             EQ @<fs_LiqPurchase>-Fiscalyear
         AND accountingdocument     EQ @<fs_LiqPurchase>-Accountingdocument
         AND accountingdocumenttype EQ @<fs_LiqPurchase>-Accountingdocumenttype
      INTO @ls_ec_liq.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_LiqPurchase> TO ls_ec_liq.
        UPDATE zdt_fi_doc_liq FROM @ls_ec_liq.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR LiqPurchase IN LiqPurchases
                    ( %tky = LiqPurchase-%tky
                      %param = LiqPurchase ) ).

  ENDMETHOD.

ENDCLASS.
