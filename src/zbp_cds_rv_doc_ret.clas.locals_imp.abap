CLASS lhc_Withholdings DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Withholdings RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION Withholdings~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION Withholdings~UpdateStatus RESULT result.

ENDCLASS.

CLASS lhc_Withholdings IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_ret  IN LOCAL MODE
      ENTITY Withholdings
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Documentstatus ReverseDocument )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Withholdings)
      FAILED failed.

    result = VALUE #( FOR Withholding IN Withholdings
                    ( %tky = Withholding-%tky
                      %features-%action-SendDocument
          = COND #( WHEN Withholding-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN Withholding-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN Withholding-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE Withholdings INTO DATA(ls_Withholding) WITH KEY companycode = <fs_result>-Companycode
                                                                  fiscalyear = <fs_result>-Fiscalyear
                                                          accountingdocument = <fs_result>-Accountingdocument
                                                      accountingdocumenttype = <fs_result>-Accountingdocumenttype.

      IF ls_Withholding-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_Withholding-ReverseDocument IS NOT INITIAL.
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
          ls_ec_ret  TYPE zdt_fi_doc_ret.

    DATA: lo_Withholdings TYPE REF TO zcl_create_retencion,
          lo_xml         TYPE REF TO zcl_create_xml,
          lo_emision     TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu  TYPE zts_inf_tribu,
          ls_retencion  TYPE zts_rete_header,
          lt_pagos      TYPE zcl_create_retencion=>ty_pagos,
          lt_sustento   TYPE zcl_create_retencion=>ty_sustento,
          lt_impuesto_s TYPE zcl_create_retencion=>ty_imp_sust,
          lt_reembolso  TYPE zcl_create_retencion=>ty_reembolso,
          lt_retencion  TYPE zcl_create_retencion=>ty_retencion,
          lt_reem_imp   TYPE zcl_create_retencion=>ty_reem_imp,
          lt_head_add   TYPE zcl_create_retencion=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_ret  IN LOCAL MODE
      ENTITY Withholdings
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Supplier
               Businessname Typeid Idnumber Establishment Emissionpoint Sequential
               Accesskey Documenttype Issuedate Documentstatus Messagedocument
               Authorizationdate Xml Mimetype Filename Documentsupplier PostingDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Withholdings).

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

    LOOP AT Withholdings ASSIGNING FIELD-SYMBOL(<fs_Withholdings>).


      READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode            = <fs_Withholdings>-CompanyCode
                                                   accountingdocumenttype = <fs_Withholdings>-Accountingdocumenttype
                                                   documentsri            = <fs_Withholdings>-Documenttype
                                                   users                  = sy-uname.

      IF sy-subrc EQ 0.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_Withholdings>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_Withholdings>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter

          <fs_Withholdings>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_Withholdings>-%tky ) TO failed-withholdings.

          APPEND VALUE #(  %tky        = <fs_Withholdings>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-withholdings.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_Withholdings>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_Withholdings>-%tky ) TO failed-withholdings.

        APPEND VALUE #(  %tky        = <fs_Withholdings>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-withholdings.

      ENDIF.

      READ TABLE lt_ec_001 INTO ls_ec_001 WITH KEY  CompanyCode = <fs_Withholdings>-companycode documenttype = <fs_Withholdings>-AccountingDocumentType
                                                    documentsri  = <fs_Withholdings>-documenttype.

      IF sy-subrc EQ 0 AND ls_ec_001-refunds IS NOT INITIAL.

        SELECT SINGLE mandt, companycode, fiscalyear, accountingdocument, accountingdocumenttype, filestatus, attachment, mimetype, filename, criticality
          FROM zdt_ec_012
         WHERE companycode            EQ @<fs_Withholdings>-Companycode
           AND fiscalyear             EQ @<fs_Withholdings>-Fiscalyear
           AND accountingdocument     EQ @<fs_Withholdings>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_Withholdings>-Accountingdocumenttype
           AND filestatus             EQ 'COMPLETE'
        INTO @ls_ec_012.

        IF sy-subrc NE 0.

          lv_flag = 'X'.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '007' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error ). "type of message

          <fs_Withholdings>-Messagedocument = 'Se debe Ingresar el Susntento de Liquidcion de Reembolso'.

          APPEND VALUE #(  %tky = <fs_Withholdings>-%tky ) TO failed-withholdings.

          APPEND VALUE #(  %tky        = <fs_Withholdings>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        =  lo_msg )
          TO reported-withholdings.

        ENDIF.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT Withholdings ASSIGNING <fs_Withholdings>.

        CREATE OBJECT lo_Withholdings
          EXPORTING
            companycode            = <fs_Withholdings>-Companycode
            fiscalyear             = <fs_Withholdings>-Fiscalyear
            accountingdocument     = <fs_Withholdings>-Accountingdocument
            accountingdocumenttype = <fs_Withholdings>-Accountingdocumenttype.

        CLEAR: ls_inf_tribu, ls_retencion, lt_impuesto_s, lt_pagos, lt_reembolso, lt_reem_imp, lt_head_add.

        CALL METHOD lo_Withholdings->callDocumentType
          EXPORTING
            documenttype = <fs_Withholdings>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            retencion    = ls_retencion
            t_pagos      = lt_pagos
            t_sustento   = lt_sustento
            t_impuesto_s = lt_impuesto_s
            t_retencion  = lt_retencion
            t_reembolso  = lt_reembolso
            t_reem_imp   = lt_reem_imp
            t_head_add   = lt_head_add.

        <fs_Withholdings>-Idnumber      = ls_retencion-identificacionsujetoretenido.
        <fs_Withholdings>-Typeid        = ls_retencion-tipoidentificacionsujetoreteni.
        <fs_Withholdings>-Businessname  = ls_retencion-razonsocialsujetoretenido.
        <fs_Withholdings>-Establishment = ls_inf_tribu-estab.
        <fs_Withholdings>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_Withholdings>-Sequential    = ls_inf_tribu-secuencial.
        <fs_Withholdings>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_Withholdings>-issuedate     = <fs_Withholdings>-PostingDate.
*        <fs_Withholdings>-issuedate     = |{ <fs_Withholdings>-BillingDocumentDate DATE = ENVIRONMENT }|.

        CLEAR: lv_xml.

        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->retencion
          EXPORTING
            header    = ls_retencion
            inf_tribu = ls_inf_tribu
            pagos     = lt_pagos
            sustento  = lt_sustento
            impuesto  = lt_impuesto_s
            reembolso = lt_reembolso
            reem_imp  = lt_reem_imp
            retencion = lt_retencion
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_Withholdings>-xml  = lv_raw.

        <fs_Withholdings>-Mimetype  = 'text/xml'.
        <fs_Withholdings>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            documenttype     = <fs_Withholdings>-Documenttype
            companycode      = <fs_Withholdings>-Companycode
            xml              = lv_base64.

        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier = lv_id
            estado           = <fs_Withholdings>-Documentstatus
            messagedocument  = lv_mensaje ).

        <fs_Withholdings>-Documentsupplier = lv_id.
        <fs_Withholdings>-Messagedocument  = lv_mensaje.

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_Withholdings>-Accountingdocument } { <fs_Withholdings>-Documentstatus } { <fs_Withholdings>-Messagedocument } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-withholdings.

        FREE: lo_emision, lo_xml, lo_Withholdings.

      ENDLOOP.

      LOOP AT Withholdings ASSIGNING <fs_Withholdings>.

        SELECT SINGLE *
         FROM zdt_fi_doc_ret
         WHERE companycode            EQ @<fs_Withholdings>-Companycode
           AND fiscalyear             EQ @<fs_Withholdings>-Fiscalyear
           AND accountingdocument     EQ @<fs_Withholdings>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_Withholdings>-Accountingdocumenttype
         INTO @ls_ec_ret.

        IF sy-subrc NE 0.
          MOVE-CORRESPONDING <fs_Withholdings> TO ls_ec_ret.
          INSERT zdt_fi_doc_ret FROM @ls_ec_ret.
        ELSE.
          MOVE-CORRESPONDING <fs_Withholdings> TO ls_ec_ret.
          UPDATE zdt_fi_doc_ret FROM @ls_ec_ret.
        ENDIF.

      ENDLOOP.

    ENDIF.

    result = VALUE #( FOR Withholding IN Withholdings
                    ( %tky        = Withholding-%tky
                      %param      = Withholding ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ret  TYPE zdt_fi_doc_ret.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_ret  IN LOCAL MODE
      ENTITY Withholdings
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype
               Supplier Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Withholdings)
      FAILED failed.

    LOOP AT Withholdings ASSIGNING FIELD-SYMBOL(<fs_Withholdings>).

      CLEAR: lv_date.

      lv_id = <fs_Withholdings>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_Withholdings>-Documenttype
          companycode      = <fs_Withholdings>-Companycode
          xml              = lv_xml.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_Withholdings>-Documentstatus
          messagedocument   = lv_mensaje ).

      IF lv_date IS NOT INITIAL.
        <fs_Withholdings>-Authorizationdate = lv_date.
      ENDIF.

      <fs_Withholdings>-Messagedocument  = lv_mensaje.

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_Withholdings>-Accountingdocument } { <fs_Withholdings>-Documentstatus } { <fs_Withholdings>-Messagedocument }{ <fs_Withholdings>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-withholdings.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT Withholdings ASSIGNING <fs_Withholdings>.

      SELECT SINGLE *
       FROM zdt_fi_doc_ret
       WHERE companycode            EQ @<fs_Withholdings>-Companycode
         AND fiscalyear             EQ @<fs_Withholdings>-Fiscalyear
         AND accountingdocument     EQ @<fs_Withholdings>-Accountingdocument
         AND accountingdocumenttype EQ @<fs_Withholdings>-Accountingdocumenttype
      INTO @ls_ec_ret.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_Withholdings> TO ls_ec_ret.
        UPDATE zdt_fi_doc_liq FROM @ls_ec_ret.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR Withholding IN Withholdings
                    ( %tky = Withholding-%tky
                      %param = Withholding ) ).

  ENDMETHOD.

ENDCLASS.
