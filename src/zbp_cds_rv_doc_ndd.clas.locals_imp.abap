CLASS lhc_DebitNotes DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR DebitNotes RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION DebitNotes~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION DebitNotes~UpdateStatus RESULT result.


ENDCLASS.

CLASS lhc_DebitNotes IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_ndd  IN LOCAL MODE
      ENTITY DebitNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Documentstatus BillingDocumentIsCancelled )
      WITH CORRESPONDING #( keys )
      RESULT DATA(DebitNotes)
      FAILED failed.

    result = VALUE #( FOR DebitNote IN DebitNotes
                    ( %tky = DebitNote-%tky
                      %features-%action-SendDocument
          = COND #( WHEN DebitNote-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN DebitNote-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN DebitNote-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).


    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE DebitNotes INTO DATA(ls_DebitNote) WITH KEY companycode = <fs_result>-Companycode
                                                               fiscalyear = <fs_result>-Fiscalyear
                                                       accountingdocument = <fs_result>-Accountingdocument
                                                   accountingdocumenttype = <fs_result>-Accountingdocumenttype
                                                       billingdocument    = <fs_result>-Billingdocument.

      IF ls_DebitNote-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_DebitNote-BillingDocumentIsCancelled IS NOT INITIAL.
        <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
      ENDIF..

    ENDLOOP.

  ENDMETHOD.

  METHOD SendDocument.

    DATA: lv_flag    TYPE c,
          lv_xml     TYPE string,
          lv_raw     TYPE xstring,
          lv_base64  TYPE string,
          lv_mensaje TYPE string,
          lv_id      TYPE sgtxt,
          lv_date    TYPE datum.

    DATA: ls_ec_ndd  TYPE zdt_sd_doc_ndd.

    DATA: lt_ec_008 TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008 TYPE zdt_ec_008,
          lt_ec_002 TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002 TYPE zdt_ec_002.

    DATA: lo_DebitNotes TYPE REF TO zcl_create_nota_debito,
          lo_xml        TYPE REF TO zcl_create_xml,
          lo_emision    TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu  TYPE zts_inf_tribu,
          ls_notadebito TYPE zts_nd_header,
          lt_impuesto   TYPE zcl_create_nota_debito=>ty_impuesto,
          lt_pagos      TYPE zcl_create_nota_debito=>ty_pagos,
          lt_motivos    TYPE zcl_create_nota_debito=>ty_motivos,
          lt_head_add   TYPE zcl_create_nota_debito=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_ndd  IN LOCAL MODE
      ENTITY DebitNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier
               salesorganization BillingDocumentDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(DebitNotes).

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    INTO TABLE @lt_ec_008.

    LOOP AT DebitNotes ASSIGNING FIELD-SYMBOL(<fs_DebitNote>).

      SELECT SINGLE plant
       FROM I_BillingDocumentItem
       WHERE plant IS NOT INITIAL
         AND BillingDocument     EQ @<fs_DebitNote>-BillingDocument
         AND BillingDocumentType EQ @<fs_DebitNote>-Billingdocumenttype
       INTO @DATA(lv_plant).

      IF sy-subrc EQ 0.
        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_DebitNote>-CompanyCode
                                                   billingdocumenttype = <fs_DebitNote>-billingdocumenttype
                                                   salesorganization   = <fs_DebitNote>-salesorganization
                                                   plant               = lv_plant
                                                   documentsri         = <fs_DebitNote>-Documenttype
                                                   users               = sy-uname.
        IF sy-subrc NE 0.
          READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_DebitNote>-CompanyCode
                                                     billingdocumenttype = <fs_DebitNote>-billingdocumenttype
                                                     plant               = lv_plant
                                                     documentsri         = <fs_DebitNote>-Documenttype
                                                     users               = sy-uname.

          IF sy-subrc NE 0.
            READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_DebitNote>-CompanyCode
                                                       billingdocumenttype = <fs_DebitNote>-billingdocumenttype
                                                       salesorganization   = <fs_DebitNote>-salesorganization
                                                       documentsri         = <fs_DebitNote>-Documenttype
                                                       users               = sy-uname.
          ENDIF.
        ENDIF.

      ELSE.

        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode           = <fs_DebitNote>-CompanyCode
                                                       billingdocumenttype = <fs_DebitNote>-billingdocumenttype
                                                       salesorganization   = <fs_DebitNote>-salesorganization
                                                       documentsri         = <fs_DebitNote>-Documenttype
                                                       users               = sy-uname.
      ENDIF.

      IF ls_ec_008 IS NOT INITIAL.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_DebitNote>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_DebitNote>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter

          <fs_DebitNote>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_DebitNote>-%tky ) TO failed-DebitNotes.

          APPEND VALUE #(  %tky        = <fs_DebitNote>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-DebitNotes.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_DebitNote>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_DebitNote>-%tky ) TO failed-DebitNotes.

        APPEND VALUE #(  %tky        = <fs_DebitNote>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-DebitNotes.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT DebitNotes ASSIGNING <fs_DebitNote>.

        CREATE OBJECT lo_DebitNotes
          EXPORTING
            companycode            = <fs_DebitNote>-Companycode
            fiscalyear             = <fs_DebitNote>-Fiscalyear
            accountingdocument     = <fs_DebitNote>-Accountingdocument
            accountingdocumenttype = <fs_DebitNote>-Accountingdocumenttype
            billingdocument        = <fs_DebitNote>-Billingdocument
            billingdocumenttype    = <fs_DebitNote>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_notadebito, lt_impuesto, lt_pagos, lt_motivos,
               lt_head_add.

        CALL METHOD lo_DebitNotes->callDocumentType
          EXPORTING
            documenttype = <fs_DebitNote>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            notadebito   = ls_notadebito
            t_impuesto   = lt_impuesto
            t_motivos    = lt_motivos
            t_pagos      = lt_pagos
            t_head_add   = lt_head_add.

        <fs_DebitNote>-Idnumber      = ls_notadebito-idcomprador.
        <fs_DebitNote>-Typeid        = ls_notadebito-tipoidcomprador.
        <fs_DebitNote>-Businessname  = ls_notadebito-razonsocialcomprador.
        <fs_DebitNote>-Establishment = ls_inf_tribu-estab.
        <fs_DebitNote>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_DebitNote>-Sequential    = ls_inf_tribu-secuencial.
        <fs_DebitNote>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_DebitNote>-issuedate     = <fs_debitnote>-BillingDocumentDate.

        CLEAR: lv_xml.
        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->notadedito
          EXPORTING
            header    = ls_notadebito
            inf_tribu = ls_inf_tribu
            impuesto  = lt_impuesto
            motivos   = lt_motivos
            pagos     = lt_pagos
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_DebitNote>-xml  = lv_raw.
        <fs_DebitNote>-Mimetype  = 'text/xml'.
        <fs_DebitNote>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            documenttype     = <fs_DebitNote>-Documenttype
            companycode      = <fs_DebitNote>-Companycode
            xml              = lv_base64.

        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier = lv_id
            estado           = <fs_DebitNote>-Documentstatus
            messagedocument  = lv_mensaje ).

        <fs_DebitNote>-Documentsupplier = lv_id.
        <fs_DebitNote>-Messagedocument  = lv_mensaje.

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_DebitNote>-BillingDocument } { <fs_DebitNote>-Documentstatus } { <fs_DebitNote>-Messagedocument }{ <fs_DebitNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-debitnotes.

        FREE: lo_emision, lo_xml, lo_DebitNotes.

      ENDLOOP.

      LOOP AT DebitNotes ASSIGNING <fs_DebitNote>.

        SELECT SINGLE *
          FROM zdt_sd_doc_ndd
          WHERE companycode           EQ @<fs_DebitNote>-Companycode
           AND fiscalyear             EQ @<fs_DebitNote>-Fiscalyear
           AND accountingdocument     EQ @<fs_DebitNote>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_DebitNote>-Accountingdocumenttype
           AND billingdocument        EQ @<fs_DebitNote>-Billingdocument
           AND billingdocumenttype    EQ @<fs_DebitNote>-Billingdocumenttype
          INTO @ls_ec_ndd.

        IF sy-subrc NE 0.
          MOVE-CORRESPONDING <fs_DebitNote> TO ls_ec_ndd.
          INSERT zdt_sd_doc_ndd FROM @ls_ec_ndd.
        ELSE.
          MOVE-CORRESPONDING <fs_DebitNote> TO ls_ec_ndd.
          UPDATE zdt_sd_doc_ndd FROM @ls_ec_ndd.
        ENDIF.

      ENDLOOP.

    ENDIF.

    result = VALUE #( FOR DebitNote IN DebitNotes
                    ( %tky = DebitNote-%tky
                      %param = DebitNote ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ndd  TYPE zdt_sd_doc_ndd.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_ndd  IN LOCAL MODE
      ENTITY DebitNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(DebitNotes).

    LOOP AT DebitNotes ASSIGNING FIELD-SYMBOL(<fs_DebitNote>).

      CLEAR: lv_date.

      lv_id = <fs_DebitNote>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_DebitNote>-Documenttype
          companycode      = <fs_DebitNote>-Companycode
          xml              = lv_xml.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_DebitNote>-Documentstatus
          messagedocument   = lv_mensaje ).


      IF lv_date IS NOT INITIAL.
        <fs_DebitNote>-Authorizationdate = lv_date.
      ENDIF.

      <fs_DebitNote>-Messagedocument  = lv_mensaje.

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_DebitNote>-BillingDocument } { <fs_DebitNote>-Documentstatus } { <fs_DebitNote>-Messagedocument }{ <fs_DebitNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-debitnotes.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT DebitNotes ASSIGNING <fs_DebitNote>.

      SELECT SINGLE *
       FROM zdt_sd_doc_ndd
       WHERE companycode            EQ @<fs_DebitNote>-Companycode
         AND fiscalyear             EQ @<fs_DebitNote>-Fiscalyear
         AND accountingdocument     EQ @<fs_DebitNote>-Accountingdocument
         AND accountingdocumenttype EQ @<fs_DebitNote>-Accountingdocumenttype
         AND billingdocument        EQ @<fs_DebitNote>-Billingdocument
         AND billingdocumenttype    EQ @<fs_DebitNote>-Billingdocumenttype
       INTO @ls_ec_ndd.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_DebitNote> TO ls_ec_ndd.
        UPDATE zdt_sd_doc_ndd FROM @ls_ec_ndd.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR DebitNote IN DebitNotes
                    ( %tky = DebitNote-%tky
                      %param = DebitNote ) ).

  ENDMETHOD.

ENDCLASS.
