CLASS lhc_CreditNotes DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR CreditNotes RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION CreditNotes~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION CreditNotes~UpdateStatus RESULT result.

ENDCLASS.

CLASS lhc_CreditNotes IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_ndc  IN LOCAL MODE
      ENTITY CreditNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Documentstatus BillingDocumentIsCancelled )
      WITH CORRESPONDING #( keys )
      RESULT DATA(CreditNotes)
      FAILED failed.

    result = VALUE #( FOR CreditNote IN CreditNotes
                    ( %tky = CreditNote-%tky
                      %features-%action-SendDocument
          = COND #( WHEN CreditNote-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN CreditNote-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN CreditNote-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE CreditNotes INTO DATA(ls_CreditNote) WITH KEY companycode = <fs_result>-Companycode
                                                               fiscalyear = <fs_result>-Fiscalyear
                                                       accountingdocument = <fs_result>-Accountingdocument
                                                   accountingdocumenttype = <fs_result>-Accountingdocumenttype
                                                       billingdocument    = <fs_result>-Billingdocument.

      IF ls_CreditNote-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_CreditNote-BillingDocumentIsCancelled IS NOT INITIAL.
        <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
      ENDIF..

    ENDLOOP.

  ENDMETHOD.

  METHOD SendDocument.

    DATA: lv_flag    TYPE c,
          lv_raw     TYPE xstring,
          lv_xml     TYPE string,
          lv_base64  TYPE string,
          lv_mensaje TYPE string,
          lv_id      TYPE sgtxt,
          lv_date    TYPE datum.

    DATA: ls_ec_ndc  TYPE zdt_sd_doc_ndc.

    DATA: lt_ec_008 TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008 TYPE zdt_ec_008,
          lt_ec_002 TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002 TYPE zdt_ec_002.

    DATA: lo_CreditNotes TYPE REF TO zcl_create_nota_credito,
          lo_xml     TYPE REF TO zcl_create_xml,
          lo_emision TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu TYPE zts_inf_tribu,
          ls_notacredito   TYPE zts_nc_header,
          lt_impuesto  TYPE zcl_create_nota_credito=>ty_impuesto,
          lt_detalle   TYPE zcl_create_nota_credito=>ty_detalle_f,
          lt_det_add   TYPE zcl_create_nota_credito=>ty_det_add,
          lt_det_imp   TYPE zcl_create_nota_credito=>ty_det_imp,
          lt_head_add  TYPE zcl_create_nota_credito=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_ndc  IN LOCAL MODE
      ENTITY CreditNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier
               salesorganization BillingDocumentDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(CreditNotes).

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    INTO TABLE @lt_ec_008.

    LOOP AT CreditNotes ASSIGNING FIELD-SYMBOL(<fs_CreditNote>).

      SELECT SINGLE plant
       FROM I_BillingDocumentItem
       WHERE plant IS NOT INITIAL
         AND BillingDocument     EQ @<fs_CreditNote>-BillingDocument
         AND BillingDocumentType EQ @<fs_CreditNote>-Billingdocumenttype
       INTO @DATA(lv_plant).

      IF sy-subrc EQ 0.
        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_CreditNote>-CompanyCode
                                                   billingdocumenttype = <fs_CreditNote>-billingdocumenttype
                                                   salesorganization   = <fs_CreditNote>-salesorganization
                                                   plant               = lv_plant
                                                   documentsri         = <fs_CreditNote>-Documenttype
                                                   users               = sy-uname.
        IF sy-subrc NE 0.
          READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_CreditNote>-CompanyCode
                                                     billingdocumenttype = <fs_CreditNote>-billingdocumenttype
                                                     plant               = lv_plant
                                                     documentsri         = <fs_CreditNote>-Documenttype
                                                     users               = sy-uname.

          IF sy-subrc NE 0.
            READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_CreditNote>-CompanyCode
                                                       billingdocumenttype = <fs_CreditNote>-billingdocumenttype
                                                       salesorganization   = <fs_CreditNote>-salesorganization
                                                       documentsri         = <fs_CreditNote>-Documenttype
                                                       users               = sy-uname.
          ENDIF.
        ENDIF.

      ELSE.

        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode           = <fs_CreditNote>-CompanyCode
                                                       billingdocumenttype = <fs_CreditNote>-billingdocumenttype
                                                       salesorganization   = <fs_CreditNote>-salesorganization
                                                       documentsri         = <fs_CreditNote>-Documenttype
                                                       users               = sy-uname.
      ENDIF.

      IF ls_ec_008 IS NOT INITIAL.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_CreditNote>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_CreditNote>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter

          <fs_CreditNote>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_CreditNote>-%tky ) TO failed-creditnotes.

          APPEND VALUE #(  %tky        = <fs_CreditNote>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-creditnotes.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_CreditNote>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_CreditNote>-%tky ) TO failed-creditnotes.

        APPEND VALUE #(  %tky        = <fs_CreditNote>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-creditnotes.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT CreditNotes ASSIGNING <fs_CreditNote>.

        CREATE OBJECT lo_CreditNotes
          EXPORTING
            companycode            = <fs_CreditNote>-Companycode
            fiscalyear             = <fs_CreditNote>-Fiscalyear
            accountingdocument     = <fs_CreditNote>-Accountingdocument
            accountingdocumenttype = <fs_CreditNote>-Accountingdocumenttype
            billingdocument        = <fs_CreditNote>-Billingdocument
            billingdocumenttype    = <fs_CreditNote>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_notacredito, lt_impuesto, lt_detalle,
               lt_det_add, lt_det_imp, lt_head_add.

        CALL METHOD lo_CreditNotes->callDocumentType
          EXPORTING
            documenttype = <fs_CreditNote>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            notacredito  = ls_notacredito
            t_impuesto   = lt_impuesto
            t_detalle    = lt_detalle
            t_det_add    = lt_det_add
            t_det_imp    = lt_det_imp
            t_head_add   = lt_head_add.

        <fs_CreditNote>-Idnumber      = ls_notacredito-idcomprador.
        <fs_CreditNote>-Typeid        = ls_notacredito-tipoidcomprador.
        <fs_CreditNote>-Businessname  = ls_notacredito-razonsocialcomprador.
        <fs_CreditNote>-Establishment = ls_inf_tribu-estab.
        <fs_CreditNote>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_CreditNote>-Sequential    = ls_inf_tribu-secuencial.
        <fs_CreditNote>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_CreditNote>-issuedate     = <fs_creditnote>-BillingDocumentDate.

        CLEAR: lv_xml.
        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->notacredito
          EXPORTING
            header    = ls_notacredito
            inf_tribu = ls_inf_tribu
            impuesto  = lt_impuesto
            detalle   = lt_detalle
            det_add   = lt_det_add
            det_imp   = lt_det_imp
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_CreditNote>-xml  = lv_raw.
        <fs_CreditNote>-Mimetype  = 'text/xml'.
        <fs_CreditNote>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            documenttype     = <fs_CreditNote>-Documenttype
            companycode      = <fs_CreditNote>-Companycode
            xml              = lv_base64.

        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier  = lv_id
            estado            = <fs_CreditNote>-Documentstatus
            messagedocument   = lv_mensaje ).

        <fs_CreditNote>-Documentsupplier = lv_id.
        <fs_CreditNote>-Messagedocument  = lv_mensaje.

        FREE: lo_emision, lo_xml, lo_CreditNotes.

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_CreditNote>-BillingDocument } { <fs_CreditNote>-Documentstatus } { <fs_CreditNote>-Messagedocument }{ <fs_CreditNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
         ) INTO TABLE reported-creditnotes.

      ENDLOOP.

      LOOP AT CreditNotes ASSIGNING <fs_CreditNote>.

        SELECT SINGLE *
         FROM zdt_sd_doc_ndc
         WHERE companycode            EQ @<fs_CreditNote>-Companycode
           AND fiscalyear             EQ @<fs_CreditNote>-Fiscalyear
           AND accountingdocument     EQ @<fs_CreditNote>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_CreditNote>-Accountingdocumenttype
           AND billingdocument        EQ @<fs_CreditNote>-Billingdocument
           AND billingdocumenttype    EQ @<fs_CreditNote>-Billingdocumenttype
          INTO @ls_ec_ndc.

        IF sy-subrc NE 0.
          MOVE-CORRESPONDING <fs_CreditNote> TO ls_ec_ndc.
          INSERT zdt_sd_doc_ndc FROM @ls_ec_ndc.
        ELSE.
          MOVE-CORRESPONDING <fs_CreditNote> TO ls_ec_ndc.
          UPDATE zdt_sd_doc_ndc FROM @ls_ec_ndc.
        ENDIF.

      ENDLOOP.

    ENDIF.

    result = VALUE #( FOR CreditNote IN CreditNotes
                    ( %tky        = CreditNote-%tky
                      %param      = CreditNote ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ndc  TYPE zdt_sd_doc_ndc.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_ndc  IN LOCAL MODE
      ENTITY CreditNotes
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(CreditNotes).

    LOOP AT CreditNotes ASSIGNING FIELD-SYMBOL(<fs_CreditNote>).

      CLEAR: lv_date.

      lv_id = <fs_CreditNote>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_CreditNote>-Documenttype
          companycode      = <fs_CreditNote>-Companycode
          xml              = lv_xml.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_CreditNote>-Documentstatus
          messagedocument   = lv_mensaje ).

      IF lv_date IS NOT INITIAL.
        <fs_CreditNote>-Authorizationdate = lv_date.
      ENDIF.

      <fs_CreditNote>-Messagedocument  = lv_mensaje.


      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_CreditNote>-BillingDocument } { <fs_CreditNote>-Documentstatus } { <fs_CreditNote>-Messagedocument }{ <fs_CreditNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-creditnotes.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT CreditNotes ASSIGNING <fs_CreditNote>.

      SELECT SINGLE *
       FROM zdt_sd_doc_ndc
       WHERE companycode            EQ @<fs_CreditNote>-Companycode
         AND fiscalyear             EQ @<fs_CreditNote>-Fiscalyear
         AND accountingdocument     EQ @<fs_CreditNote>-Accountingdocument
         AND accountingdocumenttype EQ @<fs_CreditNote>-Accountingdocumenttype
         AND billingdocument        EQ @<fs_CreditNote>-Billingdocument
         AND billingdocumenttype    EQ @<fs_CreditNote>-Billingdocumenttype
       INTO @ls_ec_ndc.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_CreditNote> TO ls_ec_ndc.
        UPDATE zdt_sd_doc_ndc FROM @ls_ec_ndc.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR CreditNote IN CreditNotes
                    ( %tky = CreditNote-%tky
                      %param = CreditNote ) ).


  ENDMETHOD.

ENDCLASS.
