CLASS lhc_TransportGuides DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR TransportGuides RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION TransportGuides~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION TransportGuides~UpdateStatus RESULT result.

ENDCLASS.

CLASS lhc_TransportGuides IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_sd_guia IN LOCAL MODE
      ENTITY TransportGuides
      FIELDS ( Companycode Deliverydocument Fiscalyear Deliverydocumenttype Documentstatus OverallSDProcessStatus )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides)
      FAILED failed.

    READ ENTITIES OF zcds_rv_doc_sd_guia  IN LOCAL MODE
      ENTITY TransportGuides BY \_TransportData
      FIELDS ( Companycode Deliverydocument Fiscalyear Deliverydocumenttype
               Carrierid Typeid Businessname Carplate Startdate Enddate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransDatas).

    result = VALUE #( FOR TransGuide IN TransGuides
                      ( %tky = TransGuide-%tky
                        %features-%action-SendDocument
          = COND #( WHEN TransGuide-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN TransGuide-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN TransGuide-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE TransGuides INTO DATA(ls_TransGuide) WITH KEY %tky-Companycode           = <fs_result>-Companycode
                                                               %tky-Deliverydocument      = <fs_result>-Deliverydocument
                                                               %tky-Fiscalyear            = <fs_result>-Fiscalyear
                                                               %tky-Deliverydocumenttype  = <fs_result>-Deliverydocumenttype.

      IF sy-subrc EQ 0 AND ( ls_TransGuide-Documentstatus IS INITIAL OR
         ls_TransGuide-Documentstatus EQ 'PENDING' OR ls_TransGuide-Documentstatus EQ 'ERROR' )
         AND ls_TransGuide-OverallGoodsMovementStatus EQ 'C'.

        READ TABLE TransDatas INTO DATA(ls_TransDatas) WITH KEY %tky-Companycode          = <fs_result>-Companycode
                                                               %tky-Deliverydocument      = <fs_result>-Deliverydocument
                                                               %tky-Fiscalyear            = <fs_result>-Fiscalyear
                                                               %tky-Deliverydocumenttype  = <fs_result>-Deliverydocumenttype.
        IF sy-subrc EQ 0.
          <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-enabled.
        ELSE.
          <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
        ENDIF.

      ELSE.
        <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_TransGuide-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
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
          lv_Fiscalyear TYPE gjahr,
          lv_date    TYPE datum.

    DATA: ls_ec_guia TYPE zdt_sd_doc_guia.

    DATA: lt_ec_008 TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008 TYPE zdt_ec_008,
          lt_ec_002 TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002 TYPE zdt_ec_002.

    DATA: lo_GuaiTras TYPE REF TO zcl_create_guia,
          lo_xml      TYPE REF TO zcl_create_xml,
          lo_emision  TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu TYPE zts_inf_tribu,
          ls_guia      TYPE zts_guia_header,
          lt_detalle   TYPE zcl_create_guia=>ty_detalle_g,
          lt_det_add   TYPE zcl_create_guia=>ty_det_add,
          lt_head_add  TYPE zcl_create_guia=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_sd_guia  IN LOCAL MODE
      ENTITY TransportGuides
      FIELDS ( Companycode Fiscalyear Deliverydocument Deliverydocumenttype Shiptoparty
               Businessname Typeid Idnumber Establishment Emissionpoint Sequential
               Accesskey Documenttype Issuedate Documentstatus Messagedocument Authorizationdate
               Xml Mimetype Filename Documentsupplier SalesOrganization DeliveryDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides).

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    INTO TABLE @lt_ec_008.

    LOOP AT TransGuides ASSIGNING FIELD-SYMBOL(<fs_TransGuides>).

      SELECT SINGLE plant
       FROM I_DeliveryDocumentItem
       WHERE plant IS NOT INITIAL
         AND DeliveryDocument     EQ @<fs_TransGuides>-Deliverydocument
       INTO @DATA(lv_plant).

      IF sy-subrc EQ 0.
        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_TransGuides>-CompanyCode
                                                   goodsmovementtype   = <fs_TransGuides>-Deliverydocumenttype
                                                   salesorganization   = <fs_TransGuides>-SalesOrganization
                                                   plant               = lv_Plant
                                                   documentsri         = <fs_TransGuides>-Documenttype
                                                   users               = sy-uname.
        IF sy-subrc NE 0.
          READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode        = <fs_TransGuides>-CompanyCode
                                                     deliverydocumenttype = <fs_TransGuides>-Deliverydocumenttype
                                                     plant                = lv_Plant
                                                     documentsri          = <fs_TransGuides>-Documenttype
                                                     users                = sy-uname.

          IF sy-subrc NE 0.
            READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode         = <fs_TransGuides>-CompanyCode
                                                       deliverydocumenttype  = <fs_TransGuides>-Deliverydocumenttype
                                                       salesorganization     = <fs_TransGuides>-SalesOrganization
                                                       documentsri           = <fs_TransGuides>-Documenttype
                                                       users                 = sy-uname.
          ENDIF.
        ENDIF.

      ELSE.

        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode           = <fs_TransGuides>-CompanyCode
                                                     deliverydocumenttype  = <fs_TransGuides>-Deliverydocumenttype
                                                       salesorganization   = <fs_TransGuides>-SalesOrganization
                                                       documentsri         = <fs_TransGuides>-Documenttype
                                                       users               = sy-uname.
      ENDIF.

      IF ls_ec_008 IS NOT INITIAL.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_TransGuides>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_TransGuides>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter

          <fs_TransGuides>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_TransGuides>-%tky ) TO failed-TransportGuides.

          APPEND VALUE #(  %tky        = <fs_TransGuides>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-TransportGuides.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_TransGuides>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_TransGuides>-%tky ) TO failed-TransportGuides.

        APPEND VALUE #(  %tky        = <fs_TransGuides>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-TransportGuides.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT TransGuides ASSIGNING <fs_TransGuides>.

        lv_fiscalyear = <fs_TransGuides>-Fiscalyear.
        CREATE OBJECT lo_GuaiTras
          EXPORTING
            companycode          = <fs_TransGuides>-Companycode
            Deliverydocument     = <fs_TransGuides>-Deliverydocument
            fiscalyear           = lv_fiscalyear
            deliverydocumenttype = <fs_TransGuides>-Deliverydocumenttype.

        CLEAR: ls_inf_tribu, ls_guia, lt_detalle, lt_det_add, lt_head_add.

        CALL METHOD lo_GuaiTras->callDocumentType
          EXPORTING
            documenttype = <fs_TransGuides>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            guia         = ls_guia
            t_detalle_g  = lt_detalle
            t_det_add    = lt_det_add
            t_head_add   = lt_head_add.

        <fs_TransGuides>-Idnumber      = ls_guia-id_destinatario.
        <fs_TransGuides>-Establishment = ls_inf_tribu-estab.
        <fs_TransGuides>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_TransGuides>-Sequential    = ls_inf_tribu-secuencial.
        <fs_TransGuides>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_TransGuides>-issuedate     = ls_guia-fecha.

        CLEAR: lv_xml.
        CREATE OBJECT lo_xml
        .
        CALL METHOD lo_xml->guiaremision
          EXPORTING
            header    = ls_guia
            inf_tribu = ls_inf_tribu
            detalle   = lt_detalle
            det_add   = lt_det_add
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_TransGuides>-xml  = lv_raw.
        <fs_TransGuides>-Mimetype  = 'text/xml'.
        <fs_TransGuides>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            documenttype     = '03'
            companycode      = <fs_TransGuides>-Companycode
            xml              = lv_base64.

        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier  = lv_id
            estado            = <fs_TransGuides>-Documentstatus
            messagedocument   = lv_mensaje ).

        <fs_TransGuides>-Documentsupplier = lv_id.
        <fs_TransGuides>-Messagedocument  = lv_mensaje.

        FREE: lo_emision, lo_xml, lo_guaitras.

      ENDLOOP.

      LOOP AT TransGuides ASSIGNING <fs_TransGuides>.

        lv_fiscalyear = <fs_TransGuides>-fiscalyear.

        SELECT SINGLE *
          FROM zdt_sd_doc_guia
          WHERE companycode           EQ @<fs_TransGuides>-Companycode
           AND fiscalyear             EQ @<fs_TransGuides>-Fiscalyear
           AND deliverydocument       EQ @<fs_TransGuides>-Deliverydocument
           AND deliverydocumenttype   EQ @<fs_TransGuides>-Deliverydocumenttype
          INTO @ls_ec_guia.

         IF sy-subrc NE 0.
           MOVE-CORRESPONDING <fs_TransGuides> TO ls_ec_guia.
           ls_ec_guia-fiscalyear = lv_fiscalyear.
           INSERT zdt_sd_doc_guia FROM @ls_ec_guia.
         ELSE.
          MOVE-CORRESPONDING <fs_TransGuides> TO ls_ec_guia.
          ls_ec_guia-fiscalyear = lv_fiscalyear.
          UPDATE zdt_sd_doc_guia FROM @ls_ec_guia.
        ENDIF.

      ENDLOOP.

    ENDIF.

    result = VALUE #( FOR TransGuide IN TransGuides
                    ( %tky   = TransGuide-%tky
                      %param = TransGuide ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_guia TYPE zdt_sd_doc_guia.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_sd_guia IN LOCAL MODE
      ENTITY TransportGuides
      FIELDS ( Companycode Fiscalyear Deliverydocument Deliverydocumenttype Shiptoparty
               Businessname Typeid Idnumber Establishment Emissionpoint Sequential
               Accesskey Documenttype Issuedate Documentstatus Messagedocument Authorizationdate
               Xml Mimetype Filename Documentsupplier SalesOrganization DeliveryDate )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides).

    LOOP AT TransGuides ASSIGNING FIELD-SYMBOL(<fs_TransGuides>).

      CLEAR: lv_date.

      lv_id = <fs_TransGuides>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_TransGuides>-Documenttype
          companycode      = <fs_TransGuides>-Companycode
          xml              = lv_xml.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_TransGuides>-Documentstatus
          messagedocument   = lv_mensaje ).

      IF lv_date IS NOT INITIAL.
        <fs_TransGuides>-Authorizationdate = lv_date.
      ENDIF.

      <fs_TransGuides>-Messagedocument  = lv_mensaje.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT TransGuides ASSIGNING <fs_TransGuides>.

      SELECT SINGLE *
        FROM zdt_sd_doc_guia
       WHERE companycode            EQ @<fs_TransGuides>-Companycode
         AND fiscalyear             EQ @<fs_TransGuides>-Fiscalyear
         AND deliverydocument       EQ @<fs_TransGuides>-Deliverydocument
         AND deliverydocumenttype   EQ @<fs_TransGuides>-Deliverydocumenttype
        INTO @ls_ec_guia.

       IF sy-subrc NE 0.
         MOVE-CORRESPONDING <fs_TransGuides> TO ls_ec_guia.
         INSERT zdt_sd_doc_guia FROM @ls_ec_guia.
      ELSE.
        MOVE-CORRESPONDING <fs_TransGuides> TO ls_ec_guia.
        UPDATE zdt_sd_doc_guia FROM @ls_ec_guia.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR TransGuide IN TransGuides
                    ( %tky   = TransGuide-%tky
                      %param = TransGuide ) ).

  ENDMETHOD.

ENDCLASS.
