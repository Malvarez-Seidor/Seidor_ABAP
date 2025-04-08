CLASS lsc_zcds_rv_doc_mm_guia DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_mm_guia IMPLEMENTATION.

  METHOD finalize.

  ENDMETHOD.

  METHOD check_before_save.

  ENDMETHOD.

  METHOD cleanup.

  ENDMETHOD.

  METHOD cleanup_finalize.

  ENDMETHOD.

  METHOD save.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_TransferGuides DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR TransferGuides RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION TransferGuides~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION TransferGuides~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR TransferGuides RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR TransferGuides RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE TransferGuides.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE TransferGuides.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE TransferGuides.

    METHODS read FOR READ
      IMPORTING keys FOR READ TransferGuides RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK TransferGuides.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

    METHODS rba_Transportdata FOR READ
      IMPORTING keys_rba FOR READ TransferGuides\_Transportdata FULL result_requested RESULT result LINK association_links.

    METHODS cba_Transportdata FOR MODIFY
      IMPORTING entities_cba FOR CREATE TransferGuides\_Transportdata.

ENDCLASS.

CLASS lhc_TransferGuides IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_mm_guia IN LOCAL MODE
      ENTITY TransferGuides
      FIELDS ( Companycode Materialdocument MaterialDocumentyear GoodsMovementType Documentstatus GoodsMovementIsCancelled )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides)
      FAILED failed.

    READ ENTITIES OF zcds_rv_doc_mm_guia  IN LOCAL MODE
      ENTITY TransferGuides BY \_TransportData
      FIELDS ( Companycode MaterialDocumentyear Materialdocument goodsmovementtype
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

      READ TABLE TransGuides INTO DATA(ls_TransGuide) WITH KEY %tky-Companycode          = <fs_result>-Companycode
                                                               %tky-Materialdocument     = <fs_result>-Materialdocument
                                                               %tky-MaterialDocumentyear = <fs_result>-Materialdocumentyear
                                                               %tky-GoodsMovementType    = <fs_result>-Goodsmovementtype.

      IF sy-subrc EQ 0 AND ( ls_TransGuide-Documentstatus IS INITIAL OR
         ls_TransGuide-Documentstatus EQ 'PENDING' OR ls_TransGuide-Documentstatus EQ 'ERROR' )
         AND ls_TransGuide-GoodsMovementIsCancelled IS INITIAL.

        READ TABLE TransDatas INTO DATA(ls_TransDatas) WITH KEY %tky-Companycode          = <fs_result>-Companycode
                                                                %tky-Materialdocument     = <fs_result>-Materialdocument
                                                                %tky-MaterialDocumentyear = <fs_result>-Materialdocumentyear
                                                                %tky-GoodsMovementType    = <fs_result>-Goodsmovementtype.
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
          lv_clave   TYPE string,
          lv_id      TYPE sgtxt,
          lv_date    TYPE datum,
          lv_message TYPE string,
          lv_update  TYPE c.

    DATA: ls_ec_guia TYPE zdt_mm_doc_guia.

    DATA: lt_cre_guia  TYPE TABLE FOR CREATE zcds_rv_doc_mm_guia,
          lt_upd_guia  TYPE TABLE FOR UPDATE zcds_rv_doc_mm_guia.

    DATA: lt_ec_008 TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008 TYPE zdt_ec_008,
          lt_ec_002 TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002 TYPE zdt_ec_002.

    DATA: lo_GuaiTras TYPE REF TO zcl_create_guia_traslado,
          lo_xml      TYPE REF TO zcl_create_xml_emi,
          lo_emision  TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu TYPE zts_inf_tribu,
          ls_guia      TYPE zts_guia_header,
          lt_detalle   TYPE zcl_create_guia_traslado=>ty_detalle_g,
          lt_det_add   TYPE zcl_create_guia_traslado=>ty_det_add,
          lt_head_add  TYPE zcl_create_guia_traslado=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_mm_guia  IN LOCAL MODE
      ENTITY TransferGuides
      FIELDS ( Companycode MaterialDocumentyear Materialdocument goodsmovementtype
               Idnumber Establishment Emissionpoint Sequential Accesskey
               Documenttype Issuedate Documentstatus Messagedocument Authorizationdate
               Xml Mimetype Filename Documentsupplier Plant StorageLocation )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides).

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    WHERE companycode NE @space
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE users EQ @sy-uname
    INTO TABLE @lt_ec_008.

    LOOP AT TransGuides ASSIGNING FIELD-SYMBOL(<fs_TransGuides>).

      IF sy-subrc EQ 0.
        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_TransGuides>-CompanyCode
                                                   goodsmovementtype   = <fs_TransGuides>-GoodsMovementType
                                                   storagelocation     = <fs_TransGuides>-StorageLocation
                                                   plant               = <fs_TransGuides>-Plant
                                                   documentsri         = <fs_TransGuides>-Documenttype
                                                   users               = sy-uname.
        IF sy-subrc NE 0.
          READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_TransGuides>-CompanyCode
                                                     goodsmovementtype   = <fs_TransGuides>-GoodsMovementType
                                                     plant               = <fs_TransGuides>-Plant
                                                     documentsri         = <fs_TransGuides>-Documenttype
                                                     users               = sy-uname.

          IF sy-subrc NE 0.
            READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_TransGuides>-CompanyCode
                                                       goodsmovementtype   = <fs_TransGuides>-GoodsMovementType
                                                       storagelocation     = <fs_TransGuides>-StorageLocation
                                                       documentsri         = <fs_TransGuides>-Documenttype
                                                       users               = sy-uname.
          ENDIF.
        ENDIF.

      ELSE.

        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode           = <fs_TransGuides>-CompanyCode
                                                       goodsmovementtype   = <fs_TransGuides>-GoodsMovementType
                                                       storagelocation     = <fs_TransGuides>-StorageLocation
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

          APPEND VALUE #(  %tky = <fs_TransGuides>-%tky ) TO failed-transferguides.

          APPEND VALUE #(  %tky        = <fs_TransGuides>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-transferguides.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_TransGuides>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_TransGuides>-%tky ) TO failed-transferguides.

        APPEND VALUE #(  %tky        = <fs_TransGuides>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-transferguides.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT TransGuides ASSIGNING <fs_TransGuides>.

        CLEAR: lv_update.

        if <fs_TransGuides>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_GuaiTras
          EXPORTING
            companycode          = <fs_TransGuides>-Companycode
            materialdocumentyear = <fs_TransGuides>-MaterialDocumentyear
            materialdocument     = <fs_TransGuides>-Materialdocument
            goodsmovementtype    = <fs_TransGuides>-GoodsMovementType.

        CLEAR: ls_inf_tribu, ls_guia, lt_detalle, lt_det_add, lt_head_add, lv_message.

        CALL METHOD lo_GuaiTras->callDocumentType
          EXPORTING
            documenttype = <fs_TransGuides>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            guia         = ls_guia
            t_detalle_g  = lt_detalle
            t_det_add    = lt_det_add
            t_head_add   = lt_head_add
            message      = lv_message.

        IF lv_message IS NOT INITIAL.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '008' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error
                                v1 = lv_message ). "type of message

          <fs_TransGuides>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_TransGuides>-%tky ) TO failed-transferguides.

          APPEND VALUE #(  %tky      = <fs_TransGuides>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-transferguides.

          CONTINUE.

        ENDIF.

        <fs_TransGuides>-Idnumber      = ls_guia-id_destinatario.
        <fs_TransGuides>-Establishment = ls_inf_tribu-estab.
        <fs_TransGuides>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_TransGuides>-Sequential    = ls_inf_tribu-secuencial.
        <fs_TransGuides>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_TransGuides>-issuedate     = ls_guia-fecha.

        CLEAR: lv_xml, lv_clave.
        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->guiaremision
          EXPORTING
            header    = ls_guia
            inf_tribu = ls_inf_tribu
            detalle   = lt_detalle
            det_add   = lt_det_add
            head_add  = lt_head_add
          IMPORTING
            xml       = lv_xml.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        CALL METHOD cl_web_http_utility=>encode_x_base64
          EXPORTING
            unencoded = lv_raw
          RECEIVING
            encoded   = lv_base64.

        <fs_TransGuides>-xml  = lv_raw.
        <fs_TransGuides>-Mimetype  = 'text/xml'.
        <fs_TransGuides>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave  = |{ <fs_TransGuides>-Accesskey }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
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

        IF lv_update IS INITIAL.

          lt_cre_guia = VALUE #( ( CompanyCode            = <fs_TransGuides>-CompanyCode
                                   MaterialDocumentYear   = <fs_TransGuides>-MaterialDocumentYear
                                   MaterialDocument       = <fs_TransGuides>-MaterialDocument
                                   GoodsMovementType      = <fs_TransGuides>-GoodsMovementType
                                   IdNumber               = <fs_TransGuides>-IdNumber
                                   Establishment          = <fs_TransGuides>-Establishment
                                   EmissionPoint          = <fs_TransGuides>-EmissionPoint
                                   Sequential             = <fs_TransGuides>-Sequential
                                   Accesskey              = <fs_TransGuides>-Accesskey
                                   DocumentType           = <fs_TransGuides>-DocumentType
                                   IssueDate              = <fs_TransGuides>-IssueDate
                                   DocumentStatus         = <fs_TransGuides>-DocumentStatus
                                   MessageDocument        = <fs_TransGuides>-MessageDocument
                                   AuthorizationDate      = <fs_TransGuides>-AuthorizationDate
                                   Xml                    = <fs_TransGuides>-Xml
                                   MimeType               = <fs_TransGuides>-MimeType
                                   FileName               = <fs_TransGuides>-FileName
                                   DocumentSupplier       = <fs_TransGuides>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      MaterialDocumentYear   = if_abap_behv=>mk-on
                                      MaterialDocument       = if_abap_behv=>mk-on
                                      GoodsMovementType      = if_abap_behv=>mk-on
                                      IdNumber               = if_abap_behv=>mk-on
                                      Establishment          = if_abap_behv=>mk-on
                                      EmissionPoint          = if_abap_behv=>mk-on
                                      Sequential             = if_abap_behv=>mk-on
                                      Accesskey              = if_abap_behv=>mk-on
                                      DocumentType           = if_abap_behv=>mk-on
                                      IssueDate              = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on
                                      Xml                    = if_abap_behv=>mk-on
                                      MimeType               = if_abap_behv=>mk-on
                                      FileName               = if_abap_behv=>mk-on
                                      DocumentSupplier       = if_abap_behv=>mk-on ) ) ).

        ELSE.

          lt_upd_guia = VALUE #( ( CompanyCode            = <fs_TransGuides>-CompanyCode
                                   MaterialDocumentYear   = <fs_TransGuides>-MaterialDocumentYear
                                   MaterialDocument       = <fs_TransGuides>-MaterialDocument
                                   GoodsMovementType      = <fs_TransGuides>-GoodsMovementType
                                   IdNumber               = <fs_TransGuides>-IdNumber
                                   Establishment          = <fs_TransGuides>-Establishment
                                   EmissionPoint          = <fs_TransGuides>-EmissionPoint
                                   Sequential             = <fs_TransGuides>-Sequential
                                   Accesskey              = <fs_TransGuides>-Accesskey
                                   DocumentType           = <fs_TransGuides>-DocumentType
                                   IssueDate              = <fs_TransGuides>-IssueDate
                                   DocumentStatus         = <fs_TransGuides>-DocumentStatus
                                   MessageDocument        = <fs_TransGuides>-MessageDocument
                                   AuthorizationDate      = <fs_TransGuides>-AuthorizationDate
                                   Xml                    = <fs_TransGuides>-Xml
                                   MimeType               = <fs_TransGuides>-MimeType
                                   FileName               = <fs_TransGuides>-FileName
                                   DocumentSupplier       = <fs_TransGuides>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      MaterialDocumentYear   = if_abap_behv=>mk-on
                                      MaterialDocument       = if_abap_behv=>mk-on
                                      GoodsMovementType      = if_abap_behv=>mk-on
                                      IdNumber               = if_abap_behv=>mk-on
                                      Establishment          = if_abap_behv=>mk-on
                                      EmissionPoint          = if_abap_behv=>mk-on
                                      Sequential             = if_abap_behv=>mk-on
                                      Accesskey              = if_abap_behv=>mk-on
                                      DocumentType           = if_abap_behv=>mk-on
                                      IssueDate              = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on
                                      Xml                    = if_abap_behv=>mk-on
                                      MimeType               = if_abap_behv=>mk-on
                                      FileName               = if_abap_behv=>mk-on
                                      DocumentSupplier       = if_abap_behv=>mk-on ) ) ).

        ENDIF.

        INSERT VALUE #( %msg = new_message_with_text(
                    text = |{ <fs_TransGuides>-MaterialDocument } { <fs_TransGuides>-Documentstatus } { <fs_TransGuides>-Messagedocument } |
                severity = if_abap_behv_message=>severity-success )
          ) INTO TABLE reported-transferguides.

        FREE: lo_emision, lo_xml, lo_guaitras.

      ENDLOOP.

      IF lt_cre_guia[] IS NOT INITIAL.

        MODIFY ENTITIES OF zcds_rv_doc_mm_guia IN LOCAL MODE
          ENTITY TransferGuides
          CREATE FROM lt_cre_guia
          REPORTED DATA(lt_reported)
          FAILED DATA(lt_failed)
          MAPPED DATA(lt_mapped).

      ENDIF.

      IF lt_upd_guia[] IS NOT INITIAL.

        MODIFY ENTITIES OF zcds_rv_doc_mm_guia IN LOCAL MODE
          ENTITY TransferGuides
          UPDATE FROM lt_upd_guia
          REPORTED lt_reported
          FAILED lt_failed
          MAPPED lt_mapped.

      ENDIF.

    ENDIF.

    result = VALUE #( FOR TransGuide IN TransGuides
                    ( %tky   = TransGuide-%tky
                      %param = TransGuide ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_clave   TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_guia TYPE zdt_mm_doc_guia.

    DATA: lt_upd_guia  TYPE TABLE FOR UPDATE zcds_rv_doc_mm_guia.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_mm_guia IN LOCAL MODE
      ENTITY TransferGuides
      FIELDS ( Companycode MaterialDocumentyear Materialdocument goodsmovementtype
               Idnumber Establishment Emissionpoint Sequential
               Accesskey Documenttype Issuedate Documentstatus Messagedocument
               Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(TransGuides).

    LOOP AT TransGuides ASSIGNING FIELD-SYMBOL(<fs_TransGuides>).

      CLEAR: lv_date, lv_clave.

      lv_id    = <fs_TransGuides>-Accesskey.
      lv_clave = <fs_TransGuides>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_TransGuides>-Documenttype
          companycode      = <fs_TransGuides>-Companycode
          xml              = lv_xml
          establishment    = <fs_TransGuides>-Establishment
          emissionpoint    = <fs_TransGuides>-Emissionpoint
          sequential       = <fs_TransGuides>-Sequential.

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

      lt_upd_guia = VALUE #( ( CompanyCode            = <fs_TransGuides>-CompanyCode
                               MaterialDocumentYear   = <fs_TransGuides>-MaterialDocumentYear
                               MaterialDocument       = <fs_TransGuides>-MaterialDocument
                               GoodsMovementType      = <fs_TransGuides>-GoodsMovementType
                               DocumentStatus         = <fs_TransGuides>-DocumentStatus
                               MessageDocument        = <fs_TransGuides>-MessageDocument
                               AuthorizationDate      = <fs_TransGuides>-AuthorizationDate
                               %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      MaterialDocumentYear   = if_abap_behv=>mk-on
                                      MaterialDocument       = if_abap_behv=>mk-on
                                      GoodsMovementType      = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on ) ) ).

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_TransGuides>-MaterialDocument } { <fs_TransGuides>-Documentstatus } { <fs_TransGuides>-Messagedocument }{ <fs_TransGuides>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-transportdata.

      FREE: lo_emision.

    ENDLOOP.

    IF lt_upd_guia[] IS NOT INITIAL.
*        me->update( EXPORTING entities = lt_upd_fac ).
       MODIFY ENTITIES OF zcds_rv_doc_mm_guia IN LOCAL MODE
         ENTITY TransferGuides
         UPDATE FROM lt_upd_guia
         REPORTED DATA(lt_reported)
         FAILED DATA(lt_failed)
         MAPPED DATA(lt_mapped).

      ENDIF.

    result = VALUE #( FOR TransGuide IN TransGuides
                    ( %tky   = TransGuide-%tky
                      %param = TransGuide ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_mm_doc_guia,
           ls_cre_guia  TYPE STRUCTURE FOR CREATE zcds_rv_doc_mm_guia.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_mm_doc_guia FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_mm_doc_guia,
           lt_updates     TYPE STANDARD TABLE OF zdt_mm_doc_guia,
           lt_controls    TYPE STANDARD TABLE OF zdt_mm_doc_guia.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_mm_doc_guia
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode             = @lt_inserts-companycode
          AND MaterialDocumentYear    = @lt_inserts-MaterialDocumentYear
          AND MaterialDocument        = @lt_inserts-MaterialDocument
          AND GoodsMovementType       = @lt_inserts-GoodsMovementType
        INTO TABLE @DATA(lt_docments).

    IF sy-subrc EQ 0.

      lt_updates = VALUE #( FOR i = 1 WHILE i LE lines( lt_inserts )
        LET
          ls_control  = VALUE #( lt_controls[ i ] OPTIONAL )
          ls_insert   = VALUE #( lt_inserts[ i ] OPTIONAL )
          ls_docment  = VALUE #( lt_docments[ MaterialDocument = ls_insert-MaterialDocument ] OPTIONAL )
          IN
            ( companycode            = ls_insert-companycode
              MaterialDocumentYear   = ls_insert-MaterialDocumentYear
              MaterialDocument       = ls_insert-MaterialDocument
              GoodsMovementType      = ls_insert-GoodsMovementType

              idnumber               = COND #( WHEN ls_insert-idnumber IS NOT INITIAL
                                               THEN ls_insert-idnumber
                                               ELSE ls_docment-idnumber )

              establishment          = COND #( WHEN ls_insert-establishment IS NOT INITIAL
                                               THEN ls_insert-establishment
                                               ELSE ls_docment-establishment )

              emissionpoint          = COND #( WHEN ls_insert-emissionpoint IS NOT INITIAL
                                               THEN ls_insert-emissionpoint
                                               ELSE ls_docment-emissionpoint )

              sequential             = COND #( WHEN ls_insert-sequential IS NOT INITIAL
                                               THEN ls_insert-sequential
                                               ELSE ls_docment-sequential )

              accesskey              = COND #( WHEN ls_insert-accesskey IS NOT INITIAL
                                               THEN ls_insert-accesskey
                                               ELSE ls_docment-accesskey )

              documenttype           = COND #( WHEN ls_insert-documenttype IS NOT INITIAL
                                               THEN ls_insert-documenttype
                                               ELSE ls_docment-documenttype )

              issuedate              = COND #( WHEN ls_insert-issuedate IS NOT INITIAL
                                               THEN ls_insert-issuedate
                                               ELSE ls_docment-issuedate )

              documentstatus         = COND #( WHEN ls_insert-documentstatus IS NOT INITIAL
                                               THEN ls_insert-documentstatus
                                               ELSE ls_docment-documentstatus )

              messagedocument        = COND #( WHEN ls_insert-messagedocument IS NOT INITIAL
                                               THEN ls_insert-messagedocument
                                               ELSE ls_docment-messagedocument )

              authorizationdate      = COND #( WHEN ls_insert-authorizationdate IS NOT INITIAL
                                               THEN ls_insert-authorizationdate
                                               ELSE ls_docment-authorizationdate )

              xml                    = COND #( WHEN ls_insert-xml IS NOT INITIAL
                                               THEN ls_insert-xml
                                               ELSE ls_docment-xml )

              mimetype               = COND #( WHEN ls_insert-mimetype IS NOT INITIAL
                                               THEN ls_insert-mimetype
                                               ELSE ls_docment-mimetype )

              filename               = COND #( WHEN ls_insert-filename IS NOT INITIAL
                                               THEN ls_insert-filename
                                               ELSE ls_docment-filename )

              documentsupplier       = COND #( WHEN ls_insert-documentsupplier IS NOT INITIAL
                                               THEN ls_insert-documentsupplier
                                               ELSE ls_docment-documentsupplier ) )
            ).

    ELSE.
      lt_updates = lt_inserts.
    ENDIF.

    UPDATE zdt_mm_doc_guia FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.

    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_mm_doc_guia WHERE companycode            EQ @<fs_keys>-companycode
                                     AND MaterialDocumentYear    EQ @<fs_keys>-MaterialDocumentYear
                                     AND MaterialDocument        EQ @<fs_keys>-MaterialDocument
                                     AND GoodsMovementType       EQ @<fs_keys>-GoodsMovementType.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_doc_mm_guia
        FOR ALL ENTRIES IN @keys
        WHERE CompanyCode            = @keys-CompanyCode
          AND MaterialDocumentYear   = @keys-MaterialDocumentYear
          AND MaterialDocument       = @keys-MaterialDocument
          AND GoodsMovementType      = @keys-GoodsMovementType
        INTO CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

  METHOD lock.

  ENDMETHOD.

  METHOD rba_Transportdata.

    SELECT * FROM zcds_rv_ec_011
        FOR ALL ENTRIES IN @keys_rba
        WHERE CompanyCode            = @keys_rba-CompanyCode
          AND MaterialDocumentYear   = @keys_rba-MaterialDocumentYear
          AND MaterialDocument       = @keys_rba-MaterialDocument
          AND GoodsMovementType      = @keys_rba-GoodsMovementType
        INTO CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

  METHOD cba_Transportdata.

    DATA : lt_inserts        TYPE STANDARD TABLE OF zdt_ec_011,
           ls_inserts        TYPE zdt_ec_011,
           lt_cre_transport  TYPE TABLE FOR UPDATE zcds_rv_ec_011,
           ls_cre_transport  TYPE STRUCTURE FOR UPDATE zcds_rv_ec_011.

    IF entities_cba IS NOT INITIAL.

      LOOP AT entities_cba INTO DATA(ls_entities).

        LOOP AT ls_entities-%target ASSIGNING FIELD-SYMBOL(<fs_TransporData>).
          MOVE-CORRESPONDING: <fs_TransporData> TO ls_inserts.
          APPEND ls_inserts TO lt_inserts.
        ENDLOOP.

      ENDLOOP.

      IF lt_inserts IS NOT INITIAL.
        MODIFY zdt_ec_011 FROM TABLE @lt_inserts.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
