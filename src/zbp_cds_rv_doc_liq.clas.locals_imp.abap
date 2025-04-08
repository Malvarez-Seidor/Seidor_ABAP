CLASS lsc_zcds_rv_doc_liq DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_liq IMPLEMENTATION.

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

CLASS lhc_LiquidationPurchase DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR LiquidationPurchase RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION LiquidationPurchase~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION LiquidationPurchase~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR LiquidationPurchase RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR LiquidationPurchase RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE LiquidationPurchase.

    METHODS update FOR MODIFY
      IMPORTING entities  FOR UPDATE LiquidationPurchase.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE LiquidationPurchase.

    METHODS read FOR READ
      IMPORTING keys FOR READ LiquidationPurchase RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK LiquidationPurchase.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

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
          lv_clave   TYPE string,
          lv_id      TYPE sgtxt,
          lv_user    TYPE sy-uname,
          lv_message TYPE string,
          lv_date    TYPE datum,
          lv_update  TYPE c.

    DATA: lt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008    TYPE zdt_ec_008,
          lt_ec_002    TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002    TYPE zdt_ec_002,
          lt_ec_001    TYPE STANDARD TABLE OF zdt_ec_001,
          ls_ec_001    TYPE zdt_ec_001,
          lt_ec_012    TYPE STANDARD TABLE OF zdt_ec_012,
          ls_ec_012    TYPE zdt_ec_012,
          lt_entry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Change,
          ls_entry     LIKE LINE OF lt_entry,
          ls_aparitem  LIKE LINE OF ls_entry-%param-_aparitems,
          lt_cre_liq   TYPE TABLE FOR CREATE zcds_rv_doc_liq,
          lt_upd_liq   TYPE TABLE FOR UPDATE zcds_rv_doc_liq,
          ls_ec_liq    TYPE zdt_fi_doc_liq.

    DATA: lo_liquidacion TYPE REF TO zcl_create_liquidacion,
          lo_xml         TYPE REF TO zcl_create_xml_emi,
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
    WHERE companycode NE @space
    INTO TABLE @lt_ec_001.

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    WHERE companycode NE @space
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE users EQ @sy-uname
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

        CLEAR: lv_update.

        if <fs_LiqPurchases>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_liquidacion
          EXPORTING
            companycode            = <fs_LiqPurchases>-Companycode
            fiscalyear             = <fs_LiqPurchases>-Fiscalyear
            accountingdocument     = <fs_LiqPurchases>-Accountingdocument
            accountingdocumenttype = <fs_LiqPurchases>-Accountingdocumenttype.

        CLEAR: ls_inf_tribu, ls_liquida, lt_impuesto, lt_pagos, lt_detalle, lt_det_add,
               lt_det_imp, lt_reembolso, lt_reem_imp, lt_head_add, lv_message.

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
            t_head_add   = lt_head_add
            message      = lv_message.

        IF lv_message IS NOT INITIAL.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '008' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error
                                v1 = lv_message ). "type of message

          <fs_LiqPurchases>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_LiqPurchases>-%tky ) TO failed-liquidationpurchase.

          APPEND VALUE #(  %tky      = <fs_LiqPurchases>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-liquidationpurchase.

          CONTINUE.

        ENDIF.

        <fs_LiqPurchases>-Idnumber      = ls_liquida-identificacionproveedor.
        <fs_LiqPurchases>-Typeid        = ls_liquida-tipoidentificacionproveedor.
        <fs_LiqPurchases>-Businessname  = ls_liquida-razonsocialproveedor.
        <fs_LiqPurchases>-Establishment = ls_inf_tribu-estab.
        <fs_LiqPurchases>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_LiqPurchases>-Sequential    = ls_inf_tribu-secuencial.
        <fs_LiqPurchases>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_LiqPurchases>-issuedate     = <fs_LiqPurchases>-PostingDate.

        ls_entry-%param-DocumentReferenceID          = |{ <fs_LiqPurchases>-Establishment }{ <fs_LiqPurchases>-Emissionpoint }{ <fs_LiqPurchases>-Sequential } |.
        ls_entry-%param-%control-DocumentReferenceID = if_abap_behv=>mk-on.

        ls_aparitem-GLAccountLineItem          = '000001'.
        ls_aparitem-documentitemtext           = ls_inf_tribu-claveacceso.
        ls_aparitem-%control-GLAccountLineItem = if_abap_behv=>mk-on.
        ls_aparitem-%control-documentitemtext  = if_abap_behv=>mk-on.
        ls_entry-%param-%control-_aparitems    = if_abap_behv=>mk-on.
        APPEND ls_aparitem TO ls_entry-%param-_aparitems.

        ls_entry-%key-AccountingDocument   = ls_entry-AccountingDocument = <fs_LiqPurchases>-AccountingDocument.
        ls_entry-%key-CompanyCode          = ls_entry-CompanyCode = <fs_LiqPurchases>-CompanyCode.
        ls_entry-%key-FiscalYear           = ls_entry-FiscalYear = <fs_LiqPurchases>-Fiscalyear.

        APPEND ls_entry TO lt_entry.

        CLEAR: lv_xml, lv_clave, ls_entry, ls_aparitem.

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

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        CALL METHOD cl_web_http_utility=>encode_x_base64
          EXPORTING
            unencoded = lv_raw
          RECEIVING
            encoded   = lv_base64.

        <fs_LiqPurchases>-xml  = lv_raw.

        <fs_LiqPurchases>-Mimetype  = 'text/xml'.
        <fs_LiqPurchases>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave = |{ <fs_LiqPurchases>-Accesskey }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
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

        IF lv_update IS INITIAL.

          lt_cre_liq = VALUE #( (  CompanyCode            = <fs_LiqPurchases>-CompanyCode
                                   FiscalYear             = <fs_LiqPurchases>-FiscalYear
                                   AccountingDocument     = <fs_LiqPurchases>-AccountingDocument
                                   AccountingDocumentType = <fs_LiqPurchases>-AccountingDocumentType
                                   Supplier               = <fs_LiqPurchases>-Supplier
                                   BusinessName           = <fs_LiqPurchases>-BusinessName
                                   TypeId                 = <fs_LiqPurchases>-TypeId
                                   IdNumber               = <fs_LiqPurchases>-IdNumber
                                   Establishment          = <fs_LiqPurchases>-Establishment
                                   EmissionPoint          = <fs_LiqPurchases>-EmissionPoint
                                   Sequential             = <fs_LiqPurchases>-Sequential
                                   Accesskey              = <fs_LiqPurchases>-Accesskey
                                   DocumentType           = <fs_LiqPurchases>-DocumentType
                                   IssueDate              = <fs_LiqPurchases>-IssueDate
                                   DocumentStatus         = <fs_LiqPurchases>-DocumentStatus
                                   MessageDocument        = <fs_LiqPurchases>-MessageDocument
                                   AuthorizationDate      = <fs_LiqPurchases>-AuthorizationDate
                                   Xml                    = <fs_LiqPurchases>-Xml
                                   MimeType               = <fs_LiqPurchases>-MimeType
                                   FileName               = <fs_LiqPurchases>-FileName
                                   DocumentSupplier       = <fs_LiqPurchases>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      Supplier               = if_abap_behv=>mk-on
                                      BusinessName           = if_abap_behv=>mk-on
                                      TypeId                 = if_abap_behv=>mk-on
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

          lt_upd_liq = VALUE #( (  CompanyCode            = <fs_LiqPurchases>-CompanyCode
                                   FiscalYear             = <fs_LiqPurchases>-FiscalYear
                                   AccountingDocument     = <fs_LiqPurchases>-AccountingDocument
                                   AccountingDocumentType = <fs_LiqPurchases>-AccountingDocumentType
                                   Supplier               = <fs_LiqPurchases>-Supplier
                                   BusinessName           = <fs_LiqPurchases>-BusinessName
                                   TypeId                 = <fs_LiqPurchases>-TypeId
                                   IdNumber               = <fs_LiqPurchases>-IdNumber
                                   Establishment          = <fs_LiqPurchases>-Establishment
                                   EmissionPoint          = <fs_LiqPurchases>-EmissionPoint
                                   Sequential             = <fs_LiqPurchases>-Sequential
                                   Accesskey              = <fs_LiqPurchases>-Accesskey
                                   DocumentType           = <fs_LiqPurchases>-DocumentType
                                   IssueDate              = <fs_LiqPurchases>-IssueDate
                                   DocumentStatus         = <fs_LiqPurchases>-DocumentStatus
                                   MessageDocument        = <fs_LiqPurchases>-MessageDocument
                                   AuthorizationDate      = <fs_LiqPurchases>-AuthorizationDate
                                   Xml                    = <fs_LiqPurchases>-Xml
                                   MimeType               = <fs_LiqPurchases>-MimeType
                                   FileName               = <fs_LiqPurchases>-FileName
                                   DocumentSupplier       = <fs_LiqPurchases>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      Supplier               = if_abap_behv=>mk-on
                                      BusinessName           = if_abap_behv=>mk-on
                                      TypeId                 = if_abap_behv=>mk-on
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

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_LiqPurchases>-Accountingdocument } { <fs_LiqPurchases>-Documentstatus } { <fs_LiqPurchases>-Messagedocument } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-liquidationpurchase.

        FREE: lo_emision, lo_xml, lo_liquidacion.

      ENDLOOP.

      IF lt_cre_liq[] IS NOT INITIAL.

       MODIFY ENTITIES OF zcds_rv_doc_liq IN LOCAL MODE
         ENTITY LiquidationPurchase
         CREATE FROM lt_cre_liq
         REPORTED DATA(lt_reported)
         FAILED DATA(lt_failed)
         MAPPED DATA(lt_mapped).

      ENDIF.

      IF lt_upd_liq[] IS NOT INITIAL.

       MODIFY ENTITIES OF zcds_rv_doc_liq IN LOCAL MODE
         ENTITY LiquidationPurchase
         UPDATE FROM lt_upd_liq
         REPORTED lt_reported
         FAILED lt_failed
         MAPPED lt_mapped.

      ENDIF.

    ENDIF.

    IF lt_entry IS NOT INITIAL.

      MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE Change FROM lt_entry
        MAPPED FINAL(ls_post_mapped)
        FAILED FINAL(ls_post_failed)
        REPORTED FINAL(ls_post_reported).

    ENDIF.

    result = VALUE #( FOR LiqPurchase IN LiqPurchases
                    ( %tky        = LiqPurchase-%tky
                      %param      = LiqPurchase ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_clave   TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_liq  TYPE zdt_fi_doc_liq.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    DATA: lt_upd_liq  TYPE TABLE FOR UPDATE zcds_rv_doc_liq.

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

      CLEAR: lv_date, lv_clave.

      lv_id    = <fs_LiqPurchase>-Accesskey.
      lv_clave = <fs_LiqPurchase>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_LiqPurchase>-Documenttype
          companycode      = <fs_LiqPurchase>-Companycode
          xml              = lv_xml
          establishment    = <fs_LiqPurchase>-Establishment
          emissionpoint    = <fs_LiqPurchase>-Emissionpoint
          sequential       = <fs_LiqPurchase>-Sequential.

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

      lt_upd_liq = VALUE #( (  CompanyCode            = <fs_LiqPurchase>-CompanyCode
                               FiscalYear             = <fs_LiqPurchase>-FiscalYear
                               AccountingDocument     = <fs_LiqPurchase>-AccountingDocument
                               AccountingDocumentType = <fs_LiqPurchase>-AccountingDocumentType
                               DocumentStatus         = <fs_LiqPurchase>-DocumentStatus
                               MessageDocument        = <fs_LiqPurchase>-MessageDocument
                               AuthorizationDate      = <fs_LiqPurchase>-AuthorizationDate
                               %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on ) ) ).

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_LiqPurchase>-Accountingdocument } { <fs_LiqPurchase>-Documentstatus } { <fs_LiqPurchase>-Messagedocument }{ <fs_LiqPurchase>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-liquidationpurchase.

      FREE: lo_emision.

    ENDLOOP.

    IF lt_upd_liq IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_liq IN LOCAL MODE
        ENTITY LiquidationPurchase
        UPDATE FROM lt_upd_liq
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.

    result = VALUE #( FOR LiqPurchase IN LiqPurchases
                    ( %tky = LiqPurchase-%tky
                      %param = LiqPurchase ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_fi_doc_liq,
           ls_cre_liq   TYPE STRUCTURE FOR CREATE zcds_rv_doc_liq.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_fi_doc_liq FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_fi_doc_liq,
           lt_updates     TYPE STANDARD TABLE OF zdt_fi_doc_liq,
           lt_controls    TYPE STANDARD TABLE OF zdt_fi_doc_liq.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_fi_doc_liq
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode            = @lt_inserts-companycode
          AND fiscalyear             = @lt_inserts-fiscalyear
          AND accountingdocument     = @lt_inserts-accountingdocument
          AND accountingdocumenttype = @lt_inserts-accountingdocumenttype
        INTO TABLE @DATA(lt_docments).

    IF sy-subrc EQ 0.

      lt_updates = VALUE #( FOR i = 1 WHILE i LE lines( lt_inserts )
        LET
          ls_control  = VALUE #( lt_controls[ i ] OPTIONAL )
          ls_insert   = VALUE #( lt_inserts[ i ] OPTIONAL )
          ls_docment  = VALUE #( lt_docments[ accountingdocument = ls_insert-accountingdocument ] OPTIONAL )
          IN
            ( companycode            = ls_insert-companycode
              fiscalyear             = ls_insert-fiscalyear
              accountingdocument     = ls_insert-accountingdocument
              accountingdocumenttype = ls_insert-accountingdocumenttype

              supplier               = COND #( WHEN ls_insert-supplier IS NOT INITIAL
                                               THEN ls_insert-supplier
                                               ELSE ls_docment-supplier )

              businessname           = COND #( WHEN ls_insert-businessname IS NOT INITIAL
                                               THEN ls_insert-businessname
                                               ELSE ls_docment-businessname )

              typeid                 = COND #( WHEN ls_insert-typeid IS NOT INITIAL
                                              THEN ls_insert-typeid
                                              ELSE ls_docment-typeid )

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

    UPDATE zdt_fi_doc_liq FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.


    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_fi_doc_ret WHERE companycode            EQ @<fs_keys>-companycode
                                     AND fiscalyear             EQ @<fs_keys>-fiscalyear
                                     AND accountingdocument     EQ @<fs_keys>-accountingdocument
                                     AND accountingdocumenttype EQ @<fs_keys>-AccountingDocumentType.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_doc_liq
        FOR ALL ENTRIES IN @keys
        WHERE CompanyCode            = @keys-CompanyCode
          AND FiscalYear             = @keys-FiscalYear
          AND AccountingDocument     = @keys-AccountingDocument
          AND AccountingDocumentType = @keys-AccountingDocumentType
        into CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

  METHOD lock.

  ENDMETHOD.

ENDCLASS.
