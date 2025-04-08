CLASS lsc_zcds_rv_doc_ret DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_ret IMPLEMENTATION.

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

CLASS lhc_Withholdings DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Withholdings RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION Withholdings~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION Withholdings~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Withholdings RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Withholdings RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Withholdings.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Withholdings.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Withholdings.

    METHODS read FOR READ
      IMPORTING keys FOR READ Withholdings RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Withholdings.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_Withholdings IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_ret  IN LOCAL MODE
      ENTITY Withholdings
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Documentstatus ReverseDocument )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Withholdings).

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
          lv_clave   TYPE string,
          lv_id      TYPE sgtxt,
          lv_user    TYPE sy-uname,
          lv_message TYPE string,
          lv_date    TYPE datum,
          lv_update  TYPE c.

    DATA: lt_ec_008   TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008   TYPE zdt_ec_008,
          lt_ec_002   TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002   TYPE zdt_ec_002,
          lt_ec_001   TYPE STANDARD TABLE OF zdt_ec_001,
          ls_ec_001   TYPE zdt_ec_001,
          lt_ec_012   TYPE STANDARD TABLE OF zdt_ec_012,
          ls_ec_012   TYPE zdt_ec_012,
          lt_cre_ret   TYPE TABLE FOR CREATE zcds_rv_doc_ret,
          lt_upd_ret   TYPE TABLE FOR UPDATE zcds_rv_doc_ret,
          lt_entry    TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Change,
          ls_entry    LIKE LINE OF lt_entry,
          ls_aparitem LIKE LINE OF ls_entry-%param-_aparitems,
          ls_glitems  LIKE LINE OF ls_entry-%param-_glitems,
          ls_ec_ret   TYPE zdt_fi_doc_ret.

    DATA: lo_Withholdings TYPE REF TO zcl_create_retencion,
          lo_xml          TYPE REF TO zcl_create_xml_emi,
          lo_emision      TYPE REF TO zcl_hs_emision_doc.

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

        CLEAR: lv_update.

        if <fs_Withholdings>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_Withholdings
          EXPORTING
            companycode            = <fs_Withholdings>-Companycode
            fiscalyear             = <fs_Withholdings>-Fiscalyear
            accountingdocument     = <fs_Withholdings>-Accountingdocument
            accountingdocumenttype = <fs_Withholdings>-Accountingdocumenttype.

        CLEAR: ls_inf_tribu, ls_retencion, lt_impuesto_s, lt_pagos, lt_reembolso, lt_reem_imp, lt_head_add, lv_mensaje.

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
            t_head_add   = lt_head_add
            message      = lv_mensaje.

        IF lv_message IS NOT INITIAL.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '008' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error
                                v1 = lv_message ). "type of message

          <fs_Withholdings>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_Withholdings>-%tky ) TO failed-withholdings.

          APPEND VALUE #(  %tky      = <fs_Withholdings>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-withholdings.

          CONTINUE.

        ENDIF.

        <fs_Withholdings>-Idnumber      = ls_retencion-identificacionsujetoretenido.
        <fs_Withholdings>-Typeid        = ls_retencion-tipoidentificacionsujetoreteni.
        <fs_Withholdings>-Businessname  = ls_retencion-razonsocialsujetoretenido.
        <fs_Withholdings>-Establishment = ls_inf_tribu-estab.
        <fs_Withholdings>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_Withholdings>-Sequential    = ls_inf_tribu-secuencial.
        <fs_Withholdings>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_Withholdings>-issuedate     = <fs_Withholdings>-PostingDate.

        ls_entry-%param-DocumentHeaderText          = |{ <fs_Withholdings>-Establishment }{ <fs_Withholdings>-Emissionpoint }{ <fs_Withholdings>-Sequential } |.
        ls_entry-%param-%control-DocumentHeaderText = if_abap_behv=>mk-on.

*        ls_aparitem-GLAccountLineItem          = '000001'.
*        ls_aparitem-DocumentItemText           = ls_inf_tribu-claveacceso.
*        ls_aparitem-%control-GLAccountLineItem = if_abap_behv=>mk-on.
*        ls_aparitem-%control-documentitemtext  = if_abap_behv=>mk-on.
*        ls_entry-%param-%control-_aparitems    = if_abap_behv=>mk-on.
*        APPEND ls_aparitem TO ls_entry-%param-_aparitems.
*
*        ls_glitems-GLAccountLineItem          = '000001'.
*        ls_glitems-documentitemtext           = ls_inf_tribu-claveacceso.
*        ls_glitems-%control-GLAccountLineItem = if_abap_behv=>mk-on.
*        ls_glitems-%control-documentitemtext  = if_abap_behv=>mk-on.
*        ls_entry-%param-%control-_glitems     = if_abap_behv=>mk-on.
*        APPEND ls_glitems TO ls_entry-%param-_glitems.

        ls_entry-%key-AccountingDocument   = ls_entry-AccountingDocument = <fs_Withholdings>-AccountingDocument.
        ls_entry-%key-CompanyCode          = ls_entry-CompanyCode = <fs_Withholdings>-CompanyCode.
        ls_entry-%key-FiscalYear           = ls_entry-FiscalYear = <fs_Withholdings>-Fiscalyear.

        APPEND ls_entry TO lt_entry.

        CLEAR: lv_xml, lv_clave, ls_entry, ls_aparitem.

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

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        CALL METHOD cl_web_http_utility=>encode_x_base64
          EXPORTING
            unencoded = lv_raw
          RECEIVING
            encoded   = lv_base64.

        <fs_Withholdings>-xml  = lv_raw.

        <fs_Withholdings>-Mimetype  = 'text/xml'.
        <fs_Withholdings>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave = |{ ls_inf_tribu-claveacceso }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
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

        IF lv_update IS INITIAL.

          lt_cre_ret = VALUE #( (  CompanyCode            = <fs_Withholdings>-CompanyCode
                                   FiscalYear             = <fs_Withholdings>-FiscalYear
                                   AccountingDocument     = <fs_Withholdings>-AccountingDocument
                                   AccountingDocumentType = <fs_Withholdings>-AccountingDocumentType
                                   Supplier               = <fs_Withholdings>-Supplier
                                   BusinessName           = <fs_Withholdings>-BusinessName
                                   TypeId                 = <fs_Withholdings>-TypeId
                                   IdNumber               = <fs_Withholdings>-IdNumber
                                   Establishment          = <fs_Withholdings>-Establishment
                                   EmissionPoint          = <fs_Withholdings>-EmissionPoint
                                   Sequential             = <fs_Withholdings>-Sequential
                                   Accesskey              = <fs_Withholdings>-Accesskey
                                   DocumentType           = <fs_Withholdings>-DocumentType
                                   IssueDate              = <fs_Withholdings>-IssueDate
                                   DocumentStatus         = <fs_Withholdings>-DocumentStatus
                                   MessageDocument        = <fs_Withholdings>-MessageDocument
                                   AuthorizationDate      = <fs_Withholdings>-AuthorizationDate
                                   Xml                    = <fs_Withholdings>-Xml
                                   MimeType               = <fs_Withholdings>-MimeType
                                   FileName               = <fs_Withholdings>-FileName
                                   DocumentSupplier       = <fs_Withholdings>-DocumentSupplier
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

          lt_upd_ret = VALUE #( (  CompanyCode            = <fs_Withholdings>-CompanyCode
                                   FiscalYear             = <fs_Withholdings>-FiscalYear
                                   AccountingDocument     = <fs_Withholdings>-AccountingDocument
                                   AccountingDocumentType = <fs_Withholdings>-AccountingDocumentType
                                   Supplier               = <fs_Withholdings>-Supplier
                                   BusinessName           = <fs_Withholdings>-BusinessName
                                   TypeId                 = <fs_Withholdings>-TypeId
                                   IdNumber               = <fs_Withholdings>-IdNumber
                                   Establishment          = <fs_Withholdings>-Establishment
                                   EmissionPoint          = <fs_Withholdings>-EmissionPoint
                                   Sequential             = <fs_Withholdings>-Sequential
                                   Accesskey              = <fs_Withholdings>-Accesskey
                                   DocumentType           = <fs_Withholdings>-DocumentType
                                   IssueDate              = <fs_Withholdings>-IssueDate
                                   DocumentStatus         = <fs_Withholdings>-DocumentStatus
                                   MessageDocument        = <fs_Withholdings>-MessageDocument
                                   AuthorizationDate      = <fs_Withholdings>-AuthorizationDate
                                   Xml                    = <fs_Withholdings>-Xml
                                   MimeType               = <fs_Withholdings>-MimeType
                                   FileName               = <fs_Withholdings>-FileName
                                   DocumentSupplier       = <fs_Withholdings>-DocumentSupplier
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
            %msg = new_message_with_text( text = |{ <fs_Withholdings>-Accountingdocument } { <fs_Withholdings>-Documentstatus } { <fs_Withholdings>-Messagedocument } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-withholdings.

        FREE: lo_emision, lo_xml, lo_Withholdings.

      ENDLOOP.


      IF lt_cre_ret[] IS NOT INITIAL.

       MODIFY ENTITIES OF zcds_rv_doc_ret IN LOCAL MODE
         ENTITY Withholdings
         CREATE FROM lt_cre_ret
         REPORTED DATA(lt_reported)
         FAILED DATA(lt_failed)
         MAPPED DATA(lt_mapped).

      ENDIF.

      IF lt_upd_ret[] IS NOT INITIAL.

       MODIFY ENTITIES OF zcds_rv_doc_ret IN LOCAL MODE
         ENTITY Withholdings
         UPDATE FROM lt_upd_ret
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

    result = VALUE #( FOR Withholding IN Withholdings
                    ( %tky        = Withholding-%tky
                      %param      = Withholding ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_clave   TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ret  TYPE zdt_fi_doc_ret.

    DATA: lt_upd_ret  TYPE TABLE FOR UPDATE zcds_rv_doc_ret.

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

      CLEAR: lv_date, lv_clave.

      lv_clave = lv_id = <fs_Withholdings>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_Withholdings>-Documenttype
          companycode      = <fs_Withholdings>-Companycode
          xml              = lv_xml
          establishment    = <fs_Withholdings>-Establishment
          emissionpoint    = <fs_Withholdings>-Emissionpoint
          sequential       = <fs_Withholdings>-Sequential.

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

      lt_upd_ret = VALUE #( (  CompanyCode            = <fs_Withholdings>-CompanyCode
                               FiscalYear             = <fs_Withholdings>-FiscalYear
                               AccountingDocument     = <fs_Withholdings>-AccountingDocument
                               AccountingDocumentType = <fs_Withholdings>-AccountingDocumentType
                               DocumentStatus         = <fs_Withholdings>-DocumentStatus
                               MessageDocument        = <fs_Withholdings>-MessageDocument
                               AuthorizationDate      = <fs_Withholdings>-AuthorizationDate
                               %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on ) ) ).

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_Withholdings>-Accountingdocument } { <fs_Withholdings>-Documentstatus } { <fs_Withholdings>-Messagedocument }{ <fs_Withholdings>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-withholdings.

      FREE: lo_emision.

    ENDLOOP.

    IF lt_upd_ret IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_ret IN LOCAL MODE
        ENTITY Withholdings
        UPDATE FROM lt_upd_ret
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.

    result = VALUE #( FOR Withholding IN Withholdings
                    ( %tky = Withholding-%tky
                      %param = Withholding ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_fi_doc_ret,
           ls_cre_ret   TYPE STRUCTURE FOR CREATE zcds_rv_doc_ret.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_fi_doc_ret FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_fi_doc_ret,
           lt_updates     TYPE STANDARD TABLE OF zdt_fi_doc_ret,
           lt_controls    TYPE STANDARD TABLE OF zdt_fi_doc_ret.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_fi_doc_ret
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

    UPDATE zdt_fi_doc_ret FROM TABLE @lt_updates.

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

    SELECT * FROM zcds_rv_doc_ret
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
