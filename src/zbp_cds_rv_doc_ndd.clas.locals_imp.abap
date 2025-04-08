CLASS lsc_zcds_rv_doc_ndd DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_ndd IMPLEMENTATION.

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

CLASS lhc_DebitNotes DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR DebitNotes RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION DebitNotes~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION DebitNotes~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR DebitNotes RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR DebitNotes RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE DebitNotes.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE DebitNotes.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE DebitNotes.

    METHODS read FOR READ
      IMPORTING keys FOR READ DebitNotes RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK DebitNotes.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

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

      READ TABLE DebitNotes INTO DATA(ls_DebitNote) WITH KEY companycode   = <fs_result>-Companycode
                                                               fiscalyear  = <fs_result>-Fiscalyear
                                                       accountingdocument  = <fs_result>-Accountingdocument
                                                   accountingdocumenttype  = <fs_result>-Accountingdocumenttype
                                                       billingdocument     = <fs_result>-Billingdocument
                                                       billingdocumenttype = <fs_result>-Billingdocumenttype.

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
          lv_clave   TYPE string,
          lv_id      TYPE sgtxt,
          lv_date    TYPE datum,
          lv_message TYPE string,
          lv_update  TYPE c.

    DATA: ls_ec_ndd  TYPE zdt_sd_doc_ndd.

    DATA: lt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008    TYPE zdt_ec_008,
          lt_entry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Change,
          ls_entry     LIKE LINE OF lt_entry,
          ls_aparitem  LIKE LINE OF ls_entry-%param-_aparitems,
          lt_cre_ndd   TYPE TABLE FOR CREATE zcds_rv_doc_ndd,
          lt_upd_ndd   TYPE TABLE FOR UPDATE zcds_rv_doc_ndd,
          lt_ec_002    TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002    TYPE zdt_ec_002.

    DATA: lo_DebitNotes TYPE REF TO zcl_create_nota_debito,
          lo_xml        TYPE REF TO zcl_create_xml_emi,
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
    WHERE companycode NE @space
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE users EQ @sy-uname
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

        CLEAR: lv_update.

        if <fs_DebitNote>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_DebitNotes
          EXPORTING
            companycode            = <fs_DebitNote>-Companycode
            fiscalyear             = <fs_DebitNote>-Fiscalyear
            accountingdocument     = <fs_DebitNote>-Accountingdocument
            accountingdocumenttype = <fs_DebitNote>-Accountingdocumenttype
            billingdocument        = <fs_DebitNote>-Billingdocument
            billingdocumenttype    = <fs_DebitNote>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_notadebito, lt_impuesto, lt_pagos, lt_motivos,
               lt_head_add, lv_message.

        CALL METHOD lo_DebitNotes->callDocumentType
          EXPORTING
            documenttype = <fs_DebitNote>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            notadebito   = ls_notadebito
            t_impuesto   = lt_impuesto
            t_motivos    = lt_motivos
            t_pagos      = lt_pagos
            t_head_add   = lt_head_add
            message      = lv_message.

        IF lv_message IS NOT INITIAL.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '008' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error
                                v1 = lv_message ). "type of message

          <fs_DebitNote>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_DebitNote>-%tky ) TO failed-debitnotes.

          APPEND VALUE #(  %tky      = <fs_DebitNote>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-debitnotes.

          CONTINUE.

        ENDIF.

        <fs_DebitNote>-Idnumber      = ls_notadebito-idcomprador.
        <fs_DebitNote>-Typeid        = ls_notadebito-tipoidcomprador.
        <fs_DebitNote>-Businessname  = ls_notadebito-razonsocialcomprador.
        <fs_DebitNote>-Establishment = ls_inf_tribu-estab.
        <fs_DebitNote>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_DebitNote>-Sequential    = ls_inf_tribu-secuencial.
        <fs_DebitNote>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_DebitNote>-issuedate     = <fs_debitnote>-BillingDocumentDate.

        ls_entry-%param-DocumentReferenceID          = |{ <fs_debitnote>-Establishment }{ <fs_debitnote>-Emissionpoint }{ <fs_debitnote>-Sequential } |.
        ls_entry-%param-%control-DocumentReferenceID = if_abap_behv=>mk-on.

        ls_aparitem-GLAccountLineItem          = '000001'.
        ls_aparitem-documentitemtext           = ls_inf_tribu-claveacceso.
        ls_aparitem-%control-GLAccountLineItem = if_abap_behv=>mk-on.
        ls_aparitem-%control-documentitemtext  = if_abap_behv=>mk-on.
        ls_entry-%param-%control-_aparitems    = if_abap_behv=>mk-on.
        APPEND ls_aparitem TO ls_entry-%param-_aparitems.

        ls_entry-%key-AccountingDocument   = ls_entry-AccountingDocument = <fs_debitnote>-AccountingDocument.
        ls_entry-%key-CompanyCode          = ls_entry-CompanyCode = <fs_debitnote>-CompanyCode.
        ls_entry-%key-FiscalYear           = ls_entry-FiscalYear = <fs_debitnote>-Fiscalyear.

        APPEND ls_entry TO lt_entry.

        CLEAR: lv_xml, ls_entry, ls_aparitem, lv_clave.

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

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        CALL METHOD cl_web_http_utility=>encode_x_base64
          EXPORTING
            unencoded = lv_raw
          RECEIVING
            encoded   = lv_base64.

        <fs_DebitNote>-xml  = lv_raw.
        <fs_DebitNote>-Mimetype  = 'text/xml'.
        <fs_DebitNote>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave = |{ <fs_DebitNote>-Accesskey }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
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

        IF lv_update IS INITIAL.

          lt_cre_ndd = VALUE #( (  CompanyCode            = <fs_DebitNote>-CompanyCode
                                   FiscalYear             = <fs_DebitNote>-FiscalYear
                                   AccountingDocument     = <fs_DebitNote>-AccountingDocument
                                   AccountingDocumentType = <fs_DebitNote>-AccountingDocumentType
                                   BillingDocument        = <fs_DebitNote>-BillingDocument
                                   BillingDocumentType    = <fs_DebitNote>-BillingDocumentType
                                   SoldtoParty            = <fs_DebitNote>-SoldtoParty
                                   BusinessName           = <fs_DebitNote>-BusinessName
                                   TypeId                 = <fs_DebitNote>-TypeId
                                   IdNumber               = <fs_DebitNote>-IdNumber
                                   Establishment          = <fs_DebitNote>-Establishment
                                   EmissionPoint          = <fs_DebitNote>-EmissionPoint
                                   Sequential             = <fs_DebitNote>-Sequential
                                   Accesskey              = <fs_DebitNote>-Accesskey
                                   DocumentType           = <fs_DebitNote>-DocumentType
                                   IssueDate              = <fs_DebitNote>-IssueDate
                                   DocumentStatus         = <fs_DebitNote>-DocumentStatus
                                   MessageDocument        = <fs_DebitNote>-MessageDocument
                                   AuthorizationDate      = <fs_DebitNote>-AuthorizationDate
                                   Xml                    = <fs_DebitNote>-Xml
                                   MimeType               = <fs_DebitNote>-MimeType
                                   FileName               = <fs_DebitNote>-FileName
                                   DocumentSupplier       = <fs_DebitNote>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      BillingDocument        = if_abap_behv=>mk-on
                                      BillingDocumentType    = if_abap_behv=>mk-on
                                      SoldtoParty            = if_abap_behv=>mk-on
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

          lt_upd_ndd = VALUE #( (  CompanyCode           = <fs_DebitNote>-CompanyCode
                                   FiscalYear             = <fs_DebitNote>-FiscalYear
                                   AccountingDocument     = <fs_DebitNote>-AccountingDocument
                                   AccountingDocumentType = <fs_DebitNote>-AccountingDocumentType
                                   BillingDocument        = <fs_DebitNote>-BillingDocument
                                   BillingDocumentType    = <fs_DebitNote>-BillingDocumentType
                                   SoldtoParty            = <fs_DebitNote>-SoldtoParty
                                   BusinessName           = <fs_DebitNote>-BusinessName
                                   TypeId                 = <fs_DebitNote>-TypeId
                                   IdNumber               = <fs_DebitNote>-IdNumber
                                   Establishment          = <fs_DebitNote>-Establishment
                                   EmissionPoint          = <fs_DebitNote>-EmissionPoint
                                   Sequential             = <fs_DebitNote>-Sequential
                                   Accesskey              = <fs_DebitNote>-Accesskey
                                   DocumentType           = <fs_DebitNote>-DocumentType
                                   IssueDate              = <fs_DebitNote>-IssueDate
                                   DocumentStatus         = <fs_DebitNote>-DocumentStatus
                                   MessageDocument        = <fs_DebitNote>-MessageDocument
                                   AuthorizationDate      = <fs_DebitNote>-AuthorizationDate
                                   Xml                    = <fs_DebitNote>-Xml
                                   MimeType               = <fs_DebitNote>-MimeType
                                   FileName               = <fs_DebitNote>-FileName
                                   DocumentSupplier       = <fs_DebitNote>-DocumentSupplier
                                   %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      BillingDocument        = if_abap_behv=>mk-on
                                      BillingDocumentType    = if_abap_behv=>mk-on
                                      SoldtoParty            = if_abap_behv=>mk-on
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
            %msg = new_message_with_text( text = |{ <fs_DebitNote>-BillingDocument } { <fs_DebitNote>-Documentstatus } { <fs_DebitNote>-Messagedocument }{ <fs_DebitNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-debitnotes.

        FREE: lo_emision, lo_xml, lo_DebitNotes.

      ENDLOOP.

    ENDIF.

    IF lt_cre_ndd[] IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_ndd IN LOCAL MODE
        ENTITY DebitNotes
        CREATE FROM lt_cre_ndd
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

     ENDIF.

     IF lt_upd_ndd[] IS NOT INITIAL.

       MODIFY ENTITIES OF zcds_rv_doc_ndd IN LOCAL MODE
         ENTITY DebitNotes
         UPDATE FROM lt_upd_ndd
         REPORTED lt_reported
         FAILED lt_failed
         MAPPED lt_mapped.

     ENDIF.

    IF lt_entry IS NOT INITIAL.

      MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE Change FROM lt_entry
        MAPPED FINAL(ls_post_mapped)
        FAILED FINAL(ls_post_failed)
        REPORTED FINAL(ls_post_reported).

    ENDIF.

    result = VALUE #( FOR DebitNote IN DebitNotes
                    ( %tky = DebitNote-%tky
                      %param = DebitNote ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_clave   TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ndd  TYPE zdt_sd_doc_ndd.

    DATA: lt_upd_ndd  TYPE TABLE FOR UPDATE zcds_rv_doc_ndd.

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

      CLEAR: lv_date, lv_clave.

      lv_id    = <fs_DebitNote>-Accesskey.
      lv_clave = <fs_DebitNote>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_DebitNote>-Documenttype
          companycode      = <fs_DebitNote>-Companycode
          xml              = lv_xml
          establishment    = <fs_DebitNote>-Establishment
          emissionpoint    = <fs_DebitNote>-Emissionpoint
          sequential       = <fs_DebitNote>-Sequential.

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

      lt_upd_ndd = VALUE #( (  CompanyCode            = <fs_DebitNote>-CompanyCode
                               FiscalYear             = <fs_DebitNote>-FiscalYear
                               AccountingDocument     = <fs_DebitNote>-AccountingDocument
                               AccountingDocumentType = <fs_DebitNote>-AccountingDocumentType
                               BillingDocument        = <fs_DebitNote>-BillingDocument
                               BillingDocumentType    = <fs_DebitNote>-BillingDocumentType
                               DocumentStatus         = <fs_DebitNote>-DocumentStatus
                               MessageDocument        = <fs_DebitNote>-MessageDocument
                               AuthorizationDate      = <fs_DebitNote>-AuthorizationDate
                               %control = VALUE #(
                                      Companycode            = if_abap_behv=>mk-on
                                      Fiscalyear             = if_abap_behv=>mk-on
                                      Accountingdocument     = if_abap_behv=>mk-on
                                      AccountingDocumentType = if_abap_behv=>mk-on
                                      BillingDocument        = if_abap_behv=>mk-on
                                      BillingDocumentType    = if_abap_behv=>mk-on
                                      DocumentStatus         = if_abap_behv=>mk-on
                                      MessageDocument        = if_abap_behv=>mk-on
                                      AuthorizationDate      = if_abap_behv=>mk-on ) ) ).

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_DebitNote>-BillingDocument } { <fs_DebitNote>-Documentstatus } { <fs_DebitNote>-Messagedocument }{ <fs_DebitNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-debitnotes.

      FREE: lo_emision.

    ENDLOOP.


    IF lt_upd_ndd IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_ndd IN LOCAL MODE
        ENTITY DebitNotes
        UPDATE FROM lt_upd_ndd
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.


    result = VALUE #( FOR DebitNote IN DebitNotes
                    ( %tky = DebitNote-%tky
                      %param = DebitNote ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_sd_doc_ndd,
           ls_cre_ndd   TYPE STRUCTURE FOR CREATE zcds_rv_doc_ndd.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_sd_doc_ndd FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_sd_doc_ndd,
           lt_updates     TYPE STANDARD TABLE OF zdt_sd_doc_ndd,
           lt_controls    TYPE STANDARD TABLE OF zdt_sd_doc_ndd.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_sd_doc_ndd
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode            = @lt_inserts-companycode
          AND fiscalyear             = @lt_inserts-fiscalyear
          AND accountingdocument     = @lt_inserts-accountingdocument
          AND accountingdocumenttype = @lt_inserts-accountingdocumenttype
          AND billingdocument        = @lt_inserts-billingdocument
          AND billingdocumenttype    = @lt_inserts-billingdocumenttype
        INTO TABLE @DATA(lt_docments).

    IF sy-subrc EQ 0.

      lt_updates = VALUE #( FOR i = 1 WHILE i LE lines( lt_inserts )
        LET
          ls_control  = VALUE #( lt_controls[ i ] OPTIONAL )
          ls_insert   = VALUE #( lt_inserts[ i ] OPTIONAL )
          ls_docment  = VALUE #( lt_docments[ billingdocument = ls_insert-billingdocument ] OPTIONAL )
          IN
            ( companycode            = ls_insert-companycode
              fiscalyear             = ls_insert-fiscalyear
              accountingdocument     = ls_insert-accountingdocument
              accountingdocumenttype = ls_insert-accountingdocumenttype
              billingdocument        = ls_insert-billingdocument
              billingdocumenttype    = ls_insert-billingdocumenttype

              soldtoparty            = COND #( WHEN ls_insert-soldtoparty IS NOT INITIAL
                                               THEN ls_insert-soldtoparty
                                               ELSE ls_docment-soldtoparty )

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

    UPDATE zdt_sd_doc_ndd FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.


    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_sd_doc_ndd WHERE companycode            EQ @<fs_keys>-companycode
                                     AND fiscalyear             EQ @<fs_keys>-fiscalyear
                                     AND accountingdocument     EQ @<fs_keys>-accountingdocument
                                     AND accountingdocumenttype EQ @<fs_keys>-AccountingDocumentType
                                     AND billingdocument        EQ @<fs_keys>-billingdocument
                                     AND billingdocumenttype    EQ @<fs_keys>-BillingDocumentType.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_doc_ndd
        FOR ALL ENTRIES IN @keys
        WHERE CompanyCode            = @keys-CompanyCode
          AND FiscalYear             = @keys-FiscalYear
          AND AccountingDocument     = @keys-AccountingDocument
          AND AccountingDocumentType = @keys-AccountingDocumentType
          AND BillingDocument        = @keys-BillingDocument
          AND BillingDocumentType    = @keys-BillingDocumentType
        into CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

  METHOD lock.

  ENDMETHOD.

ENDCLASS.
