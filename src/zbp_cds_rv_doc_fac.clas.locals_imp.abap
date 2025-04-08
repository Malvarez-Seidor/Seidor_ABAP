CLASS lsc_zcds_rv_doc_fac DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_fac IMPLEMENTATION.

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

CLASS lhc_InvoiceDocuments DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR InvoiceDocuments RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION InvoiceDocuments~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION InvoiceDocuments~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR InvoiceDocuments RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR invoicedocuments RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE invoicedocuments.

    METHODS update FOR MODIFY
      IMPORTING entities  FOR UPDATE invoicedocuments.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE invoicedocuments.

    METHODS read FOR READ
      IMPORTING keys FOR READ invoicedocuments RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK invoicedocuments.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_InvoiceDocuments IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_doc_fac  IN LOCAL MODE
      ENTITY InvoiceDocuments
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Documentstatus BillingDocumentIsCancelled )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Facturas)
      FAILED failed.

    result = VALUE #( FOR Factura IN Facturas
                    ( %tky = Factura-%tky
                      %features-%action-SendDocument
          = COND #( WHEN Factura-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
                    WHEN Factura-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
                    WHEN Factura-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled ) ) ).

    LOOP AT result ASSIGNING FIELD-SYMBOL(<fs_result>).

      READ TABLE Facturas INTO DATA(ls_Factura) WITH KEY companycode  = <fs_result>-Companycode
                                                          fiscalyear  = <fs_result>-Fiscalyear
                                                  accountingdocument  = <fs_result>-Accountingdocument
                                              accountingdocumenttype  = <fs_result>-Accountingdocumenttype
                                                  billingdocument     = <fs_result>-Billingdocument
                                                  BillingDocumentType = <fs_result>-BillingDocumentType.

      IF sy-subrc EQ 0.

        IF ls_Factura-Documentstatus EQ 'PROCESS'.
          <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
        ELSE.
            <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
        ENDIF.

        IF ls_Factura-BillingDocumentIsCancelled IS NOT INITIAL.
            <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
        ENDIF.

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
          lv_user    TYPE sy-uname,
          lv_message TYPE string,
          lv_date    TYPE datum,
          lv_update  TYPE c.

    DATA: lt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008    TYPE zdt_ec_008,
          lt_ec_002    TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002    TYPE zdt_ec_002,
          lt_entry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Change,
          ls_entry     LIKE LINE OF lt_entry,
          ls_aparitem  LIKE LINE OF ls_entry-%param-_aparitems,
          lt_cre_fac   TYPE TABLE FOR CREATE zcds_rv_doc_fac,
          lt_upd_fac   TYPE TABLE FOR UPDATE zcds_rv_doc_fac,
          ls_ec_fac    TYPE zdt_sd_doc_fac.

    DATA: lo_factura TYPE REF TO zcl_create_factura,
          lo_xml     TYPE REF TO zcl_create_xml_emi,
          lo_emision TYPE REF TO zcl_hs_emision_doc.

    DATA: ls_inf_tribu TYPE zts_inf_tribu,
          ls_factura   TYPE zts_fac_header,
          lt_impuesto  TYPE zcl_create_factura=>ty_impuesto,
          lt_pagos     TYPE zcl_create_factura=>ty_pagos,
          lt_detalle   TYPE zcl_create_factura=>ty_detalle_f,
          lt_det_add   TYPE zcl_create_factura=>ty_det_add,
          lt_det_imp   TYPE zcl_create_factura=>ty_det_imp,
          lt_reembolso TYPE zcl_create_factura=>ty_reembolso,
          lt_reem_imp  TYPE zcl_create_factura=>ty_reem_imp,
          lt_head_add  TYPE zcl_create_factura=>ty_head_add.

    READ ENTITIES OF zcds_rv_doc_fac  IN LOCAL MODE
      ENTITY InvoiceDocuments
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier
               BillingDocumentDate salesorganization )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Facturas).

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
      FROM zdt_ec_002
      WHERE companycode NE @space
      INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence,  accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
      FROM zdt_ec_008
      WHERE users EQ @sy-uname
      INTO TABLE @lt_ec_008.

    LOOP AT Facturas ASSIGNING FIELD-SYMBOL(<fs_Factura>).

      SELECT SINGLE plant
       FROM I_BillingDocumentItem
       WHERE plant IS NOT INITIAL
         AND BillingDocument     EQ @<fs_Factura>-BillingDocument
         AND BillingDocumentType EQ @<fs_Factura>-Billingdocumenttype
       INTO @DATA(lv_plant).

      IF sy-subrc EQ 0.
        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_Factura>-CompanyCode
                                                   billingdocumenttype = <fs_Factura>-billingdocumenttype
                                                   salesorganization   = <fs_Factura>-salesorganization
                                                   plant               = lv_plant
                                                   documentsri         = <fs_Factura>-Documenttype
                                                   users               = sy-uname.
        IF sy-subrc NE 0.
          READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_Factura>-CompanyCode
                                                     billingdocumenttype = <fs_Factura>-billingdocumenttype
                                                     plant               = lv_plant
                                                     documentsri         = <fs_Factura>-Documenttype
                                                     users               = sy-uname.

          IF sy-subrc NE 0.
            READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode       = <fs_Factura>-CompanyCode
                                                       billingdocumenttype = <fs_Factura>-billingdocumenttype
                                                       salesorganization   = <fs_Factura>-salesorganization
                                                       documentsri         = <fs_Factura>-Documenttype
                                                       users               = sy-uname.
          ENDIF.
        ENDIF.

      ELSE.

        READ TABLE lt_ec_008 INTO ls_ec_008 WITH KEY companycode           = <fs_Factura>-CompanyCode
                                                       billingdocumenttype = <fs_Factura>-billingdocumenttype
                                                       salesorganization   = <fs_Factura>-salesorganization
                                                       documentsri         = <fs_Factura>-Documenttype
                                                       users               = sy-uname.
      ENDIF.

      IF ls_ec_008 IS NOT INITIAL.

        READ TABLE lt_ec_002 INTO ls_ec_002 WITH KEY companycode    = <fs_Factura>-CompanyCode
                                                     establishment  = ls_ec_008-establishment
                                                     emissionpoint  = ls_ec_008-emissionpoint
                                                     documentsri    = <fs_Factura>-Documenttype.
        IF ls_ec_002 IS INITIAL.

          lv_flag = 'X'.

          DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                                  number = '003' "number of message defined in the message class
                                severity = cl_abap_behv=>ms-error "type of message
                                      v1 = ls_ec_008-establishment   "First Parameter
                                      v2 = ls_ec_008-emissionpoint )."Second Parameter


          <fs_Factura>-Messagedocument = 'Objecto de Rango de Numeros No asignado'.

          APPEND VALUE #(  %tky = <fs_Factura>-%tky ) TO failed-invoicedocuments.

          APPEND VALUE #(  %tky        = <fs_Factura>-%tky
                           %state_area = 'VALIDATE_SEQUENTIAL'
                           %msg        = lo_msg )
          TO reported-invoicedocuments.

        ENDIF.

      ELSE.

        lv_flag = 'X'.

        lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                          number = '004' "number of message defined in the message class
                        severity = cl_abap_behv=>ms-error ). "type of message

        <fs_Factura>-Messagedocument = 'Usuario no asignado Establecimiento y Pto de Emisión'.

        APPEND VALUE #(  %tky = <fs_Factura>-%tky ) TO failed-invoicedocuments.

        APPEND VALUE #(  %tky        = <fs_Factura>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
        TO reported-invoicedocuments.

      ENDIF.

    ENDLOOP.

    IF lv_flag IS INITIAL.

      LOOP AT Facturas ASSIGNING <fs_Factura>.

        CLEAR: lv_update.

        if <fs_Factura>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_factura
          EXPORTING
            companycode            = <fs_Factura>-Companycode
            fiscalyear             = <fs_Factura>-Fiscalyear
            accountingdocument     = <fs_Factura>-Accountingdocument
            accountingdocumenttype = <fs_Factura>-Accountingdocumenttype
            billingdocument        = <fs_Factura>-Billingdocument
            billingdocumenttype    = <fs_Factura>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_factura, lt_impuesto, lt_pagos, lt_detalle, lt_det_add,
               lt_det_imp, lt_reembolso, lt_reem_imp, lt_head_add, lv_mensaje.

        CALL METHOD lo_factura->callDocumentType
          EXPORTING
            documenttype = <fs_Factura>-Documenttype
          IMPORTING
            inf_tribu    = ls_inf_tribu
            factura      = ls_factura
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

          <fs_Factura>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_Factura>-%tky ) TO failed-invoicedocuments.

          APPEND VALUE #(  %tky      = <fs_Factura>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-invoicedocuments.

          CONTINUE.

        ENDIF.

        <fs_Factura>-Idnumber      = ls_factura-idcomprador.
        <fs_Factura>-Typeid        = ls_factura-tipoidcomprador.
        <fs_Factura>-Businessname  = ls_factura-razonsocialcomprador.
        <fs_Factura>-Establishment = ls_inf_tribu-estab.
        <fs_Factura>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_Factura>-Sequential    = ls_inf_tribu-secuencial.
        <fs_Factura>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_Factura>-issuedate     = <fs_Factura>-BillingDocumentDate.

        ls_entry-%param-DocumentReferenceID          = |{ <fs_Factura>-Establishment }{ <fs_Factura>-Emissionpoint }{ <fs_Factura>-Sequential } |.
        ls_entry-%param-%control-DocumentReferenceID = if_abap_behv=>mk-on.

        ls_aparitem-GLAccountLineItem          = '000001'.
        ls_aparitem-documentitemtext           = ls_inf_tribu-claveacceso.
        ls_aparitem-%control-GLAccountLineItem = if_abap_behv=>mk-on.
        ls_aparitem-%control-documentitemtext  = if_abap_behv=>mk-on.
        ls_entry-%param-%control-_aparitems    = if_abap_behv=>mk-on.
        APPEND ls_aparitem TO ls_entry-%param-_aparitems.

        ls_entry-%key-AccountingDocument   = ls_entry-AccountingDocument = <fs_Factura>-AccountingDocument.
        ls_entry-%key-CompanyCode          = ls_entry-CompanyCode = <fs_Factura>-CompanyCode.
        ls_entry-%key-FiscalYear           = ls_entry-FiscalYear = <fs_Factura>-Fiscalyear.

        APPEND ls_entry TO lt_entry.

        CLEAR: lv_xml, ls_entry, ls_aparitem.

        CREATE OBJECT lo_xml.

        CALL METHOD lo_xml->factura
          EXPORTING
            header    = ls_factura
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

*        CALL METHOD cl_web_http_utility=>encode_base64
*          EXPORTING
*            unencoded = lv_xml
*          RECEIVING
*            encoded   = lv_base64.
*
*        CALL METHOD cl_web_http_utility=>encode_utf8
*            EXPORTING
*            unencoded = lv_xml
*          RECEIVING
*            encoded   = lv_base64.

*        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_Factura>-%data-xml = <fs_Factura>-xml = lv_raw.

        <fs_Factura>-Mimetype = 'text/xml'.
        <fs_Factura>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave = |{ <fs_Factura>-Accesskey }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
            documenttype     = <fs_Factura>-Documenttype
            companycode      = <fs_Factura>-Companycode
            xml              = lv_base64.


        CALL METHOD lo_emision->send_request_by_url(
          IMPORTING
            documentsupplier = lv_id
            estado           = <fs_Factura>-Documentstatus
            messagedocument  = lv_mensaje ).


        <fs_Factura>-Documentsupplier = lv_id.
        <fs_Factura>-Messagedocument  = lv_mensaje.


        IF lv_update IS INITIAL.

          lt_cre_fac = VALUE #( (  CompanyCode            = <fs_Factura>-CompanyCode
                                   FiscalYear             = <fs_Factura>-FiscalYear
                                   AccountingDocument     = <fs_Factura>-AccountingDocument
                                   AccountingDocumentType = <fs_Factura>-AccountingDocumentType
                                   BillingDocument        = <fs_Factura>-BillingDocument
                                   BillingDocumentType    = <fs_Factura>-BillingDocumentType
                                   SoldtoParty            = <fs_Factura>-SoldtoParty
                                   BusinessName           = <fs_Factura>-BusinessName
                                   TypeId                 = <fs_Factura>-TypeId
                                   IdNumber               = <fs_Factura>-IdNumber
                                   Establishment          = <fs_Factura>-Establishment
                                   EmissionPoint          = <fs_Factura>-EmissionPoint
                                   Sequential             = <fs_Factura>-Sequential
                                   Accesskey              = <fs_Factura>-Accesskey
                                   DocumentType           = <fs_Factura>-DocumentType
                                   IssueDate              = <fs_Factura>-IssueDate
                                   DocumentStatus         = <fs_Factura>-DocumentStatus
                                   MessageDocument        = <fs_Factura>-MessageDocument
                                   AuthorizationDate      = <fs_Factura>-AuthorizationDate
                                   Xml                    = <fs_Factura>-Xml
                                   MimeType               = <fs_Factura>-MimeType
                                   FileName               = <fs_Factura>-FileName
                                   DocumentSupplier       = <fs_Factura>-DocumentSupplier
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

          lt_upd_fac = VALUE #( (  CompanyCode           = <fs_Factura>-CompanyCode
                                   FiscalYear             = <fs_Factura>-FiscalYear
                                   AccountingDocument     = <fs_Factura>-AccountingDocument
                                   AccountingDocumentType = <fs_Factura>-AccountingDocumentType
                                   BillingDocument        = <fs_Factura>-BillingDocument
                                   BillingDocumentType    = <fs_Factura>-BillingDocumentType
                                   SoldtoParty            = <fs_Factura>-SoldtoParty
                                   BusinessName           = <fs_Factura>-BusinessName
                                   TypeId                 = <fs_Factura>-TypeId
                                   IdNumber               = <fs_Factura>-IdNumber
                                   Establishment          = <fs_Factura>-Establishment
                                   EmissionPoint          = <fs_Factura>-EmissionPoint
                                   Sequential             = <fs_Factura>-Sequential
                                   Accesskey              = <fs_Factura>-Accesskey
                                   DocumentType           = <fs_Factura>-DocumentType
                                   IssueDate              = <fs_Factura>-IssueDate
                                   DocumentStatus         = <fs_Factura>-DocumentStatus
                                   MessageDocument        = <fs_Factura>-MessageDocument
                                   AuthorizationDate      = <fs_Factura>-AuthorizationDate
                                   Xml                    = <fs_Factura>-Xml
                                   MimeType               = <fs_Factura>-MimeType
                                   FileName               = <fs_Factura>-FileName
                                   DocumentSupplier       = <fs_Factura>-DocumentSupplier
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

        INSERT VALUE #( %msg = new_message_with_text(
                    text = |{ <fs_Factura>-BillingDocument } { <fs_Factura>-Documentstatus } { <fs_Factura>-Messagedocument } |
                severity = if_abap_behv_message=>severity-success )
          ) INTO TABLE reported-invoicedocuments.

        FREE: lo_emision, lo_xml, lo_factura.

      ENDLOOP.

      IF lt_cre_fac[] IS NOT INITIAL.
*         me->create( EXPORTING entities = lt_cre_fac ).

       MODIFY ENTITIES OF zcds_rv_doc_fac IN LOCAL MODE
         ENTITY InvoiceDocuments
         CREATE FROM lt_cre_fac
         REPORTED DATA(lt_reported)
         FAILED DATA(lt_failed)
         MAPPED DATA(lt_mapped).

      ENDIF.

      IF lt_upd_fac[] IS NOT INITIAL.
*        me->update( EXPORTING entities = lt_upd_fac ).
       MODIFY ENTITIES OF zcds_rv_doc_fac IN LOCAL MODE
         ENTITY InvoiceDocuments
         UPDATE FROM lt_upd_fac
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

    ENDIF.

    result = VALUE #( FOR Factura IN Facturas
                    ( %tky         = Factura-%tky
                      %param-%data = Factura-%data ) ).


  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_clave   TYPE string,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_fac  TYPE zdt_sd_doc_fac.

    DATA: lt_upd_fac  TYPE TABLE FOR UPDATE zcds_rv_doc_fac.

    DATA: lo_emision TYPE REF TO zcl_hs_emision_doc.

    READ ENTITIES OF zcds_rv_doc_fac  IN LOCAL MODE
      ENTITY InvoiceDocuments
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier )
      WITH CORRESPONDING #( keys )
      RESULT DATA(Facturas).

    LOOP AT Facturas ASSIGNING FIELD-SYMBOL(<fs_Factura>).

      CLEAR: lv_date, lv_clave.

      lv_id    = <fs_Factura>-Accesskey.
      lv_clave = <fs_Factura>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_Factura>-Documenttype
          companycode      = <fs_Factura>-Companycode
          xml              = lv_xml
          establishment    = <fs_Factura>-Establishment
          emissionpoint    = <fs_Factura>-Emissionpoint
          sequential       = <fs_Factura>-Sequential.

      CALL METHOD lo_emision->send_request_by_url(
        IMPORTING
          authorizationdate = lv_date
          documentsupplier  = lv_id
          estado            = <fs_Factura>-Documentstatus
          messagedocument   = lv_mensaje ).

      IF lv_date IS NOT INITIAL.
        <fs_Factura>-Authorizationdate = lv_date.
      ENDIF.

      <fs_Factura>-Messagedocument  = lv_mensaje.

      lt_upd_fac = VALUE #( (  CompanyCode           = <fs_Factura>-CompanyCode
                               FiscalYear             = <fs_Factura>-FiscalYear
                               AccountingDocument     = <fs_Factura>-AccountingDocument
                               AccountingDocumentType = <fs_Factura>-AccountingDocumentType
                               BillingDocument        = <fs_Factura>-BillingDocument
                               BillingDocumentType    = <fs_Factura>-BillingDocumentType
                               DocumentStatus         = <fs_Factura>-DocumentStatus
                               MessageDocument        = <fs_Factura>-MessageDocument
                               AuthorizationDate      = <fs_Factura>-AuthorizationDate
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
            %msg = new_message_with_text( text = |{ <fs_Factura>-BillingDocument } { <fs_Factura>-Documentstatus } { <fs_Factura>-Messagedocument }{ <fs_Factura>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-invoicedocuments.

      FREE: lo_emision.

    ENDLOOP.

    IF lt_upd_fac IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_fac IN LOCAL MODE
        ENTITY InvoiceDocuments
        UPDATE FROM lt_upd_fac
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.

    result = VALUE #( FOR Factura IN Facturas
                    ( %tky = Factura-%tky
                      %param = Factura ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_sd_doc_fac,
           ls_cre_fac   TYPE STRUCTURE FOR CREATE zcds_rv_doc_fac.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_sd_doc_fac FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_sd_doc_fac,
           lt_updates     TYPE STANDARD TABLE OF zdt_sd_doc_fac,
           lt_controls    TYPE STANDARD TABLE OF zdt_sd_doc_fac.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_sd_doc_fac
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode            EQ @lt_inserts-companycode
          AND fiscalyear             EQ @lt_inserts-fiscalyear
          AND accountingdocument     EQ @lt_inserts-accountingdocument
          AND accountingdocumenttype EQ @lt_inserts-accountingdocumenttype
          AND billingdocument        EQ @lt_inserts-billingdocument
          AND billingdocumenttype    EQ @lt_inserts-billingdocumenttype
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

    UPDATE zdt_sd_doc_fac FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.


    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_sd_doc_fac WHERE companycode            EQ @<fs_keys>-companycode
                                     AND fiscalyear             EQ @<fs_keys>-fiscalyear
                                     AND accountingdocument     EQ @<fs_keys>-accountingdocument
                                     AND accountingdocumenttype EQ @<fs_keys>-AccountingDocumentType
                                     AND billingdocument        EQ @<fs_keys>-billingdocument
                                     AND billingdocumenttype    EQ @<fs_keys>-BillingDocumentType.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_doc_fac
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
