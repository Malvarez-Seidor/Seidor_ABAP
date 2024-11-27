CLASS lhc_InvoiceDocuments DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR InvoiceDocuments RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION InvoiceDocuments~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION InvoiceDocuments~UpdateStatus RESULT result.

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

      READ TABLE Facturas INTO DATA(ls_Factura) WITH KEY companycode = <fs_result>-Companycode
                                                          fiscalyear = <fs_result>-Fiscalyear
                                                  accountingdocument = <fs_result>-Accountingdocument
                                              accountingdocumenttype = <fs_result>-Accountingdocumenttype
                                                  billingdocument    = <fs_result>-Billingdocument.

      IF ls_Factura-Documentstatus EQ 'PROCESS' AND sy-subrc EQ 0.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-enabled.
      ELSE.
        <fs_result>-%features-%action-UpdateStatus = if_abap_behv=>fc-o-disabled.
      ENDIF.

      IF ls_Factura-BillingDocumentIsCancelled IS NOT INITIAL.
        <fs_result>-%features-%action-SendDocument = if_abap_behv=>fc-o-disabled.
      ENDIF.

    ENDLOOP.
*                  (  %tky = Factura-%tky
*                     %features-%action-UpdateStatus
*         = COND #( WHEN Factura-Documentstatus EQ 'PROCESS' THEN if_abap_behv=>fc-o-enabled
*                   ELSE if_abap_behv=>fc-o-disabled ) ) ).

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
          lt_cre_fac TYPE TABLE FOR CREATE zcds_rv_doc_fac,
          lt_upd_fac TYPE TABLE FOR UPDATE zcds_rv_doc_fac,
          ls_ec_fac  TYPE zdt_sd_doc_fac.

    DATA: lo_factura TYPE REF TO zcl_create_factura,
          lo_xml     TYPE REF TO zcl_create_xml,
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
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence,  accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
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

        CREATE OBJECT lo_factura
          EXPORTING
            companycode            = <fs_Factura>-Companycode
            fiscalyear             = <fs_Factura>-Fiscalyear
            accountingdocument     = <fs_Factura>-Accountingdocument
            accountingdocumenttype = <fs_Factura>-Accountingdocumenttype
            billingdocument        = <fs_Factura>-Billingdocument
            billingdocumenttype    = <fs_Factura>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_factura, lt_impuesto, lt_pagos, lt_detalle,
               lt_det_add, lt_det_imp, lt_reembolso, lt_reem_imp, lt_head_add.

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
            t_head_add   = lt_head_add.

        <fs_Factura>-Idnumber      = ls_factura-idcomprador.
        <fs_Factura>-Typeid        = ls_factura-tipoidcomprador.
        <fs_Factura>-Businessname  = ls_factura-razonsocialcomprador.
        <fs_Factura>-Establishment = ls_inf_tribu-estab.
        <fs_Factura>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_Factura>-Sequential    = ls_inf_tribu-secuencial.
        <fs_Factura>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_Factura>-issuedate     = <fs_Factura>-BillingDocumentDate.
*        <fs_Factura>-issuedate     = |{ <fs_Factura>-BillingDocumentDate DATE = ENVIRONMENT }|.

        CLEAR: lv_xml.

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

        CALL METHOD cl_web_http_utility=>encode_base64
          EXPORTING
            unencoded = lv_xml
          RECEIVING
            encoded   = lv_base64.

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        <fs_Factura>-xml  = lv_raw.

        <fs_Factura>-Mimetype  = 'text/xml'.
        <fs_Factura>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
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

        INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_Factura>-BillingDocument } { <fs_Factura>-Documentstatus } { <fs_Factura>-Messagedocument } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-invoicedocuments.

        FREE: lo_emision, lo_xml, lo_factura.

      ENDLOOP.

      LOOP AT Facturas ASSIGNING <fs_Factura>.

        SELECT SINGLE *
         FROM zdt_sd_doc_fac
         WHERE companycode            EQ @<fs_Factura>-Companycode
           AND fiscalyear             EQ @<fs_Factura>-Fiscalyear
           AND accountingdocument     EQ @<fs_Factura>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_Factura>-Accountingdocumenttype
           AND billingdocument        EQ @<fs_Factura>-Billingdocument
           AND billingdocumenttype    EQ @<fs_Factura>-Billingdocumenttype
         INTO @ls_ec_fac.

        IF sy-subrc NE 0.
          MOVE-CORRESPONDING <fs_Factura> TO ls_ec_fac.
          INSERT zdt_sd_doc_fac FROM @ls_ec_fac.
        ELSE.
          MOVE-CORRESPONDING <fs_Factura> TO ls_ec_fac.
          UPDATE zdt_sd_doc_fac FROM @ls_ec_fac.
        ENDIF.

      ENDLOOP.

*      IF lt_cre_fac[] IS NOT INITIAL.
*
*        MODIFY ENTITIES OF zcds_rv_doc_fac IN LOCAL MODE
*        ENTITY InvoiceDocuments
*          CREATE FROM lt_cre_fac
*          MAPPED mapped
*          FAILED failed
*          REPORTED reported.
*
*      ENDIF.

*      IF lt_upd_fac[] IS NOT INITIAL.
*
*        MODIFY ENTITIES OF zcds_rv_doc_fac IN LOCAL MODE
*        ENTITY InvoiceDocuments
*          UPDATE FIELDS (  Companycode Fiscalyear Accountingdocument Accountingdocumenttype Billingdocument
*               Billingdocumenttype Soldtoparty Businessname Typeid Idnumber Establishment
*               Emissionpoint Sequential Accesskey Documenttype Issuedate Documentstatus
*               Messagedocument Authorizationdate Xml Mimetype Filename Documentsupplier
*               BillingDocumentDate salesorganization )
*          WITH CORRESPONDING #( lt_upd_fac ).
*
*      ENDIF.

    ENDIF.

    result = VALUE #( FOR Factura IN Facturas
                    ( %tky        = Factura-%tky
                      %param      = Factura ) ).

*        result = VALUE #( FOR Factura IN Facturas
*                    ( %tky = Factura-%tky
*                      %features-%action-SendDocument
*          = COND #( WHEN Factura-Documentstatus IS INITIAL   THEN if_abap_behv=>fc-o-enabled
*                    WHEN Factura-Documentstatus EQ 'PENDING' THEN if_abap_behv=>fc-o-enabled
*                    WHEN Factura-Documentstatus EQ 'ERROR'   THEN if_abap_behv=>fc-o-enabled
*                    ELSE if_abap_behv=>fc-o-disabled ) )
*                  (  %tky = Factura-%tky
*                     %features-%action-UpdateStatus
*         = COND #( WHEN Factura-Documentstatus EQ 'PROCESS' THEN if_abap_behv=>fc-o-enabled
*                   ELSE if_abap_behv=>fc-o-disabled ) ) ).


  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_fac  TYPE zdt_sd_doc_fac.

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

      CLEAR: lv_date.

      lv_id = <fs_Factura>-Documentsupplier.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          documenttype     = <fs_Factura>-Documenttype
          companycode      = <fs_Factura>-Companycode
          xml              = lv_xml.

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

      INSERT VALUE #(
            %msg = new_message_with_text( text = |{ <fs_Factura>-BillingDocument } { <fs_Factura>-Documentstatus } { <fs_Factura>-Messagedocument }{ <fs_Factura>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-invoicedocuments.

      FREE: lo_emision.

    ENDLOOP.

    LOOP AT Facturas ASSIGNING <fs_Factura>.

      SELECT SINGLE *
       FROM zdt_sd_doc_fac
       WHERE companycode            EQ @<fs_Factura>-Companycode
         AND fiscalyear             EQ @<fs_Factura>-Fiscalyear
         AND accountingdocument     EQ @<fs_Factura>-Accountingdocument
         AND accountingdocumenttype EQ @<fs_Factura>-Accountingdocumenttype
         AND billingdocument        EQ @<fs_Factura>-Billingdocument
         AND billingdocumenttype    EQ @<fs_Factura>-Billingdocumenttype
       INTO @ls_ec_fac.

      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING <fs_Factura> TO ls_ec_fac.
        UPDATE zdt_sd_doc_fac FROM @ls_ec_fac.
      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR Factura IN Facturas
                    ( %tky = Factura-%tky
                      %param = Factura ) ).

  ENDMETHOD.

ENDCLASS.
