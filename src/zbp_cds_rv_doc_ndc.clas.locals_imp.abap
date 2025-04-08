CLASS lsc_zcds_rv_doc_ndc DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_doc_ndc IMPLEMENTATION.

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

CLASS lhc_CreditNotes DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR CreditNotes RESULT result.

    METHODS SendDocument FOR MODIFY
      IMPORTING keys FOR ACTION CreditNotes~SendDocument RESULT result.

    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION CreditNotes~UpdateStatus RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR CreditNotes RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR CreditNotes RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE CreditNotes.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE CreditNotes.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE CreditNotes.

    METHODS read FOR READ
      IMPORTING keys FOR READ CreditNotes RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK CreditNotes.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

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
                                                               fiscalyear  = <fs_result>-Fiscalyear
                                                       accountingdocument  = <fs_result>-Accountingdocument
                                                   accountingdocumenttype  = <fs_result>-Accountingdocumenttype
                                                       billingdocument     = <fs_result>-Billingdocument
                                                       billingdocumenttype = <fs_result>-Billingdocumenttype.

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
          lv_clave   TYPE string,
          lv_id      TYPE sgtxt,
          lv_message TYPE string,
          lv_date    TYPE datum,
          lv_update  TYPE c.

    DATA: ls_ec_ndc  TYPE zdt_sd_doc_ndc.

    DATA: lt_ec_008    TYPE STANDARD TABLE OF zdt_ec_008,
          ls_ec_008    TYPE zdt_ec_008,
          lt_entry     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Change,
          ls_entry     LIKE LINE OF lt_entry,
          ls_aparitem  LIKE LINE OF ls_entry-%param-_aparitems,
          lt_cre_ndc   TYPE TABLE FOR CREATE zcds_rv_doc_ndc,
          lt_upd_ndc   TYPE TABLE FOR UPDATE zcds_rv_doc_ndc,
          lt_ec_002    TYPE STANDARD TABLE OF zdt_ec_002,
          ls_ec_002    TYPE zdt_ec_002.

    DATA: lo_CreditNotes TYPE REF TO zcl_create_nota_credito,
          lo_xml     TYPE REF TO zcl_create_xml_emi,
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
    WHERE companycode NE @space
    INTO TABLE @lt_ec_002.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE users EQ @sy-uname
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

        CLEAR: lv_update.

        if <fs_CreditNote>-Accesskey IS NOT INITIAL.
          lv_update = abap_true.
        ENDIF.

        CREATE OBJECT lo_CreditNotes
          EXPORTING
            companycode            = <fs_CreditNote>-Companycode
            fiscalyear             = <fs_CreditNote>-Fiscalyear
            accountingdocument     = <fs_CreditNote>-Accountingdocument
            accountingdocumenttype = <fs_CreditNote>-Accountingdocumenttype
            billingdocument        = <fs_CreditNote>-Billingdocument
            billingdocumenttype    = <fs_CreditNote>-Billingdocumenttype.

        CLEAR: ls_inf_tribu, ls_notacredito, lt_impuesto, lt_detalle,
               lt_det_add, lt_det_imp, lt_head_add, lv_message.

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
            t_head_add   = lt_head_add
            message      = lv_message.

        IF lv_message IS NOT INITIAL.

          lo_msg = new_message( id = 'ZMC_DOC_ELEC'  " id = Name Of message class
                            number = '008' "number of message defined in the message class
                          severity = cl_abap_behv=>ms-error
                                v1 = lv_message ). "type of message

          <fs_CreditNote>-Messagedocument = lv_message.

          APPEND VALUE #(  %tky = <fs_CreditNote>-%tky ) TO failed-creditnotes.

          APPEND VALUE #(  %tky      = <fs_CreditNote>-%tky
                         %state_area = 'VALIDATE_SEQUENTIAL'
                         %msg        =  lo_msg )
          TO reported-creditnotes.

          CONTINUE.

        ENDIF.

        <fs_CreditNote>-Idnumber      = ls_notacredito-idcomprador.
        <fs_CreditNote>-Typeid        = ls_notacredito-tipoidcomprador.
        <fs_CreditNote>-Businessname  = ls_notacredito-razonsocialcomprador.
        <fs_CreditNote>-Establishment = ls_inf_tribu-estab.
        <fs_CreditNote>-Emissionpoint = ls_inf_tribu-ptoemi.
        <fs_CreditNote>-Sequential    = ls_inf_tribu-secuencial.
        <fs_CreditNote>-accesskey     = ls_inf_tribu-claveacceso.
        <fs_CreditNote>-issuedate     = <fs_creditnote>-BillingDocumentDate.

        ls_entry-%param-DocumentReferenceID          = |{ <fs_creditnote>-Establishment }{ <fs_creditnote>-Emissionpoint }{ <fs_creditnote>-Sequential } |.
        ls_entry-%param-%control-DocumentReferenceID = if_abap_behv=>mk-on.

        ls_aparitem-GLAccountLineItem          = '000001'.
        ls_aparitem-documentitemtext           = ls_inf_tribu-claveacceso.
        ls_aparitem-%control-GLAccountLineItem = if_abap_behv=>mk-on.
        ls_aparitem-%control-documentitemtext  = if_abap_behv=>mk-on.
        ls_entry-%param-%control-_aparitems    = if_abap_behv=>mk-on.
        APPEND ls_aparitem TO ls_entry-%param-_aparitems.

        ls_entry-%key-AccountingDocument   = ls_entry-AccountingDocument = <fs_creditnote>-AccountingDocument.
        ls_entry-%key-CompanyCode          = ls_entry-CompanyCode = <fs_creditnote>-CompanyCode.
        ls_entry-%key-FiscalYear           = ls_entry-FiscalYear = <fs_creditnote>-Fiscalyear.

        APPEND ls_entry TO lt_entry.

        CLEAR: lv_xml, lv_clave, ls_entry, ls_aparitem.

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

        lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

        CALL METHOD cl_web_http_utility=>encode_x_base64
          EXPORTING
            unencoded = lv_raw
          RECEIVING
            encoded   = lv_base64.

        <fs_CreditNote>-xml  = lv_raw.
        <fs_CreditNote>-Mimetype  = 'text/xml'.
        <fs_CreditNote>-filename = |{ ls_inf_tribu-claveacceso }.xml|.

        lv_clave = |{ <fs_CreditNote>-Accesskey }.txt|.

        CREATE OBJECT lo_emision
          EXPORTING
            documentsupplier = lv_id
            clave            = lv_clave
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

        IF lv_update IS INITIAL.

          lt_cre_ndc = VALUE #( (  CompanyCode            = <fs_CreditNote>-CompanyCode
                                   FiscalYear             = <fs_CreditNote>-FiscalYear
                                   AccountingDocument     = <fs_CreditNote>-AccountingDocument
                                   AccountingDocumentType = <fs_CreditNote>-AccountingDocumentType
                                   BillingDocument        = <fs_CreditNote>-BillingDocument
                                   BillingDocumentType    = <fs_CreditNote>-BillingDocumentType
                                   SoldtoParty            = <fs_CreditNote>-SoldtoParty
                                   BusinessName           = <fs_CreditNote>-BusinessName
                                   TypeId                 = <fs_CreditNote>-TypeId
                                   IdNumber               = <fs_CreditNote>-IdNumber
                                   Establishment          = <fs_CreditNote>-Establishment
                                   EmissionPoint          = <fs_CreditNote>-EmissionPoint
                                   Sequential             = <fs_CreditNote>-Sequential
                                   Accesskey              = <fs_CreditNote>-Accesskey
                                   DocumentType           = <fs_CreditNote>-DocumentType
                                   IssueDate              = <fs_CreditNote>-IssueDate
                                   DocumentStatus         = <fs_CreditNote>-DocumentStatus
                                   MessageDocument        = <fs_CreditNote>-MessageDocument
                                   AuthorizationDate      = <fs_CreditNote>-AuthorizationDate
                                   Xml                    = <fs_CreditNote>-Xml
                                   MimeType               = <fs_CreditNote>-MimeType
                                   FileName               = <fs_CreditNote>-FileName
                                   DocumentSupplier       = <fs_CreditNote>-DocumentSupplier
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

          lt_upd_ndc = VALUE #( (  CompanyCode           = <fs_CreditNote>-CompanyCode
                                   FiscalYear             = <fs_CreditNote>-FiscalYear
                                   AccountingDocument     = <fs_CreditNote>-AccountingDocument
                                   AccountingDocumentType = <fs_CreditNote>-AccountingDocumentType
                                   BillingDocument        = <fs_CreditNote>-BillingDocument
                                   BillingDocumentType    = <fs_CreditNote>-BillingDocumentType
                                   SoldtoParty            = <fs_CreditNote>-SoldtoParty
                                   BusinessName           = <fs_CreditNote>-BusinessName
                                   TypeId                 = <fs_CreditNote>-TypeId
                                   IdNumber               = <fs_CreditNote>-IdNumber
                                   Establishment          = <fs_CreditNote>-Establishment
                                   EmissionPoint          = <fs_CreditNote>-EmissionPoint
                                   Sequential             = <fs_CreditNote>-Sequential
                                   Accesskey              = <fs_CreditNote>-Accesskey
                                   DocumentType           = <fs_CreditNote>-DocumentType
                                   IssueDate              = <fs_CreditNote>-IssueDate
                                   DocumentStatus         = <fs_CreditNote>-DocumentStatus
                                   MessageDocument        = <fs_CreditNote>-MessageDocument
                                   AuthorizationDate      = <fs_CreditNote>-AuthorizationDate
                                   Xml                    = <fs_CreditNote>-Xml
                                   MimeType               = <fs_CreditNote>-MimeType
                                   FileName               = <fs_CreditNote>-FileName
                                   DocumentSupplier       = <fs_CreditNote>-DocumentSupplier
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
            %msg = new_message_with_text( text = |{ <fs_CreditNote>-BillingDocument } { <fs_CreditNote>-Documentstatus } { <fs_CreditNote>-Messagedocument }{ <fs_CreditNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
         ) INTO TABLE reported-creditnotes.

        FREE: lo_emision, lo_xml, lo_CreditNotes.

      ENDLOOP.


     IF lt_cre_ndc[] IS NOT INITIAL.
*         me->create( EXPORTING entities = lt_cre_fac ).

       MODIFY ENTITIES OF zcds_rv_doc_ndc IN LOCAL MODE
         ENTITY CreditNotes
         CREATE FROM lt_cre_ndc
         REPORTED DATA(lt_reported)
         FAILED DATA(lt_failed)
         MAPPED DATA(lt_mapped).

      ENDIF.

      IF lt_upd_ndc[] IS NOT INITIAL.
*        me->update( EXPORTING entities = lt_upd_fac ).
       MODIFY ENTITIES OF zcds_rv_doc_ndc IN LOCAL MODE
         ENTITY CreditNotes
         UPDATE FROM lt_upd_ndc
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

    result = VALUE #( FOR CreditNote IN CreditNotes
                    ( %tky        = CreditNote-%tky
                      %param      = CreditNote ) ).

  ENDMETHOD.

  METHOD UpdateStatus.

    DATA: lv_id      TYPE sgtxt,
          lv_xml     TYPE string,
          lv_mensaje TYPE string,
          lv_clave   TYPE string,
          lv_date    TYPE datum.

    DATA: ls_ec_ndc  TYPE zdt_sd_doc_ndc.

    DATA: lt_upd_ndc  TYPE TABLE FOR UPDATE zcds_rv_doc_ndc.

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

      CLEAR: lv_date, lv_clave.

      lv_id      = <fs_CreditNote>-Accesskey.
      lv_clave   = <fs_CreditNote>-Accesskey.

      CREATE OBJECT lo_emision
        EXPORTING
          documentsupplier = lv_id
          clave            = lv_clave
          documenttype     = <fs_CreditNote>-Documenttype
          companycode      = <fs_CreditNote>-Companycode
          xml              = lv_xml
          establishment    = <fs_CreditNote>-Establishment
          emissionpoint    = <fs_CreditNote>-Emissionpoint
          sequential       = <fs_CreditNote>-Sequential.

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

      lt_upd_ndc = VALUE #( (  CompanyCode            = <fs_CreditNote>-CompanyCode
                               FiscalYear             = <fs_CreditNote>-FiscalYear
                               AccountingDocument     = <fs_CreditNote>-AccountingDocument
                               AccountingDocumentType = <fs_CreditNote>-AccountingDocumentType
                               BillingDocument        = <fs_CreditNote>-BillingDocument
                               BillingDocumentType    = <fs_CreditNote>-BillingDocumentType
                               DocumentStatus         = <fs_CreditNote>-DocumentStatus
                               MessageDocument        = <fs_CreditNote>-MessageDocument
                               AuthorizationDate      = <fs_CreditNote>-AuthorizationDate
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
            %msg = new_message_with_text( text = |{ <fs_CreditNote>-BillingDocument } { <fs_CreditNote>-Documentstatus } { <fs_CreditNote>-Messagedocument }{ <fs_CreditNote>-Authorizationdate } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-creditnotes.

      FREE: lo_emision.

    ENDLOOP.

    IF lt_upd_ndc IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_doc_ndc IN LOCAL MODE
        ENTITY CreditNotes
        UPDATE FROM lt_upd_ndc
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.

    result = VALUE #( FOR CreditNote IN CreditNotes
                    ( %tky = CreditNote-%tky
                      %param = CreditNote ) ).


  ENDMETHOD.


  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_sd_doc_ndc,
           ls_cre_ndc   TYPE STRUCTURE FOR CREATE zcds_rv_doc_ndc.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_sd_doc_ndc FROM TABLE @lt_inserts.

    ENDIF.


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_sd_doc_ndc,
           lt_updates     TYPE STANDARD TABLE OF zdt_sd_doc_ndc,
           lt_controls    TYPE STANDARD TABLE OF zdt_sd_doc_ndc.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_sd_doc_ndc
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

    UPDATE zdt_sd_doc_ndc FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.


    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_sd_doc_ndc WHERE companycode            EQ @<fs_keys>-companycode
                                     AND fiscalyear             EQ @<fs_keys>-fiscalyear
                                     AND accountingdocument     EQ @<fs_keys>-accountingdocument
                                     AND accountingdocumenttype EQ @<fs_keys>-AccountingDocumentType
                                     AND billingdocument        EQ @<fs_keys>-billingdocument
                                     AND billingdocumenttype    EQ @<fs_keys>-BillingDocumentType.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_doc_ndc
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
