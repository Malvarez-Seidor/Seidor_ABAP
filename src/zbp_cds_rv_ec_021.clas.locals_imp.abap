CLASS lsc_zcds_rv_ec_021 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zcds_rv_ec_021 IMPLEMENTATION.

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

CLASS lhc_ReportTransactionalAnnex DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR ReportTransactionalAnnex RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ReportTransactionalAnnex RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR ReportTransactionalAnnex RESULT result.

    METHODS Download FOR MODIFY
      IMPORTING keys FOR ACTION ReportTransactionalAnnex~Download RESULT result.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE ReportTransactionalAnnex.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE ReportTransactionalAnnex.

    METHODS read FOR READ
      IMPORTING keys FOR READ ReportTransactionalAnnex RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK ReportTransactionalAnnex.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE ReportTransactionalAnnex.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_ReportTransactionalAnnex IMPLEMENTATION.

  METHOD get_instance_features.

     READ ENTITIES OF ZCDS_RV_EC_021  IN LOCAL MODE
      ENTITY ReportTransactionalAnnex
      FIELDS ( Companycode Fiscalyear Monat Purchases Sales Export Cancel Xml FileName MimeType
*               StatusXML Status CompanyCodeName Criticality Description )
               StatusXML Status )
      WITH CORRESPONDING #( keys )
      RESULT DATA(AnexosTransaccional)
      FAILED failed.

    result = VALUE #( FOR AnexoTransaccional IN AnexosTransaccional
                    ( %tky = AnexoTransaccional-%tky
                      %features-%action-Download = if_abap_behv=>fc-o-enabled ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD Download.

    DATA: lv_xml     TYPE string,
          lv_raw     TYPE xstring,
          lv_base64  TYPE string,
          lv_monat   TYPE monat,
          lv_update  TYPE c.

    DATA: lt_cre_ats   TYPE TABLE FOR CREATE zcds_rv_ec_021,
          lt_upd_ats   TYPE TABLE FOR UPDATE zcds_rv_ec_021,
          ls_ec_ats    TYPE zdt_ec_021.

    DATA: lo_xml     TYPE REF TO zcl_create_xml_ats,
          lo_ats     TYPE REF TO zcl_create_ats.

    DATA: ls_informant    TYPE zts_informant,
          lt_purchases    TYPE zcl_create_xml_ats=>ty_purchases,
          lt_sales        TYPE zcl_create_xml_ats=>ty_sales,
          lt_export       TYPE zcl_create_xml_ats=>ty_export,
          lt_tot_sales    TYPE zcl_create_xml_ats=>ty_tot_sales,
          lt_canceled     TYPE zcl_create_xml_ats=>ty_canceled,
          lt_withholdings TYPE zcl_create_xml_ats=>ty_withholdings,
          lt_support      TYPE zcl_create_xml_ats=>ty_support,
          lt_pagos        TYPE zcl_create_xml_ats=>ty_pagos.

    READ ENTITIES OF zcds_rv_ec_021  IN LOCAL MODE
      ENTITY ReportTransactionalAnnex
      FIELDS ( Companycode Fiscalyear Monat Purchases Sales Export Cancel Xml FileName MimeType
               StatusXML Status CompanyCodeName Criticality Description )
      WITH CORRESPONDING #( keys )
      RESULT DATA(AnexosTransaccional).

    LOOP AT AnexosTransaccional ASSIGNING FIELD-SYMBOL(<fs_Anexo>).


      CLEAR: ls_informant, lt_canceled, lt_export, lt_pagos, lt_purchases, lt_sales,
             lt_tot_sales, lt_withholdings, lt_support, lv_monat, lv_update.

      SELECT SINGLE client, companycode, fiscalyear, monat, purchases, sales, export, cancel, status, xml, mimetype, filename
        FROM zdt_ec_021
        WHERE companycode EQ @<fs_Anexo>-Companycode
          AND fiscalyear  EQ @<fs_Anexo>-Fiscalyear
          AND monat       EQ @<fs_Anexo>-Monat

      INTO @ls_ec_ats.
      IF sy-subrc EQ 0.
        lv_update = abap_true.
      ENDIF.

      lv_monat = <fs_Anexo>-monat.

      CREATE OBJECT lo_ats
        EXPORTING
          companycode            = <fs_Anexo>-Companycode
          fiscalyear             = <fs_Anexo>-Fiscalyear
          fiscalperiod           = lv_monat.

        CALL METHOD lo_ats->callinformation
          IMPORTING
            is_informant    = ls_informant
            it_canceled     = lt_canceled
            it_export       = lt_export
            it_purchases    = lt_purchases
            it_sales        = lt_sales
            it_total_sales  = lt_tot_sales
            it_withholdings = lt_withholdings
            it_support      = lt_support.

      CLEAR: lv_xml.

      CREATE OBJECT lo_xml
        EXPORTING
          is_informant    = ls_informant
          it_canceled     = lt_canceled
          it_export       = lt_export
          it_purchases    = lt_purchases
          it_sales        = lt_sales
          it_total_sales  = lt_tot_sales
          it_withholdings = lt_withholdings
          it_support      = lt_support.

      CALL METHOD lo_xml->crearxml
        IMPORTING
          xml       = lv_xml.

      CALL METHOD cl_web_http_utility=>encode_base64
        EXPORTING
          unencoded = lv_xml
        RECEIVING
          encoded   = lv_base64.

      lv_raw = cl_abap_conv_codepage=>create_out( )->convert( lv_xml ).

      <fs_Anexo>-FileName  = |{ ls_informant-idinformante }_{ ls_informant-mes }_{ ls_informant-anio }.xml|.
      <fs_Anexo>-%data-xml = <fs_Anexo>-xml = lv_raw.

      <fs_Anexo>-Mimetype = 'text/xml'.

      <fs_Anexo>-Status   = 'GENERATED'.

      IF lv_update IS INITIAL.

        lt_cre_ats = VALUE #( (  CompanyCode            = <fs_Anexo>-CompanyCode
                                 FiscalYear             = <fs_Anexo>-FiscalYear
                                 Monat                  = <fs_Anexo>-Monat
                                 Purchases              = <fs_Anexo>-Purchases
                                 Sales                  = <fs_Anexo>-Sales
                                 Export                 = <fs_Anexo>-Export
                                 Cancel                 = <fs_Anexo>-Cancel
                                 Status                 = <fs_Anexo>-Status
                                 Xml                    = <fs_Anexo>-Xml
                                 MimeType               = <fs_Anexo>-MimeType
                                 FileName               = <fs_Anexo>-FileName
                                 %control = VALUE #(
                                    Companycode            = if_abap_behv=>mk-on
                                    Fiscalyear             = if_abap_behv=>mk-on
                                    Monat                  = if_abap_behv=>mk-on
                                    Purchases              = if_abap_behv=>mk-on
                                    Sales                  = if_abap_behv=>mk-on
                                    Export                 = if_abap_behv=>mk-on
                                    Cancel                 = if_abap_behv=>mk-on
                                    Status                 = if_abap_behv=>mk-on
                                    Xml                    = if_abap_behv=>mk-on
                                    MimeType               = if_abap_behv=>mk-on
                                    FileName               = if_abap_behv=>mk-on ) ) ).

      ELSE.

        lt_upd_ats = VALUE #( (  CompanyCode            = <fs_Anexo>-CompanyCode
                                 FiscalYear             = <fs_Anexo>-FiscalYear
                                 Monat                  = <fs_Anexo>-Monat
                                 Purchases              = <fs_Anexo>-Purchases
                                 Sales                  = <fs_Anexo>-Sales
                                 Export                 = <fs_Anexo>-Export
                                 Cancel                 = <fs_Anexo>-Cancel
                                 Status                 = <fs_Anexo>-Status
                                 Xml                    = <fs_Anexo>-Xml
                                 MimeType               = <fs_Anexo>-MimeType
                                 FileName               = <fs_Anexo>-FileName
                                 %control = VALUE #(
                                    Companycode            = if_abap_behv=>mk-on
                                    Fiscalyear             = if_abap_behv=>mk-on
                                    Monat                  = if_abap_behv=>mk-on
                                    Purchases              = if_abap_behv=>mk-on
                                    Sales                  = if_abap_behv=>mk-on
                                    Export                 = if_abap_behv=>mk-on
                                    Cancel                 = if_abap_behv=>mk-on
                                    Status                 = if_abap_behv=>mk-on
                                    Xml                    = if_abap_behv=>mk-on
                                    MimeType               = if_abap_behv=>mk-on
                                    FileName               = if_abap_behv=>mk-on ) ) ).

      ENDIF.

      INSERT VALUE #( %msg = new_message_with_text(
                  text = |{ <fs_Anexo>-Companycode } { <fs_Anexo>-FiscalYear } { <fs_Anexo>-Monat } |
              severity = if_abap_behv_message=>severity-success )
        ) INTO TABLE reported-ReportTransactionalAnnex.

      FREE: lo_xml, lo_ats.

    ENDLOOP.

    IF lt_cre_ats[] IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_ec_021 IN LOCAL MODE
        ENTITY ReportTransactionalAnnex
        CREATE FROM lt_cre_ats
        REPORTED DATA(lt_reported)
        FAILED DATA(lt_failed)
        MAPPED DATA(lt_mapped).

    ENDIF.

    IF lt_upd_ats[] IS NOT INITIAL.

      MODIFY ENTITIES OF zcds_rv_ec_021 IN LOCAL MODE
        ENTITY ReportTransactionalAnnex
        UPDATE FROM lt_upd_ats
        REPORTED lt_reported
        FAILED lt_failed
        MAPPED lt_mapped.

    ENDIF.

    result = VALUE #( FOR AnexoTransaccional IN AnexosTransaccional
                    ( %tky         = AnexoTransaccional-%tky
                      %param-%data = AnexoTransaccional-%data ) ).


  ENDMETHOD.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_ec_021,
           lt_updates     TYPE STANDARD TABLE OF zdt_ec_021,
           lt_controls    TYPE STANDARD TABLE OF zdt_ec_021.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_ec_021
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode            EQ @lt_inserts-companycode
          AND fiscalyear             EQ @lt_inserts-fiscalyear
          AND  monat                 EQ @lt_inserts-monat
        INTO TABLE @DATA(lt_docments).

    IF sy-subrc EQ 0.

      lt_updates = VALUE #( FOR i = 1 WHILE i LE lines( lt_inserts )
        LET
          ls_control  = VALUE #( lt_controls[ i ] OPTIONAL )
          ls_insert   = VALUE #( lt_inserts[ i ] OPTIONAL )
          ls_docment  = VALUE #( lt_docments[ monat = ls_insert-monat ] OPTIONAL )
          IN
            ( companycode            = ls_insert-companycode
              fiscalyear             = ls_insert-fiscalyear
              monat                  = ls_insert-monat

              Purchases              = ls_insert-Purchases
              Sales                  = ls_insert-Sales
              Export                 = ls_insert-Export
              Cancel                 = ls_insert-Cancel

*              Purchases              = COND #( WHEN ls_insert-Purchases IS NOT INITIAL
*                                               THEN ls_insert-Purchases
*                                               ELSE ls_docment-Purchases )
*
*              Sales                  = COND #( WHEN ls_insert-Sales IS NOT INITIAL
*                                               THEN ls_insert-Sales
*                                               ELSE ls_docment-Sales )
*
*              Export                 = COND #( WHEN ls_insert-Export IS NOT INITIAL
*                                               THEN ls_insert-Export
*                                               ELSE ls_docment-Export )
*
*              Cancel                 = COND #( WHEN ls_insert-Cancel IS NOT INITIAL
*                                               THEN ls_insert-Cancel
*                                               ELSE ls_docment-Cancel )

              Xml                    = COND #( WHEN ls_insert-Xml IS NOT INITIAL
                                               THEN ls_insert-Xml
                                               ELSE ls_docment-Xml )

              Filename               = COND #( WHEN ls_insert-Filename IS NOT INITIAL
                                               THEN ls_insert-Filename
                                               ELSE ls_docment-Filename )

              Mimetype               = COND #( WHEN ls_insert-Mimetype IS NOT INITIAL
                                               THEN ls_insert-Mimetype
                                               ELSE ls_docment-Mimetype )

              Status                 = COND #( WHEN ls_insert-Status IS NOT INITIAL
                                               THEN ls_insert-Status
                                               ELSE ls_docment-Status ) )
            ).

    ELSE.
      lt_updates = lt_inserts.
    ENDIF.

    UPDATE zdt_ec_021 FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.

    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_ec_021 WHERE companycode     EQ @<fs_keys>-companycode
                                 AND fiscalyear      EQ @<fs_keys>-fiscalyear
                                 AND monat           EQ @<fs_keys>-Monat.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    DATA:lv_companycode  TYPE bukrs,
         lv_FiscalYear  TYPE gjahr,
         lv_fiscalperiod  TYPE monat.

    READ TABLE keys INTO DATA(ls_keys) INDEX 1.
    IF sy-subrc EQ 0.

      lv_companycode  = ls_keys-Companycode.
      lv_FiscalYear   = ls_keys-FiscalYear.
      lv_fiscalperiod = ls_keys-Monat.

      SELECT * FROM zcds_rv_ec_021
          FOR ALL ENTRIES IN @keys
          WHERE companycode      = @keys-CompanyCode
            AND FiscalYear       = @keys-FiscalYear
            AND Monat            = @keys-Monat
            INTO CORRESPONDING FIELDS OF TABLE @result.

*      SELECT * FROM zcds_rv_ec_021( p_companycode      = @lv_CompanyCode,
*                                    p_FiscalYear       = @lv_FiscalYear,
*                                    p_fiscalperiod     = @lv_fiscalperiod )
*          FOR ALL ENTRIES IN @keys
*          WHERE companycode      = @keys-CompanyCode
*            AND FiscalYear       = @keys-FiscalYear
*            AND Monat            = @keys-Monat
*          into CORRESPONDING FIELDS OF table @result.

    ENDIF.

  ENDMETHOD.

  METHOD lock.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD create.

    DATA : lt_inserts   TYPE STANDARD TABLE OF zdt_ec_021,
           ls_cre_fac   TYPE STRUCTURE FOR CREATE zcds_rv_ec_021.

    IF entities IS NOT INITIAL.

      lt_inserts = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      INSERT zdt_ec_021 FROM TABLE @lt_inserts.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
