CLASS lhc_LiquidationSupports DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    DATA update_allowed TYPE abap_bool.


    METHODS uploadexceldata FOR MODIFY
      IMPORTING keys FOR ACTION liquidationsupports~uploadexceldata RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR LiquidationSupports RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR liquidationsupports RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR liquidationsupports RESULT result.

    METHODS precheck_cba_supportdetails FOR PRECHECK
      IMPORTING entities FOR CREATE liquidationsupports\_supportdetails.

    METHODS fields FOR DETERMINE ON MODIFY
      IMPORTING keys FOR liquidationsupports~fields.
    METHODS carga FOR VALIDATE ON SAVE
      IMPORTING keys FOR liquidationsupports~carga.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE liquidationsupports.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.


ENDCLASS.

CLASS lhc_LiquidationSupports IMPLEMENTATION.

  METHOD get_instance_features.

    DATA: ls_ec_012 TYPE zdt_ec_012.

    DATA: lv_navnw  TYPE navnw.

    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Supplier Filestatus Attachment Mimetype
               AmountInCompanyCodeCurrency Last_changed_by
               BusinessPartnerFullName Criticality CompanyCodeName AccountingDocumentTypeName CompanyCodeCurrency UserFullName )
      WITH CORRESPONDING #( keys )
      RESULT DATA(LiqSupports)
      FAILED failed.

    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports BY \_SupportDetails
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype
               typeid idnumber documenttype establishment emissionpoint sequential
               accesskey issuedate taxcode amountbasetax amountbasetax0 amountbasenotax
               amountbaseexetax  amounttax amountice total_price currency  )
      WITH CORRESPONDING #( keys )
      RESULT DATA(DetSupports)
      LINK DATA(link)
      FAILED failed.


    LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>) WHERE Filestatus IS INITIAL
                                                    OR Filestatus EQ 'PENDING'.

      SELECT SINGLE *
         FROM zdt_ec_012
         WHERE companycode            EQ @<fs_LiqSupport>-Companycode
           AND fiscalyear             EQ @<fs_LiqSupport>-Fiscalyear
           AND accountingdocument     EQ @<fs_LiqSupport>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_LiqSupport>-Accountingdocumenttype
         INTO @ls_ec_012.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING <fs_LiqSupport> TO ls_ec_012.
        INSERT zdt_ec_012 FROM @ls_ec_012.
      ENDIF.

      CLEAR: lv_navnw.

      SELECT *
      FROM zdt_ec_013
      WHERE companycode             EQ @<fs_LiqSupport>-companycode
        AND fiscalyear              EQ @<fs_LiqSupport>-fiscalyear
        AND accountingdocument      EQ @<fs_LiqSupport>-accountingdocument
        AND accountingdocumenttype  EQ @<fs_LiqSupport>-accountingdocumenttype
      INTO TABLE @DATA(lt_docments).

      LOOP AT lt_docments INTO DATA(ls_docments).

        lv_navnw += ls_docments-amountbasenotax.
        lv_navnw += ls_docments-amountbaseexetax.
        lv_navnw += ls_docments-amountbasetax.
        lv_navnw += ls_docments-amountbasetax0.
        lv_navnw += ls_docments-amountice.
        lv_navnw += ls_docments-amounttax.

      ENDLOOP.

      IF <fs_LiqSupport>-AmountInCompanyCodeCurrency EQ lv_navnw.

        IF ls_ec_012 IS NOT INITIAL.

          <fs_LiqSupport>-filestatus = ls_ec_012-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
          UPDATE zdt_ec_012 FROM @ls_ec_012.

        ENDIF.

      ENDIF.

    ENDLOOP.

    result = VALUE #( FOR LiqSupport IN LiqSupports ( %key = LiqSupport-%key
                                               %features-%action-uploadExcelData = COND #( WHEN LiqSupport-%data-Filestatus = 'COMPLETE'
                                                                                           THEN if_abap_behv=>fc-o-disabled
                                                                                           ELSE if_abap_behv=>fc-o-enabled ) ) ).

  ENDMETHOD.

  METHOD uploadExcelData.

    DATA: ls_ec_012 TYPE zdt_ec_012.

** Data declarations
    DATA: rows                TYPE STANDARD TABLE OF string,
          lv_string           TYPE string,
          ls_excel_data       TYPE zdt_ec_013,
          lt_excel_data       TYPE STANDARD TABLE OF zdt_ec_013,
          lv_raw              TYPE xstring,
          lv_item             TYPE i,
          lv_navnw            TYPE navnw,
          lv_issuedate        TYPE c LENGTH 10,
          lv_amountbasetax    TYPE c LENGTH 16,
          lv_amountbasetax0   TYPE c LENGTH 16,
          lv_amountbasenotax  TYPE c LENGTH 16,
          lv_amountbaseexetax TYPE c LENGTH 16,
          lv_amounttax        TYPE c LENGTH 16,
          lv_amountice        TYPE c LENGTH 16,
          lv_totalprice       TYPE c LENGTH 16.

** Read the parent instance
    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(lt_inv).

    READ TABLE lt_inv INTO DATA(ls_inv) INDEX 1.

** Get attachment value from the instance
    DATA(lv_attachment) = ls_inv-attachment.

    lv_raw = lv_attachment.

** Convert excel file with CSV format into internal table of type string
    lv_string = cl_abap_conv_codepage=>create_in( )->convert( lv_raw ).

** Split the string table to rows
    SPLIT lv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rows.

    IF lv_string IS INITIAL.

      DATA(lo_msg) = new_message( id = 'ZMC_DOC_ELEC'            "id = Name Of message class
                              number = '006'                     "number of message defined in the message class
                            severity = cl_abap_behv=>ms-error ). "type of message

      APPEND VALUE #(  %tky = ls_inv-%tky ) TO failed-liquidationsupports.

      APPEND VALUE #(  %tky        = ls_inv-%tky
                       %state_area = 'VALIDATE_MONTO'
                       %msg        =  lo_msg )
      TO reported-liquidationsupports.

    ENDIF.

    CHECK lv_string IS NOT INITIAL.

** Process the rows and append to the internal table
    LOOP AT rows INTO DATA(ls_row).
      IF sy-tabix EQ 1.
        CONTINUE.
      ENDIF.

      lv_item += 1.
      ls_excel_data-companycode             = ls_inv-Companycode.
      ls_excel_data-accountingdocument      = ls_inv-Accountingdocument.
      ls_excel_data-accountingdocumenttype  = ls_inv-Accountingdocumenttype.
      ls_excel_data-fiscalyear              = ls_inv-Fiscalyear.
      ls_excel_data-currency                = ls_inv-CompanyCodeCurrency.
      ls_excel_data-draftuuid               = lv_item.

      SPLIT ls_row AT ';' INTO ls_excel_data-typeid
                               ls_excel_data-idnumber
                               ls_excel_data-documenttype
                               ls_excel_data-establishment
                               ls_excel_data-emissionpoint
                               ls_excel_data-sequential
                               ls_excel_data-accesskey
                               lv_issuedate
                               ls_excel_data-taxcode
                               lv_amountbasetax0
                               lv_amountbasetax
                               lv_amounttax
                               lv_amountbasenotax
                               lv_amountbaseexetax
                               lv_amountice
                               lv_totalprice.

      REPLACE ALL OCCURRENCES OF '.' IN lv_issuedate WITH space.
      REPLACE ALL OCCURRENCES OF '-' IN lv_issuedate WITH space.
      REPLACE ALL OCCURRENCES OF '/' IN lv_issuedate WITH space.
      CONDENSE lv_issuedate.

      IF lv_issuedate IS NOT INITIAL.
        CONCATENATE lv_issuedate+4(4) lv_issuedate+2(2) lv_issuedate(2) INTO ls_excel_data-issuedate.
      ENDIF.

      IF lv_amountbasetax CS ',' AND lv_amountbasetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ELSEIF lv_amountbasetax CS ',' AND lv_amountbasetax CS '.'.
        IF lv_amountbasetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH space.
        ELSEIF lv_amountbasetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ENDIF.

      IF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ELSEIF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 CS '.'.
        IF lv_amountbasetax0 CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH space.
        ELSEIF lv_amountbasetax0 CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax0 WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ENDIF.

      IF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ELSEIF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax CS '.'.
        IF lv_amountbaseexetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH space.
        ELSEIF lv_amountbaseexetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbaseexetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ENDIF.

      IF lv_amountbasenotax CS ',' AND lv_amountbasenotax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ELSEIF lv_amountbasenotax CS ',' AND lv_amountbasenotax CS '.'.
        IF lv_amountbasenotax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH space.
        ELSEIF lv_amountbasenotax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasenotax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ENDIF.

      IF lv_amounttax CS ',' AND lv_amounttax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ELSEIF lv_amounttax CS ',' AND lv_amounttax CS '.'.
        IF lv_amounttax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH space.
        ELSEIF lv_amounttax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amounttax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ENDIF.

      IF lv_amountice CS ',' AND lv_amountice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ELSEIF lv_amountice CS ',' AND lv_amountice CS '.'.
        IF lv_amountice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH space.
        ELSEIF lv_amountice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ENDIF.

      IF lv_totalprice CS ',' AND lv_totalprice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ELSEIF lv_totalprice CS ',' AND lv_totalprice CS '.'.
        IF lv_totalprice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH space.
        ELSEIF lv_totalprice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_totalprice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ENDIF.

      ls_excel_data-amountbasetax     = lv_amountbasetax.
      ls_excel_data-amountbasetax0    = lv_amountbasetax0.
      ls_excel_data-amountbaseexetax  = lv_amountbaseexetax.
      ls_excel_data-amountbasenotax   = lv_amountbasenotax.
      ls_excel_data-amounttax         = lv_amounttax.
      ls_excel_data-amountice         = lv_amountice.
      ls_excel_data-total_price       = lv_totalprice.

      lv_navnw += ls_excel_data-amountice       + ls_excel_data-amounttax        +
                  ls_excel_data-amountbasenotax + ls_excel_data-amountbaseexetax +
                  ls_excel_data-amountbasetax0  + ls_excel_data-amountbasetax.

      APPEND ls_excel_data TO lt_excel_data.

      CLEAR: ls_row, ls_excel_data.
    ENDLOOP.

    IF lv_navnw NE ls_inv-AmountInCompanyCodeCurrency.

      lo_msg = new_message( id = 'ZMC_DOC_ELEC'                  "id = Name Of message class
                        number = '005'                           "number of message defined in the message class
                      severity = cl_abap_behv=>ms-error
                      v1 = lv_navnw
                      v2 = ls_inv-AmountInCompanyCodeCurrency ). "type of message

      APPEND VALUE #(  %tky = ls_inv-%tky ) TO failed-liquidationsupports.

      APPEND VALUE #(  %tky        = ls_inv-%tky
                       %state_area = 'VALIDATE_MONTO'
                       %msg        =  lo_msg )
      TO reported-liquidationsupports.

    ELSE.

     READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
       ENTITY LiquidationSupports
       ALL FIELDS WITH
       CORRESPONDING #( keys )
       RESULT DATA(LiqSupports).

      LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>) WHERE ( Filestatus EQ space
                                                                     OR   Filestatus EQ 'PENDING' ).

        SELECT SINGLE *
          FROM zdt_ec_012
          WHERE companycode            EQ @<fs_LiqSupport>-Companycode
            AND fiscalyear             EQ @<fs_LiqSupport>-Fiscalyear
            AND accountingdocument     EQ @<fs_LiqSupport>-Accountingdocument
            AND accountingdocumenttype EQ @<fs_LiqSupport>-Accountingdocumenttype
           INTO @ls_ec_012.

        IF sy-subrc EQ 0.

          MOVE-CORRESPONDING <fs_LiqSupport> TO ls_ec_012.
          <fs_LiqSupport>-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
          UPDATE zdt_ec_012 FROM @ls_ec_012.

          DELETE FROM zdt_ec_013 WHERE companycode EQ @<fs_LiqSupport>-Companycode
                      AND fiscalyear               EQ @<fs_LiqSupport>-Fiscalyear
                      AND accountingdocument       EQ @<fs_LiqSupport>-Accountingdocument
                      AND accountingdocumenttype   EQ @<fs_LiqSupport>-Accountingdocumenttype.
        ENDIF.

      ENDLOOP.

      LOOP AT lt_excel_data INTO ls_excel_data.
        INSERT zdt_ec_013 FROM @ls_excel_data.
      ENDLOOP.

      result = VALUE #( FOR LiqSupport IN LiqSupports ( %tky   = LiqSupport-%tky
                                                        %param = LiqSupport ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

  METHOD earlynumbering_create.

  ENDMETHOD.

  METHOD precheck_cba_Supportdetails.

    DATA: ls_ec_012 TYPE zdt_ec_012.

    DATA: lv_navnw TYPE navnw.

    DATA: update_requested TYPE abap_bool,
          update_grtanted  TYPE abap_bool.

    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Filestatus Attachment )
      WITH CORRESPONDING #( entities )
      RESULT DATA(LiqSupports)
      FAILED failed.

    CHECK LiqSupports is not initial.

    LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>).

      IF <fs_LiqSupport>-Filestatus = 'COMPLETE'.

        IF update_requested = abap_true.
           update_grtanted = is_update_allowed(  ).

          IF update_grtanted = abap_false.
            APPEND VALUE #(  %tky = <fs_LiqSupport>-%tky ) TO failed-liquidationsupports.
            APPEND VALUE #(  %tky = entities[ 1 ]-%tky
                             %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                             text = 'No Authorization to create status!!!'
                                )
               ) TO reported-liquidationsupports.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<fs_entities>).

      CLEAR: lv_navnw.

      LOOP AT <fs_entities>-%target ASSIGNING FIELD-SYMBOL(<fs_target>) .

        lv_navnw += <fs_target>-amountbasenotax.
        lv_navnw += <fs_target>-amountbaseexetax.
        lv_navnw += <fs_target>-amountbasetax.
        lv_navnw += <fs_target>-amountbasetax0.
        lv_navnw += <fs_target>-amountice.
        lv_navnw += <fs_target>-amounttax.

      ENDLOOP.

      SELECT SINGLE *
         FROM zdt_ec_012
         WHERE companycode           EQ @<fs_entities>-Companycode
          AND fiscalyear             EQ @<fs_entities>-Fiscalyear
          AND accountingdocument     EQ @<fs_entities>-Accountingdocument
          AND accountingdocumenttype EQ @<fs_entities>-Accountingdocumenttype
         INTO @ls_ec_012.

      SELECT *
      FROM zdt_ec_013
      WHERE companycode             EQ @<fs_entities>-companycode
        AND fiscalyear              EQ @<fs_entities>-fiscalyear
        AND accountingdocument      EQ @<fs_entities>-accountingdocument
        AND accountingdocumenttype  EQ @<fs_entities>-accountingdocumenttype
      INTO TABLE @DATA(lt_docments).

      LOOP AT lt_docments INTO DATA(ls_docments).

        lv_navnw += ls_docments-amountbasenotax.
        lv_navnw += ls_docments-amountbaseexetax.
        lv_navnw += ls_docments-amountbasetax.
        lv_navnw += ls_docments-amountbasetax0.
        lv_navnw += ls_docments-amountice.
        lv_navnw += ls_docments-amounttax.

      ENDLOOP.

      READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
        ENTITY LiquidationSupports
        FIELDS ( CompanyCode FiscalYear AccountingDocument AccountingDocumentType AccountingDocumentTypeName
                 Supplier BusinessPartnerFullName Attachment Filename Filestatus Mimetype AmountInCompanyCodeCurrency
                 AccountingDocCreatedByUser UserFullName CompanyCodeCurrency CompanyCodeName Criticality Last_changed_by )
        WITH VALUE #(  (  %key = <fs_entities>-%key ) )
        RESULT DATA(LiqSupport).

      IF sy-subrc EQ 0.

        READ TABLE LiqSupport INTO DATA(ls_LiquidationSupports) INDEX 1.

        IF ls_LiquidationSupports-amountincompanycodecurrency EQ lv_navnw.

          IF ls_ec_012 IS NOT INITIAL.

            ls_ec_012-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
            UPDATE zdt_ec_012 FROM @ls_ec_012.

          ELSE.

            MOVE-CORRESPONDING: ls_LiquidationSupports TO  ls_ec_012.
            ls_ec_012-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
            INSERT zdt_ec_012 FROM @ls_ec_012.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD fields.

    DATA: ls_ec_012 TYPE zdt_ec_012.

** Data declarations
    DATA: rows                TYPE STANDARD TABLE OF string,
          lv_string           TYPE string,
          ls_excel_data       TYPE zdt_ec_013,
          lt_excel_data       TYPE STANDARD TABLE OF zdt_ec_013,
          lv_raw              TYPE xstring,
          lv_item             TYPE i,
          lv_navnw            TYPE navnw,
          lv_issuedate        TYPE c LENGTH 10,
          lv_amountbasetax    TYPE c LENGTH 16,
          lv_amountbasetax0   TYPE c LENGTH 16,
          lv_amountbasenotax  TYPE c LENGTH 16,
          lv_amountbaseexetax TYPE c LENGTH 16,
          lv_amounttax        TYPE c LENGTH 16,
          lv_amountice        TYPE c LENGTH 16,
          lv_totalprice       TYPE c LENGTH 16.

** Read the parent instance
    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(lt_inv).

    READ TABLE lt_inv INTO DATA(ls_inv) INDEX 1.

** Get attachment value from the instance
    DATA(lv_attachment) = ls_inv-attachment.

    lv_raw = lv_attachment.

** Convert excel file with CSV format into internal table of type string
    lv_string = cl_abap_conv_codepage=>create_in( )->convert( lv_raw ).

** Split the string table to rows
    SPLIT lv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rows.

    CHECK lv_string IS NOT INITIAL.

** Process the rows and append to the internal table
    LOOP AT rows INTO DATA(ls_row).
      IF sy-tabix EQ 1.
        CONTINUE.
      ENDIF.

      lv_item += 1.
      ls_excel_data-companycode             = ls_inv-Companycode.
      ls_excel_data-accountingdocument      = ls_inv-Accountingdocument.
      ls_excel_data-accountingdocumenttype  = ls_inv-Accountingdocumenttype.
      ls_excel_data-fiscalyear              = ls_inv-Fiscalyear.
      ls_excel_data-currency                = ls_inv-CompanyCodeCurrency.
      ls_excel_data-draftuuid               = lv_item.

      SPLIT ls_row AT ';' INTO ls_excel_data-typeid
                               ls_excel_data-idnumber
                               ls_excel_data-documenttype
                               ls_excel_data-establishment
                               ls_excel_data-emissionpoint
                               ls_excel_data-sequential
                               ls_excel_data-accesskey
                               lv_issuedate
                               ls_excel_data-taxcode
                               lv_amountbasetax0
                               lv_amountbasetax
                               lv_amounttax
                               lv_amountbasenotax
                               lv_amountbaseexetax
                               lv_amountice
                               lv_totalprice.

      REPLACE ALL OCCURRENCES OF '.' IN lv_issuedate WITH space.
      REPLACE ALL OCCURRENCES OF '-' IN lv_issuedate WITH space.
      REPLACE ALL OCCURRENCES OF '/' IN lv_issuedate WITH space.
      CONDENSE lv_issuedate.

      IF lv_issuedate IS NOT INITIAL.
        CONCATENATE lv_issuedate+4(4) lv_issuedate+2(2) lv_issuedate(2) INTO ls_excel_data-issuedate.
      ENDIF.

      IF lv_amountbasetax CS ',' AND lv_amountbasetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ELSEIF lv_amountbasetax CS ',' AND lv_amountbasetax CS '.'.
        IF lv_amountbasetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH space.
        ELSEIF lv_amountbasetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ENDIF.

      IF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ELSEIF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 CS '.'.
        IF lv_amountbasetax0 CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH space.
        ELSEIF lv_amountbasetax0 CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax0 WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ENDIF.

      IF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ELSEIF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax CS '.'.
        IF lv_amountbaseexetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH space.
        ELSEIF lv_amountbaseexetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbaseexetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ENDIF.

      IF lv_amountbasenotax CS ',' AND lv_amountbasenotax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ELSEIF lv_amountbasenotax CS ',' AND lv_amountbasenotax CS '.'.
        IF lv_amountbasenotax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH space.
        ELSEIF lv_amountbasenotax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasenotax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ENDIF.

      IF lv_amounttax CS ',' AND lv_amounttax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ELSEIF lv_amounttax CS ',' AND lv_amounttax CS '.'.
        IF lv_amounttax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH space.
        ELSEIF lv_amounttax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amounttax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ENDIF.

      IF lv_amountice CS ',' AND lv_amountice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ELSEIF lv_amountice CS ',' AND lv_amountice CS '.'.
        IF lv_amountice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH space.
        ELSEIF lv_amountice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ENDIF.

      IF lv_totalprice CS ',' AND lv_totalprice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ELSEIF lv_totalprice CS ',' AND lv_totalprice CS '.'.
        IF lv_totalprice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH space.
        ELSEIF lv_totalprice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_totalprice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ENDIF.

      ls_excel_data-amountbasetax     = lv_amountbasetax.
      ls_excel_data-amountbasetax0    = lv_amountbasetax0.
      ls_excel_data-amountbaseexetax  = lv_amountbaseexetax.
      ls_excel_data-amountbasenotax   = lv_amountbasenotax.
      ls_excel_data-amounttax         = lv_amounttax.
      ls_excel_data-amountice         = lv_amountice.
      ls_excel_data-total_price       = lv_totalprice.

      lv_navnw += ls_excel_data-amountice       + ls_excel_data-amounttax        +
                  ls_excel_data-amountbasenotax + ls_excel_data-amountbaseexetax +
                  ls_excel_data-amountbasetax0  + ls_excel_data-amountbasetax.

      APPEND ls_excel_data TO lt_excel_data.

      CLEAR: ls_row, ls_excel_data.
    ENDLOOP.

    IF lv_navnw EQ ls_inv-AmountInCompanyCodeCurrency.

     READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
       ENTITY LiquidationSupports
       ALL FIELDS WITH
       CORRESPONDING #( keys )
       RESULT DATA(LiqSupports).

      LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>) WHERE ( Filestatus EQ space
                                                                     OR   Filestatus EQ 'PENDING' ).

        SELECT SINGLE *
          FROM zdt_ec_012
          WHERE companycode            EQ @<fs_LiqSupport>-Companycode
            AND fiscalyear             EQ @<fs_LiqSupport>-Fiscalyear
            AND accountingdocument     EQ @<fs_LiqSupport>-Accountingdocument
            AND accountingdocumenttype EQ @<fs_LiqSupport>-Accountingdocumenttype
           INTO @ls_ec_012.

        IF sy-subrc EQ 0.

          MOVE-CORRESPONDING <fs_LiqSupport> TO ls_ec_012.
          <fs_LiqSupport>-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
          UPDATE zdt_ec_012 FROM @ls_ec_012.

          DELETE FROM zdt_ec_013 WHERE companycode EQ @<fs_LiqSupport>-Companycode
                      AND fiscalyear               EQ @<fs_LiqSupport>-Fiscalyear
                      AND accountingdocument       EQ @<fs_LiqSupport>-Accountingdocument
                      AND accountingdocumenttype   EQ @<fs_LiqSupport>-Accountingdocumenttype.
        ENDIF.

      ENDLOOP.

      LOOP AT lt_excel_data INTO ls_excel_data.
        INSERT zdt_ec_013 FROM @ls_excel_data.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD Carga.

    DATA: ls_ec_012 TYPE zdt_ec_012.

** Data declarations
    DATA: rows                TYPE STANDARD TABLE OF string,
          lv_string           TYPE string,
          ls_excel_data       TYPE zdt_ec_013,
          lt_excel_data       TYPE STANDARD TABLE OF zdt_ec_013,
          lv_raw              TYPE xstring,
          lv_item             TYPE i,
          lv_navnw            TYPE navnw,
          lv_issuedate        TYPE c LENGTH 10,
          lv_amountbasetax    TYPE c LENGTH 16,
          lv_amountbasetax0   TYPE c LENGTH 16,
          lv_amountbasenotax  TYPE c LENGTH 16,
          lv_amountbaseexetax TYPE c LENGTH 16,
          lv_amounttax        TYPE c LENGTH 16,
          lv_amountice        TYPE c LENGTH 16,
          lv_totalprice       TYPE c LENGTH 16.

** Read the parent instance
    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(lt_inv).

    READ TABLE lt_inv INTO DATA(ls_inv) INDEX 1.

** Get attachment value from the instance
    DATA(lv_attachment) = ls_inv-attachment.

    lv_raw = lv_attachment.

** Convert excel file with CSV format into internal table of type string
    lv_string = cl_abap_conv_codepage=>create_in( )->convert( lv_raw ).

** Split the string table to rows
    SPLIT lv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rows.

    CHECK lv_string IS NOT INITIAL.

** Process the rows and append to the internal table
    LOOP AT rows INTO DATA(ls_row).

      IF sy-tabix EQ 1.
        CONTINUE.
      ENDIF.

      SPLIT ls_row AT ';' INTO ls_excel_data-typeid
                               ls_excel_data-idnumber
                               ls_excel_data-documenttype
                               ls_excel_data-establishment
                               ls_excel_data-emissionpoint
                               ls_excel_data-sequential
                               ls_excel_data-accesskey
                               lv_issuedate
                               ls_excel_data-taxcode
                               lv_amountbasetax0
                               lv_amountbasetax
                               lv_amounttax
                               lv_amountbasenotax
                               lv_amountbaseexetax
                               lv_amountice
                               lv_totalprice.

      IF lv_amountbasetax CS ',' AND lv_amountbasetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ELSEIF lv_amountbasetax CS ',' AND lv_amountbasetax CS '.'.
        IF lv_amountbasetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH space.
        ELSEIF lv_amountbasetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax WITH '.'.
      ENDIF.

      IF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ELSEIF lv_amountbasetax0 CS ',' AND lv_amountbasetax0 CS '.'.
        IF lv_amountbasetax0 CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH space.
        ELSEIF lv_amountbasetax0 CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasetax0 WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasetax0 WITH '.'.
      ENDIF.

      IF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ELSEIF lv_amountbaseexetax CS ',' AND lv_amountbaseexetax CS '.'.
        IF lv_amountbaseexetax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH space.
        ELSEIF lv_amountbaseexetax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbaseexetax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbaseexetax WITH '.'.
      ENDIF.

      IF lv_amountbasenotax CS ',' AND lv_amountbasenotax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ELSEIF lv_amountbasenotax CS ',' AND lv_amountbasenotax CS '.'.
        IF lv_amountbasenotax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH space.
        ELSEIF lv_amountbasenotax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountbasenotax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountbasenotax WITH '.'.
      ENDIF.

      IF lv_amounttax CS ',' AND lv_amounttax NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ELSEIF lv_amounttax CS ',' AND lv_amounttax CS '.'.
        IF lv_amounttax CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH space.
        ELSEIF lv_amounttax CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amounttax WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amounttax WITH '.'.
      ENDIF.

      IF lv_amountice CS ',' AND lv_amountice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ELSEIF lv_amountice CS ',' AND lv_amountice CS '.'.
        IF lv_amountice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH space.
        ELSEIF lv_amountice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_amountice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_amountice WITH '.'.
      ENDIF.

      IF lv_totalprice CS ',' AND lv_totalprice NS '.'.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ELSEIF lv_totalprice CS ',' AND lv_totalprice CS '.'.
        IF lv_totalprice CP '*.++' .
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH space.
        ELSEIF lv_totalprice CP '*,++'.
          REPLACE ALL OCCURRENCES OF '.' IN lv_totalprice WITH space.
          REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv_totalprice WITH '.'.
      ENDIF.

      ls_excel_data-amountbasetax     = lv_amountbasetax.
      ls_excel_data-amountbasetax0    = lv_amountbasetax0.
      ls_excel_data-amountbaseexetax  = lv_amountbaseexetax.
      ls_excel_data-amountbasenotax   = lv_amountbasenotax.
      ls_excel_data-amounttax         = lv_amounttax.
      ls_excel_data-amountice         = lv_amountice.
      ls_excel_data-total_price       = lv_totalprice.

      lv_navnw += ls_excel_data-amountice       + ls_excel_data-amounttax        +
                  ls_excel_data-amountbasenotax + ls_excel_data-amountbaseexetax +
                  ls_excel_data-amountbasetax0  + ls_excel_data-amountbasetax.

    ENDLOOP.

    IF lv_navnw NE ls_inv-AmountInCompanyCodeCurrency.


      DATA(lo_msg) = new_message(
                                 id = 'ZMC_DOC_ELEC'                   "id = Name Of message class
                              number = '005'                           "number of message defined in the message class
                            severity = cl_abap_behv=>ms-error          "type of message
                                  v1 = lv_navnw
                                  v2 = ls_inv-AmountInCompanyCodeCurrency
                                  ).

      APPEND VALUE #(  %tky              = ls_inv-%tky
                       %state_area       = 'VALIDATE_CARGA'
*                       %msg              = lo_msg
                       %msg              = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Dummy message' )
                       %element-FileName = if_abap_behv=>mk-on )
      TO reported-liquidationsupports.

      APPEND VALUE #(  %tky              = ls_inv-%tky
                        ) TO failed-liquidationsupports.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
