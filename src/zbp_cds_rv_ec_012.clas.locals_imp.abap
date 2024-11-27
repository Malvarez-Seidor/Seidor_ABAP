CLASS lhc_LiquidationSupports DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR LiquidationSupports RESULT result.

    METHODS fields FOR DETERMINE ON MODIFY
      IMPORTING keys FOR liquidationsupports~fields.

    METHODS uploadexceldata FOR MODIFY
      IMPORTING keys FOR ACTION liquidationsupports~uploadexceldata RESULT result.

ENDCLASS.

CLASS lhc_LiquidationSupports IMPLEMENTATION.

  METHOD get_instance_features.

    DATA: ls_ec_012 TYPE zdt_ec_012.

    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Filestatus Attachment  )
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

    result = VALUE #( FOR LiqSupport IN LiqSupports ( %key = LiqSupport-%key
                                               %features-%action-uploadexceldata = COND #( WHEN LiqSupport-%data-Filestatus = 'COMPLETE'
                                                                                           THEN if_abap_behv=>fc-f-read_only
                                                                                           ELSE if_abap_behv=>fc-f-unrestricted ) ) ).


    result = VALUE #( FOR DetSupport IN DetSupports ( %key = DetSupport-%key ) ).

    LOOP AT LiqSupports INTO DATA(ls_LiqSupport) WHERE Filestatus IS INITIAL
                                                    OR Filestatus EQ 'PENDING'.

      SELECT SINGLE *
         FROM zdt_ec_012
         WHERE companycode            EQ @ls_LiqSupport-Companycode
           AND fiscalyear             EQ @ls_LiqSupport-Fiscalyear
           AND accountingdocument     EQ @ls_LiqSupport-Accountingdocument
           AND accountingdocumenttype EQ @ls_LiqSupport-Accountingdocumenttype
         INTO @ls_ec_012.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING ls_LiqSupport TO ls_ec_012.
        INSERT zdt_ec_012 FROM @ls_ec_012.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD fields.

    DATA: ls_ec_012 TYPE zdt_ec_012,
          ls_ec_013 TYPE zdt_ec_013.

*    MODIFY ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
*    ENTITY LiquidationSupports
*    UPDATE FROM VALUE #( FOR key IN keys ( Companycode            = key-Companycode
*                                           Fiscalyear             = key-Fiscalyear
*                                           Accountingdocument     = key-Accountingdocument
*                                           Accountingdocumenttype = key-Accountingdocumenttype
*                                           Filestatus             = 'PENDING' " Accepted
*                                           %control-Filestatus    = if_abap_behv=>mk-on ) ).


    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(LiqSupports).

    LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>) WHERE Attachment IS NOT INITIAL
                                                                  AND ( Filestatus EQ space
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

        SELECT SINGLE *
         FROM zdt_ec_013
         WHERE companycode            EQ @<fs_LiqSupport>-Companycode
           AND fiscalyear             EQ @<fs_LiqSupport>-Fiscalyear
           AND accountingdocument     EQ @<fs_LiqSupport>-Accountingdocument
           AND accountingdocumenttype EQ @<fs_LiqSupport>-Accountingdocumenttype
          INTO @ls_ec_013.

        IF sy-subrc EQ 0.
          <fs_LiqSupport>-Filestatus = ls_ec_012-filestatus = 'COMPLETE'.
        ENDIF.
        UPDATE zdt_ec_012 FROM @ls_ec_012.

      ENDIF.

    ENDLOOP.

    MODIFY ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      EXECUTE uploadexceldata
      FROM CORRESPONDING #( keys ).

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
          lv_issuedate        TYPE char10,
          lv_amountbasetax    TYPE char10,
          lv_amountbasetax0   TYPE char10,
          lv_amountbasenotax  TYPE char10,
          lv_amountbaseexetax TYPE char10,
          lv_amounttax        TYPE char10,
          lv_amountice        TYPE char10,
          lv_totalprice       TYPE char10.

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
      ls_excel_data-documentitem            = lv_item.

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

ENDCLASS.
