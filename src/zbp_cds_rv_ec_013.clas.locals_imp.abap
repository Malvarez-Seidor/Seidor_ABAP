CLASS lhc_supportdetails DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

*    METHODS createSES FOR MODIFY
*      IMPORTING keys FOR ACTION SupportDetails~createSES RESULT result.

*    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
*      IMPORTING keys REQUEST requested_authorizations FOR SupportDetails RESULT result.

*    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
*      IMPORTING REQUEST requested_authorizations FOR SupportDetails RESULT result.

    METHODS precheck_update FOR PRECHECK
      IMPORTING entities FOR UPDATE SupportDetails.


ENDCLASS.

CLASS lhc_supportdetails IMPLEMENTATION.

*  METHOD createSES.
*
*    DATA: ls_ec_013       TYPE zdt_ec_013,
*          ls_ec_012       TYPE zdt_ec_012.
*
*    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
*      ENTITY LiquidationSupports
*      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype Filestatus Attachment  )
*      WITH CORRESPONDING #( keys )
*      RESULT DATA(LiqSupports)
*      FAILED failed.
*
*    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
*      ENTITY LiquidationSupports BY \_SupportDetails
*      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype
*               typeid idnumber documenttype establishment emissionpoint sequential
*               accesskey issuedate taxcode amountbasetax amountbasetax0 amountbasenotax
*               amountbaseexetax  amounttax amountice total_price currency  )
*      WITH CORRESPONDING #( keys )
*      RESULT DATA(DetSupports)
*      LINK DATA(link)
*      FAILED failed.
*
*    result = VALUE #( FOR DetSupport IN DetSupports ( %key = DetSupport-%key ) ).
*
*    LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>) WHERE ( Filestatus EQ space
*                                                                   OR   Filestatus EQ 'PENDING' ).
*
*        SELECT SINGLE *
*          FROM zdt_ec_012
*          WHERE companycode            EQ @<fs_LiqSupport>-Companycode
*            AND fiscalyear             EQ @<fs_LiqSupport>-Fiscalyear
*            AND accountingdocument     EQ @<fs_LiqSupport>-Accountingdocument
*            AND accountingdocumenttype EQ @<fs_LiqSupport>-Accountingdocumenttype
*           INTO @ls_ec_012.
*
*        IF sy-subrc EQ 0.
*
*          MOVE-CORRESPONDING <fs_LiqSupport> TO ls_ec_012.
*          <fs_LiqSupport>-filestatus = ls_ec_012-filestatus = 'COMPLETE'.
*          UPDATE zdt_ec_012 FROM @ls_ec_012.
*
*          DELETE FROM zdt_ec_013 WHERE companycode EQ @<fs_LiqSupport>-Companycode
*                      AND fiscalyear               EQ @<fs_LiqSupport>-Fiscalyear
*                      AND accountingdocument       EQ @<fs_LiqSupport>-Accountingdocument
*                      AND accountingdocumenttype   EQ @<fs_LiqSupport>-Accountingdocumenttype.
*        ENDIF.
*
*      ENDLOOP.
*
*      LOOP AT DetSupports INTO DATA(ls_DetSupport).
*        MOVE-CORRESPONDING ls_DetSupport TO ls_ec_013.
*        INSERT zdt_ec_013 FROM @ls_ec_013.
*      ENDLOOP.
*
*  ENDMETHOD.
*
*  METHOD get_instance_authorizations.
*  ENDMETHOD.
*
*  METHOD get_global_authorizations.
*  ENDMETHOD.

  METHOD precheck_update.

    DATA: ls_ec_012 TYPE zdt_ec_012.

    DATA: lv_navnw TYPE navnw.

    CLEAR: lv_navnw.

    DATA: update_requested TYPE abap_bool,
           update_grtanted TYPE abap_bool.

    READ ENTITIES OF zcds_rv_ec_012 IN LOCAL MODE
      ENTITY LiquidationSupports
      FIELDS ( Companycode Fiscalyear Accountingdocument Accountingdocumenttype AmountInCompanyCodeCurrency Filestatus Attachment )
      WITH CORRESPONDING #( entities )
      RESULT DATA(LiqSupports)
      FAILED failed.

    CHECK LiqSupports is not initial.

    LOOP AT LiqSupports ASSIGNING FIELD-SYMBOL(<fs_LiqSupport>).

      IF <fs_LiqSupport>-Filestatus = 'COMPLETE'.

        IF update_grtanted = abap_false.

          APPEND VALUE #(  %tky = <fs_LiqSupport>-%tky ) TO failed-liquidationsupports.
          APPEND VALUE #(  %tky = <fs_LiqSupport>-%tky
                           %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                           text = 'No Authorization to update status!!!'
                                )
              ) TO reported-liquidationsupports.

        ENDIF.

      ENDIF.

    ENDLOOP.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<fs_entities>).

      lv_navnw += <fs_entities>-amountbasenotax.
      lv_navnw += <fs_entities>-amountbaseexetax.
      lv_navnw += <fs_entities>-amountbasetax.
      lv_navnw += <fs_entities>-amountbasetax0.
      lv_navnw += <fs_entities>-amountice.
      lv_navnw += <fs_entities>-amounttax.

    ENDLOOP.

    SELECT SINGLE *
      FROM zdt_ec_012
      WHERE companycode            EQ @<fs_entities>-Companycode
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

    IF LiqSupports[] IS NOT INITIAL.

      READ TABLE LiqSupports INTO DATA(ls_LiquidationSupports) INDEX 1.

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

  ENDMETHOD.

ENDCLASS.
