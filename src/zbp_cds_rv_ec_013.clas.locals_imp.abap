CLASS lhc_supportdetails DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS createSES FOR MODIFY
      IMPORTING keys FOR ACTION SupportDetails~createSES RESULT result.

ENDCLASS.

CLASS lhc_supportdetails IMPLEMENTATION.

  METHOD createSES.

    DATA: ls_ec_013       TYPE zdt_ec_013,
          ls_ec_012       TYPE zdt_ec_012.

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

*    result = VALUE #( FOR LiqSupport IN LiqSupports ( %key = LiqSupport-%key ) ).
    result = VALUE #( FOR DetSupport IN DetSupports ( %key = DetSupport-%key ) ).

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

      LOOP AT DetSupports INTO DATA(ls_DetSupport).
        MOVE-CORRESPONDING ls_DetSupport TO ls_ec_013.
        INSERT zdt_ec_013 FROM @ls_ec_013.
      ENDLOOP.

  ENDMETHOD.

ENDCLASS.
