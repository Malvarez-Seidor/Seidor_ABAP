CLASS lhc_SeqAdministrator DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR SeqAdministrator RESULT result.

    METHODS getNext FOR MODIFY
      IMPORTING keys FOR ACTION SeqAdministrator~getNext RESULT result.

ENDCLASS.

CLASS lhc_SeqAdministrator IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_ec_002  IN LOCAL MODE
      ENTITY SeqAdministrator
      FIELDS ( Companycode Documentsri Emissionpoint Establishment Objet Address Sequential )
      WITH CORRESPONDING #( keys )
      RESULT DATA(OnlinesSeq)
      FAILED failed.

    result = VALUE #( FOR OnlineSeq IN OnlinesSeq
                    ( %tky = OnlineSeq-%tky
                      %features-%action-getNext "= if_abap_behv=>fc-o-enabled ) ).
          = COND #( WHEN OnlineSeq-Objet IS NOT INITIAL THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled ) ) ).

  ENDMETHOD.

  METHOD getNext.

    READ ENTITIES OF zcds_rv_ec_002  IN LOCAL MODE
      ENTITY SeqAdministrator
      FIELDS ( Companycode Documentsri Establishment Emissionpoint Objet Address Sequential )
      WITH CORRESPONDING #( keys )
      RESULT DATA(OnlineSeq).

    LOOP AT OnlineSeq ASSIGNING FIELD-SYMBOL(<OnlineSeq>).

        TRY.
          CALL METHOD cl_numberrange_runtime=>number_get
            EXPORTING
              nr_range_nr = '01'
              object      = <OnlineSeq>-Objet
            IMPORTING
              number      = DATA(lv_number)
              returncode  = DATA(lv_rcode).

          IF lv_number IS NOT INITIAL.
            DATA(lv_len)  = strlen( lv_number ).
            DATA(lv_cant) = lv_len - 9.
            IF lv_cant LE 0.
                lv_cant = 0.
            ENDIF.
            IF lv_len GT 9.
              lv_len = 9.
            ENDIF.
            <OnlineSeq>-Sequential = lv_number+lv_cant(lv_len).
          ENDIF.

        CATCH cx_number_ranges INTO DATA(lr_error).

      ENDTRY.

      INSERT VALUE #( %tky = <OnlineSeq>-%tky %param = <OnlineSeq> ) INTO TABLE result.

    ENDLOOP.

    MODIFY ENTITIES OF zcds_rv_ec_002 IN LOCAL MODE
      ENTITY SeqAdministrator
        UPDATE FIELDS ( Sequential )
        WITH CORRESPONDING #( OnlineSeq ).

    INSERT VALUE #(
            %msg = new_message_with_text( text = |{ lines( OnlineSeq ) } records changed { lv_number+lv_cant(lv_len) } |
            severity = if_abap_behv_message=>severity-success )
      ) INTO TABLE reported-seqadministrator.


  ENDMETHOD.

ENDCLASS.
