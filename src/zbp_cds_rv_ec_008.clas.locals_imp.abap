CLASS lhc_SeqMaintenance DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    DATA update_allowed TYPE abap_bool.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR SeqMaintenance RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR SeqMaintenance RESULT result.

    METHODS is_update_allowed
      RETURNING
        value(r_result) TYPE abap_bool.

ENDCLASS.

CLASS lhc_SeqMaintenance IMPLEMENTATION.

  METHOD get_global_authorizations.

*   Check if EDIT operation is triggered or not
    IF requested_authorizations-%update = if_abap_behv=>mk-on OR
       requested_authorizations-%create = if_abap_behv=>mk-on OR
       requested_authorizations-%delete = if_abap_behv=>mk-on.

*     Check method IS_UPDATE_ALLOWED (Authorization simulation Check method)
      IF is_update_allowed( ) = abap_false.

*       update result with EDIT Allowed
        result-%update = if_abap_behv=>auth-allowed.
        result-%create = if_abap_behv=>auth-allowed.
        result-%delete = if_abap_behv=>auth-allowed.

      ELSE.

*       update result with EDIT Not Allowed
        result-%update = if_abap_behv=>auth-unauthorized.
        result-%create = if_abap_behv=>auth-unauthorized.
        result-%delete = if_abap_behv=>auth-unauthorized.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_instance_authorizations.

    DATA: update_requested TYPE abap_bool,
          update_grtanted  TYPE abap_bool.

     READ ENTITIES OF zcds_rv_ec_008  IN LOCAL MODE
       ENTITY SeqMaintenance
       FIELDS ( Companycode Documentsri Establishment Emissionpoint Users Sequence
                Accountingdocumenttype Billingdocumenttype Deliverydocumenttype
                Goodsmovementtype Plant Salesorganization Storagelocation )
       WITH CORRESPONDING #( keys )
       RESULT DATA(Maintenances).

     CHECK Maintenances is not initial.

     update_requested = COND #( WHEN requested_authorizations-%update = if_abap_behv=>mk-on
                             THEN abap_true ELSE abap_false ).
     result = VALUE #( FOR Maintenance IN Maintenances ( %tky = Maintenance-%tky ) ).

  ENDMETHOD.

  METHOD is_update_allowed.
    update_allowed = abap_false.
  ENDMETHOD.

ENDCLASS.
