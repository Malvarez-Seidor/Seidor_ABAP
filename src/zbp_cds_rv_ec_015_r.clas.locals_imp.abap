CLASS lhc_ElectronicDocumentsRecepti DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ElectronicDocumentsReception RESULT result.

ENDCLASS.

CLASS lhc_ElectronicDocumentsRecepti IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
