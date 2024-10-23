CLASS lhc_test DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION IMPORTING keys REQUEST requested_authorizations FOR test RESULT result.

    METHODS create  FOR MODIFY  IMPORTING entities FOR CREATE test.

    METHODS update  FOR MODIFY  IMPORTING entities FOR UPDATE test.

    METHODS delete  FOR MODIFY  IMPORTING keys FOR DELETE test.

    METHODS read    FOR READ    IMPORTING keys FOR READ test RESULT result.

    METHODS lock    FOR LOCK    IMPORTING keys FOR LOCK test.

    METHODS test FOR MODIFY     IMPORTING keys FOR ACTION test~test.

ENDCLASS.

CLASS lhc_test IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
    zbp_i_test_002=>op = |C|.
    LOOP AT entities INTO DATA(entity).
    ENDLOOP.
  ENDMETHOD. " create

  METHOD update.
    zbp_i_test_002=>op = |U|.
    CLEAR zbp_i_test_002=>entities_update[].
    LOOP AT entities INTO DATA(entity).
        APPEND entity TO zbp_i_test_002=>entities_update.
    ENDLOOP.
*   The command "COMMIT-ENTITIES" is not allowed in a BEHAVIOR class.
*    COMMIT ENTITIES BEGIN.
*    COMMIT ENTITIES END.
  ENDMETHOD. " update

  METHOD delete.
    zbp_i_test_002=>op = |D|.
    zbp_i_test_002=>keys_delete = keys.
  ENDMETHOD. " delete

  METHOD read.
    zbp_i_test_002=>op = |R|.
    LOOP AT keys INTO DATA(key).
    ENDLOOP.
  ENDMETHOD. " read

  METHOD lock.
    zbp_i_test_002=>op = |L|.
    LOOP AT keys INTO DATA(key).
    ENDLOOP.
  ENDMETHOD. " lock

  METHOD test.
  ENDMETHOD.

ENDCLASS. " lhc_test IMPLEMENTATION

CLASS lsc_zi_test_002 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS. " lsc_zi_test_002 DEFINITION


CLASS lsc_zi_test_002 IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    DATA ztest002 TYPE ztest002.

    CASE zbp_i_test_002=>op.

        WHEN 'C'.
            DATA it_test TYPE TABLE OF ztest002.
            SELECT MAX( ID ) FROM ztest002 INTO @DATA(maxID).
            DATA(newID) = CONV string( maxID + 1 ).
            INSERT VALUE #( ID = newID DATA = |{ newID }{ newID }{ newID }| COUNTRY = 'DE' ) INTO TABLE it_test.
            INSERT ztest002 FROM TABLE @it_test.
        WHEN 'R'.
            LOOP AT zbp_i_test_002=>keys_read INTO DATA(key_read).
            ENDLOOP.
        WHEN 'U'.
            LOOP AT zbp_i_test_002=>entities_update INTO DATA(entity_update).
                IF ( entity_update-%control-Data = if_abap_behv=>mk-on ).
                    UPDATE ztest002 SET data = @entity_update-Data WHERE ( id = @entity_update-id ).
                ENDIF.
                IF ( entity_update-%control-Country = if_abap_behv=>mk-on ).
                    UPDATE ztest002 SET country = @entity_update-Country WHERE ( id = @entity_update-id ).
                ENDIF.
            ENDLOOP.
        WHEN 'D'.
            LOOP AT zbp_i_test_002=>keys_delete INTO DATA(key_delete).
                DELETE FROM ztest002 WHERE ( id = @key_delete-id ).
            ENDLOOP.
     ENDCASE.

  ENDMETHOD. " save

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS. " lsc_zi_test_002 IMPLEMENTATION
