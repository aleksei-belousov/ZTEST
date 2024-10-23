CLASS zbp_i_test_002 DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zi_test_002.
    CLASS-DATA op TYPE C.
    CLASS-DATA entities_create  TYPE TABLE FOR CREATE   zi_test_002\\test.
    CLASS-DATA entities_update  TYPE TABLE FOR UPDATE   zi_test_002\\test.
    CLASS-DATA keys_read        TYPE TABLE FOR READ IMPORT zi_test_002\\test.
    CLASS-DATA keys_delete      TYPE TABLE FOR DELETE   zi_test_002\\test.
    CLASS-DATA keys_lock        TYPE TABLE FOR LOCK     zi_test_002\\test.
ENDCLASS.



CLASS ZBP_I_TEST_002 IMPLEMENTATION.
ENDCLASS.
