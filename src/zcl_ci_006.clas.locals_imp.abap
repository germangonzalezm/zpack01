* Created by RS_CI_CREATE_TEST
define CHECK_MSG1.
  MESSAGE = CHECK( P_CODE = &1 P_SOBJTYPE = SOBJTYPE
                   P_SOBJNNAME = SOBJNAME P_PARAM_1 = &2 P_LINE = &3 P_COL = &4 P_CHECKSUM1 = &5 ).
  if MESSAGE is not initial.
    FAIL( MSG = MESSAGE QUIT = NO ).
  endif.
end-of-definition.
class LCL_TEST definition final for testing risk level harmless duration short
        inheriting from CL_CI_TEST_VERIFY.
  public section.
    class-methods CLASS_CONSTRUCTOR.
    methods GENERAL for testing.
    methods TEST_1 for testing.
    methods TEST_2 for testing.
    methods TEST_3 for testing.
endclass.
class LCL_TEST implementation.
  method CLASS_CONSTRUCTOR.
    TEST = 'ZCL_CI_006'.
  endmethod.

  method GENERAL.
    CHECK_GENERAL( TEST ).
  endmethod.

  method TEST_1.
    RUN( P_VARIANT  = 'VERI_FREE_SEARCH_1'
         P_OBJ_TYPE = 'PROG'
         P_OBJ_NAME = 'RS_CI_TEST_FREE_SEARCH_1' ).
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 1
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

  method TEST_2.
    RUN( P_VARIANT  = 'VERI_FREE_SEARCH_2'
         P_OBJ_TYPE = 'PROG'
         P_OBJ_NAME = 'RS_CI_TEST_FREE_SEARCH_1' ).
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    CHECK_MSG1 '0001' '''XXXX''' 000014 0002 314079131.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 2
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

  method TEST_3.
    RUN( P_VARIANT  = 'VERI_FREE_SEARCH_3'
         P_OBJ_TYPE = 'PROG'
         P_OBJ_NAME = 'RS_CI_TEST_FREE_SEARCH_1' ).
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    CHECK_MSG1 '0001' '"XXXX' 000016 0002 880887074.
    CHECK_MSG1 '0001' '*  XXXX' 000018 0000 880887074.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 3
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

endclass.


class LCL_TEST_NONE definition final for testing risk level harmless duration short
        inheriting from CL_CI_TEST_VERIFY.
  public section.
    class-methods CLASS_CONSTRUCTOR.
    methods TEST_1 for testing.
    methods TEST_2 for testing.
    methods TEST_3 for testing.
endclass.
class LCL_TEST_NONE implementation.
  method CLASS_CONSTRUCTOR.
    TEST = 'ZCL_CI_006'.
  endmethod.

   method TEST_1.
    RUN( P_VARIANT     = 'VERI_FREE_SEARCH_1'
         P_OBJ_TYPE    = 'PROG'
         P_OBJ_NAME    = 'RS_CI_TEST_FREE_SEARCH_1'
         P_DESTINATION = 'NONE').
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 1
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

  method TEST_2.
    RUN( P_VARIANT     = 'VERI_FREE_SEARCH_2'
         P_OBJ_TYPE    = 'PROG'
         P_OBJ_NAME    = 'RS_CI_TEST_FREE_SEARCH_1'
         P_DESTINATION = 'NONE'  ).
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    CHECK_MSG1 '0001' '''XXXX''' 000014 0002 314079131.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 2
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

  method TEST_3.
    RUN( P_VARIANT     = 'VERI_FREE_SEARCH_3'
         P_OBJ_TYPE    = 'PROG'
         P_OBJ_NAME    = 'RS_CI_TEST_FREE_SEARCH_1'
         P_DESTINATION = 'NONE' ).
    SOBJTYPE = 'PROG'.
    SOBJNAME = 'RS_CI_TEST_FREE_SEARCH_1'.
    CHECK_MSG1 '0001' 'XXXX' 000012 0003 95626193.
    CHECK_MSG1 '0001' '"XXXX' 000016 0002 880887074.
    CHECK_MSG1 '0001' '*  XXXX' 000018 0000 880887074.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 3
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.


endclass.
