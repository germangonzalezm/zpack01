
* Created by RS_CI_CREATE_TEST
define CHECK_MSG0.
  MESSAGE = CHECK( P_CODE = &1 P_SOBJTYPE = 'PROG'
                   P_SOBJNNAME = INCLUDE P_LINE = &2 P_COL = &3 P_CHECKSUM1 = &4 ).
  if MESSAGE is not initial.
    FAIL( MSG = MESSAGE QUIT = NO ).
  endif.
end-of-definition.

class LCL_TEST definition final for testing risk level harmless duration short
        inheriting from CL_CI_TEST_VERIFY.
  public section.
    class-methods CLASS_CONSTRUCTOR.
    methods TEST1 for testing.
endclass.
class LCL_TEST implementation.
  method CLASS_CONSTRUCTOR.
    TEST = 'ZCL_CI_008'.
  endmethod.
  method TEST1.
    RUN( P_VARIANT  = 'VERI_PRETTY_PRINT'
         P_OBJ_TYPE = 'CLAS'
         P_OBJ_NAME = 'ZCL_CI_008' ).
    INCLUDE = 'ZCL_CI_008=======CCAU'.
    CHECK_MSG0 'PP_ERR' 000003 0000 626837752.
    INCLUDE = 'ZCL_CI_008=======CCDEF'.
    CHECK_MSG0 'PP_ERR' 000006 0000 1513282957.
    INCLUDE = 'ZCL_CI_008=======CCIMP'.
    CHECK_MSG0 'PP_ERR' 000007 0000 -1903004994.
    INCLUDE = METHOD_INCLUDE( 'ZCL_CI_008=>CONSTRUCTOR' ).
    CHECK_MSG0 'PP_ERR' 000001 0000 748390137.
    INCLUDE = METHOD_INCLUDE( 'ZCL_CI_008=>RUN' ).
    CHECK_MSG0 'PP_ERR' 000001 0000 567844896.
    INCLUDE = METHOD_INCLUDE( 'ZCL_CI_008=>IF_CI_TEST~QUERY_ATTRIBUTES' ).
    CHECK_MSG0 'PP_ERR' 000001 0000 622443927.
    INCLUDE = METHOD_INCLUDE( 'ZCL_CI_008=>GET_ATTRIBUTES' ).
    CHECK_MSG0 'PP_ERR' 000001 0000 996076946.
    INCLUDE = METHOD_INCLUDE( 'ZCL_CI_008=>PUT_ATTRIBUTES' ).
    CHECK_MSG0 'PP_ERR' 000001 0000 1067446309.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 8
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.
endclass.
