*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes



define CHECK_VMSG.
  MESSAGE = CHECK_VERIFY( P_TEST = &1 P_CODE = &2 ).
  if MESSAGE is not initial.
    FAIL( MSG = MESSAGE QUIT = NO ).
  endif.
end-of-definition.


class LCL_TEST_VERIFY definition final for testing risk level harmless duration medium
        inheriting from CL_CI_TEST_VERIFY.
  public section.
    methods TEST_LOCAL  for testing.
    methods TEST_REMOTE for testing.
    methods TEST_RFC    for testing.

endclass.


class LCL_TEST_VERIFY implementation.

  method TEST_LOCAL.
    VERIFY( P_VARIANT  = 'VERI_ITAB_PERFORMANCE' ).
  endmethod.




  method TEST_REMOTE.
    VERIFY( P_VARIANT = 'VERI_ITAB_PERFORMANCE'
            P_SYSID   = 'XXXXXXXX' ).

    CHECK_VMSG 'CI_CI_TEST_VERIFY' 'SYSID'.

    ASSERT_EQUALS( ACT = LINES( VERIFY_LIST )
                   EXP = 1
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.


  method TEST_RFC.
    VERIFY( P_VARIANT      = 'VERI_ITAB_PERFORMANCE'
            P_DESTINATION  = 'NONE' ).

    CHECK_VMSG 'CL_CI_TESTS' '_NON_RFC_'.

    ASSERT_EQUALS( ACT = LINES( VERIFY_LIST )
                   EXP = 1
                   MSG = 'Falsche Anzahl von Meldungen' ).
  endmethod.

endclass.



*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class LCL_TEST definition final for testing             "#AU Risk_Level Harmless
               inheriting from CL_CI_TEST_VERIFY. "#AU Duration Short
  public section.
    class-methods CLASS_CONSTRUCTOR.
    methods GENERAL    for testing.
    methods TEST_OK    for testing.
    methods TEST_ERR   for testing.


endclass.                    "lcl_test DEFINITION


define CHECK_ERR.
  MESSAGE = CHECK( P_CODE = &1 P_SOBJTYPE = 'PROG'
                   P_SOBJNNAME = INCLUDE  P_LINE = &2 P_COL = &3 ).


  if MESSAGE is not initial.
    FAIL( MSG = MESSAGE QUIT = NO ).
  endif.
end-of-definition.



*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class LCL_TEST implementation.

  method CLASS_CONSTRUCTOR.
    TEST = ZCL_CI_008_aux=>C_MY_NAME.
    set language 'D'.
  endmethod.                    "class_constructor


  method GENERAL.
    CHECK_GENERAL( TEST ).
  endmethod.


  method TEST_OK.
    RUN( P_VARIANT  = 'VERI_ITAB_PERFORMANCE'
         P_OBJ_TYPE = 'PROG'
         P_OBJ_NAME = 'SCI_TEST_ITAB_PERFORMANCE_1' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.
  endmethod.                    "test

  method TEST_ERR.

    RUN( P_VARIANT  = 'VERI_ITAB_PERFORMANCE'
         P_OBJ_TYPE = 'PROG'
         P_OBJ_NAME = 'SCI_TEST_ITAB_PERFORMANCE_2' ).

    INCLUDE = 'SCI_TEST_ITAB_PERFORMANCE_2'.

    CHECK_ERR 'MESSAGEGM3'   242 2.
    CHECK_ERR 'MESSAGEGM3'   244 2.
    CHECK_ERR 'MESSAGEGM3'   268 2.
    CHECK_ERR 'MESSAGEGM3'   270 2.
    CHECK_ERR 'MESSAGEGM3'   282 2.
    CHECK_ERR 'MESSAGEGM3'   284 2.
    CHECK_ERR 'MESSAGEGM3'   296 2.
    CHECK_ERR 'MESSAGEGM3'   298 2.
    CHECK_ERR 'MESSAGEGM3'   310 2.
    CHECK_ERR 'MESSAGEGM3'   312 2.
    CHECK_ERR 'MESSAGEGM3'   377 2.
    CHECK_ERR 'MESSAGEGM3'   379 2.
    CHECK_ERR 'MESSAGEGM3'   404 2.
    CHECK_ERR 'MESSAGEGM3'   406 2.
    CHECK_ERR 'MESSAGEGM3'   417 2.
    CHECK_ERR 'MESSAGEGM3'   419 2.
    CHECK_ERR 'MESSAGEGM3'   433 2.
    CHECK_ERR 'MESSAGEGM3'   436 2.
    CHECK_ERR 'MESSAGEGM3'   462 2.
    CHECK_ERR 'MESSAGEGM3'   465 2.
    CHECK_ERR 'MESSAGEGM3'   481 2.
    CHECK_ERR 'MESSAGEGM3'   484 2.
    CHECK_ERR 'MESSAGEGM3'   500 2.
    CHECK_ERR 'MESSAGEGM3'   503 2.
    CHECK_ERR 'MESSAGEGM3'   519 33.
    CHECK_ERR 'MESSAGEGM3'   522 33.
    CHECK_ERR 'MESSAGEGM3'   589 2.
    CHECK_ERR 'MESSAGEGM3'   591 2.
    CHECK_ERR 'MESSAGEGM3'   602 2.
    CHECK_ERR 'MESSAGEGM3'   604 2.
    CHECK_ERR 'MESSAGEGM3'   615 2.
    CHECK_ERR 'MESSAGEGM3'   617 2.
    CHECK_ERR 'MESSAGEGM3'   628 2.
    CHECK_ERR 'MESSAGEGM3'   630 2.
    CHECK_ERR 'MESSAGEGM3'   641 2.
    CHECK_ERR 'MESSAGEGM3'   643 2.
    CHECK_ERR 'MESSAGEGM3'   654 24.
    CHECK_ERR 'MESSAGEGM3'   656 24.
    CHECK_ERR 'MESSAGEGM3'   706 2.
    CHECK_ERR 'MESSAGEGM3'   708 2.
    CHECK_ERR 'MESSAGEGM3'   722 2.
    CHECK_ERR 'MESSAGEGM3'   725 2.
    CHECK_ERR 'MESSAGEGM3'   749 2.
    CHECK_ERR 'MESSAGEGM3'   751 2.
    CHECK_ERR 'MESSAGEGM3'   762 2.
    CHECK_ERR 'MESSAGEGM3'   764 2.
    CHECK_ERR 'MESSAGEGM3'   775 2.
    CHECK_ERR 'MESSAGEGM3'   777 2.

    CHECK_ERR 'MESSAGEGM2'   308 2.
    CHECK_ERR 'MESSAGEGM2'   375 2.
    CHECK_ERR 'MESSAGEGM2'   478 33.
    CHECK_ERR 'MESSAGEGM2'   516 33.
    CHECK_ERR 'MESSAGEGM2'   639 24.
    CHECK_ERR 'MESSAGEGM2'   652 24.
    CHECK_ERR 'MESSAGEGM2'   786 48.
    CHECK_ERR 'MESSAGEGM2'   799 48.

    CHECK_ERR 'MESSAGEGM1'   373 2.
    CHECK_ERR 'MESSAGEGM1'   513 33.
    CHECK_ERR 'MESSAGEGM1'   650 24.
    CHECK_ERR 'MESSAGEGM1'   797 48.

    CHECK_ERR 'MESSAGEGM0'    84 2.
    CHECK_ERR 'MESSAGEGM0'    90 2.
    CHECK_ERR 'MESSAGEGM0'    92 2.
    CHECK_ERR 'MESSAGEGM0'   103 2.
    CHECK_ERR 'MESSAGEGM0'   107 2.
    CHECK_ERR 'MESSAGEGM0'   150 2.
    CHECK_ERR 'MESSAGEGM0'   156 2.

    CHECK_ERR 'MESSAGEGM0'   180 2.
    CHECK_ERR 'MESSAGEGM0'   186 2.
    CHECK_ERR 'MESSAGEGM0'   200 2.
    CHECK_ERR 'MESSAGEGM0'   210 2.
    CHECK_ERR 'MESSAGEGM0'   212 2.
    CHECK_ERR 'MESSAGEGM0'   236 2.
    CHECK_ERR 'MESSAGEGM0'   246 2.
    CHECK_ERR 'MESSAGEGM0'   262 2.
    CHECK_ERR 'MESSAGEGM0'   272 2.
    CHECK_ERR 'MESSAGEGM0'   276 2.
    CHECK_ERR 'MESSAGEGM0'   286 2.
    CHECK_ERR 'MESSAGEGM0'   290 2.
    CHECK_ERR 'MESSAGEGM0'   300 2.
    CHECK_ERR 'MESSAGEGM0'   304 2.
    CHECK_ERR 'MESSAGEGM0'   314 2.
    CHECK_ERR 'MESSAGEGM0'   371 2.
    CHECK_ERR 'MESSAGEGM0'   381 2.
    CHECK_ERR 'MESSAGEGM0'   398 2.
    CHECK_ERR 'MESSAGEGM0'   408 2.
    CHECK_ERR 'MESSAGEGM0'   411 2.
    CHECK_ERR 'MESSAGEGM0'   421 2.
    CHECK_ERR 'MESSAGEGM0'   424 2.
    CHECK_ERR 'MESSAGEGM0'   453 33.
    CHECK_ERR 'MESSAGEGM0'   468 35.
    CHECK_ERR 'MESSAGEGM0'   472 33.
    CHECK_ERR 'MESSAGEGM0'   487 35.
    CHECK_ERR 'MESSAGEGM0'   491 33.
    CHECK_ERR 'MESSAGEGM0'   506 35.
    CHECK_ERR 'MESSAGEGM0'   510 33.
    CHECK_ERR 'MESSAGEGM0'   525 35.
    CHECK_ERR 'MESSAGEGM0'   583 2.
    CHECK_ERR 'MESSAGEGM0'   593 2.
    CHECK_ERR 'MESSAGEGM0'   596 2.
    CHECK_ERR 'MESSAGEGM0'   606 2.
    CHECK_ERR 'MESSAGEGM0'   609 24.
    CHECK_ERR 'MESSAGEGM0'   619 26.
    CHECK_ERR 'MESSAGEGM0'   622 24.
    CHECK_ERR 'MESSAGEGM0'   632 26.
    CHECK_ERR 'MESSAGEGM0'   635 24.
    CHECK_ERR 'MESSAGEGM0'   645 26.
    CHECK_ERR 'MESSAGEGM0'   648 24.
    CHECK_ERR 'MESSAGEGM0'   658 26.
    CHECK_ERR 'MESSAGEGM0'   661 2.
    CHECK_ERR 'MESSAGEGM0'   671 2.
    CHECK_ERR 'MESSAGEGM0'   700 2.
    CHECK_ERR 'MESSAGEGM0'   710 2.
    CHECK_ERR 'MESSAGEGM0'   713 2.
    CHECK_ERR 'MESSAGEGM0'   743 2.
    CHECK_ERR 'MESSAGEGM0'   753 2.
    CHECK_ERR 'MESSAGEGM0'   756 48.
    CHECK_ERR 'MESSAGEGM0'   766 50.
    CHECK_ERR 'MESSAGEGM0'   769 48.
    CHECK_ERR 'MESSAGEGM0'   779 50.
    CHECK_ERR 'MESSAGEGM0'   782 48.
    CHECK_ERR 'MESSAGEGM0'   792 50.
    CHECK_ERR 'MESSAGEGM0'   795 48.
    CHECK_ERR 'MESSAGEGM0'   805 50.
    CHECK_ERR 'MESSAGEGM0'   808 2.
    CHECK_ERR 'MESSAGEGM0'   818 2.
    CHECK_ERR 'MESSAGEGJG'   857 30.

    if LINES( RESULT_LIST ) <> 127.
      FAIL( 'Zuviele Meldungen').
    endif.


  endmethod.                    "TEST_ERR

endclass.                    "LCL_TEST IMPLEMENTATION
