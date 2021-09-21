*"* local class implementation for public class
*"* CL_CI_TEST_SELECT
*"* use this source file for the implementation part of




*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class LCL_TEST definition final for testing           "#AU Risk_Level Harmless
               inheriting from CL_CI_TEST_VERIFY. "#AU Duration Short
  public section.
    class-methods CLASS_CONSTRUCTOR.
    methods GENERAL  for testing.
    methods TEST_OK  for testing.
    methods TEST_ERR for testing.

endclass.                    "lcl_test DEFINITION


define CHECK_ERR.
  MESSAGE = CHECK( P_CODE = &1 P_PARAM_1 = &2 P_SOBJTYPE = 'PROG'
                   P_SOBJNNAME = INCLUDE  P_LINE = &3 P_COL = &4 ).


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
    TEST = ZCL_CI_002=>C_MY_NAME.
  endmethod.                    "class_constructor

  method GENERAL.
    CHECK_GENERAL( TEST ).
  endmethod.

  method TEST_OK.


    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'PROG'
        P_OBJ_NAME = 'RS_CI_TEST_ABAP_NAMING_1' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

    RUN(
       P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
       P_OBJ_TYPE = 'PROG'
       P_OBJ_NAME = 'RS_CI_TEST_ABAP_NAMING_2' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

   RUN(
       P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
       P_OBJ_TYPE = 'PROG'
       P_OBJ_NAME = 'RS_CI_TEST_ABAP_NAMING_3' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.


    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'TYPE'
        P_OBJ_NAME = 'SCITS' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'CLAS'
        P_OBJ_NAME = 'CL_CI_VERI_CLASS_1' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'INTF'
        P_OBJ_NAME = 'IF_CI_VERI_INTERFACE_1' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

    RUN(
         P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
         P_OBJ_TYPE = 'FUGR'
         P_OBJ_NAME = 'SCI_TEST_NAMING_4' ).

    if LINES( RESULT_LIST ) <> 0.
      FAIL( 'Unerwarteter Fehler' ).
    endif.

  endmethod.                    "test




  method TEST_ERR.

    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'FUGR'
        P_OBJ_NAME = 'SCI_TEST_NAMING_2' ).

    INCLUDE = 'LSCI_TEST_NAMING_2TOP'.

    CHECK_ERR 'GLOTYP'     'XGT_1'      14 17.
    CHECK_ERR 'GLOCONS'    'XGC_1'      15 17.
    CHECK_ERR 'GLODAT'     'XGD_1'      16 17.
    CHECK_ERR 'GLOFS'      '<XG_1>'     17 17.

    CHECK_ERR 'PARAM'      'XPA_1'      19 17.
    CHECK_ERR 'PARAM'      'XPA_2'      20 17.
    CHECK_ERR 'SELOPT'     'XSO_1'      21 17.
    CHECK_ERR 'LOCCLS'     'XLCL_CLASS' 26  8.
    CHECK_ERR 'LOCCLSTYP'  'XLCLT_1'    28 18.
    CHECK_ERR 'LOCCLSCONS' 'XLCLC_1'    29 18.
    CHECK_ERR 'LOCCLSDAT'  'XLCLD_1 '   30 18.
    CHECK_ERR 'LOCCLSCDAT' 'XLCLCD_1'   31 18.
    CHECK_ERR 'LOCCLSMETH' 'XLCLME_1'   32 18.
    CHECK_ERR 'METHIMP'    'XMEIM_1'    33 18.
    CHECK_ERR 'METHEXP'    'XMEEX_1'    34 18.
    CHECK_ERR 'METHCHG'    'XMECH_1'    35 18.
    CHECK_ERR 'LOCCLSMETH' 'SO_NICHT'   36 18.
    CHECK_ERR 'METHRET'    'XMERE_1'    37 24.
    CHECK_ERR 'LOCCLSEV'   'XLCLEV_1'   38 18.
    CHECK_ERR 'EVNTEXP'    'XEVEX_1'    39 24.

    CHECK_ERR 'LOCINTF'    'XLIF_INTF'       47 12.
    CHECK_ERR 'LOCCLSTYP'  'XLIFT_1'         48 16.
    CHECK_ERR 'LOCCLSCONS' 'XLIFC_1'         49 16.
    CHECK_ERR 'LOCCLSDAT'  'XLIFD_1 '        50 16.
    CHECK_ERR 'LOCCLSCDAT' 'XLIFCD_1'        51 16.
    CHECK_ERR 'LOCCLSMETH' 'NIX_VIEL_GUT'    52 16.
    CHECK_ERR 'METHIMP'    'XMEIM_1'         53 16.
    CHECK_ERR 'METHEXP'    'XMEEX_1'         54 16.
    CHECK_ERR 'METHCHG'    'XMECH_1'         55 16.
    CHECK_ERR 'LOCCLSMETH' 'ME_DAS_IST_OKAY' 56 16.
    CHECK_ERR 'METHRET'    'XMERE_1'         57 22.
    CHECK_ERR 'LOCCLSEV'   'XLCLEV_1'        58 16.
    CHECK_ERR 'EVNTEXP'    'XEVEX_1'         59 22.
    CHECK_ERR 'DEFINE'     'X'               62  9.
    CHECK_ERR 'GLOTYP'     'XGT_S_STRUCT'    67 13.


    INCLUDE = 'LSCI_TEST_NAMING_2F01'.

    CHECK_ERR 'LOCTYP'     'XLT_1'      69 20.
    CHECK_ERR 'LOCCONS'    'XLC_1'      70 20.
    CHECK_ERR 'LOCDAT'     'XLD_1'      71 20.
    CHECK_ERR 'LOCFS'      '<XL_FS>'    72 20.

    CHECK_ERR 'FORMTAB'    'XFOTA_1'    89 13.
    CHECK_ERR 'FORMTAB'    'XFOTA_2'    89 27.
    CHECK_ERR 'FORMUSI'    'XFOUS_1'    90 13.
    CHECK_ERR 'FORMUSI'    'XFOUS_2'    90 27.
    CHECK_ERR 'FORMCHG'    'XFOCH_1'    91 13.
    CHECK_ERR 'FORMCHG'    'XFOCH_2'    91 27.



    CHECK_ERR 'LOCTYP'     'XLT_1'      93 18.
    CHECK_ERR 'LOCCONS'    'XLC_1'      94 18.
    CHECK_ERR 'LOCDAT'     'XLD_1'      95 18.
    CHECK_ERR 'LOCFS'      '<XL_FS1>'    96 18.

    CHECK_ERR 'LOCDAT'     'XLOC'       156  9.
    CHECK_ERR 'LOCFS'      '<XL_FS2>'   157 41.

    INCLUDE = 'LSCI_TEST_NAMING_2$01'.

    CHECK_ERR 'FUNCIMP'    'XFUIM_1'     8 17.
    CHECK_ERR 'FUNCIMP'    'XFUIM_2'     9 13.
    CHECK_ERR 'FUNCEXP'    'XFUEX_1'    11 17.
    CHECK_ERR 'FUNCEXP'    'XFUEX_2'    12 13.
    CHECK_ERR 'FUNCTAB'    'XFUTA_1'    14  8.
    CHECK_ERR 'FUNCCHG'    'XFUCH_1'    16 17.
    CHECK_ERR 'FUNCCHG'    'XFUCH_2'    17 13.

    INCLUDE = 'LSCI_TEST_NAMING_2U01'.
    CHECK_ERR 'FUNC'      'XX_FUNC_TEST' 1  9.
    CHECK_ERR 'LOCTYP'     'XLT_1'      16 16.
    CHECK_ERR 'LOCCONS'    'XLC_1'      17 16.
    CHECK_ERR 'LOCDAT'     'XLD_1'      18 16.
    CHECK_ERR 'LOCFS'      '<XL_FS>'    19 16.
    CHECK_ERR 'LOCDAT'     'XLD_2'      20 16.
    CHECK_ERR 'DEFINE'     'XD_MACRO'   21  9.
    CHECK_ERR 'LOCDAT'     'XLD_3'      24  7.
    CHECK_ERR 'LOCFS'      '<XLS_S>'    26 42.


    ASSERT_EQUALS( EXP  = 75
                   ACT  = LINES( RESULT_LIST )
                   MSG = 'Falsche Anzahl von Meldungen' ).


    RUN( P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
         P_OBJ_TYPE = 'CLAS'
         P_OBJ_NAME = 'CL_CI_VER_CLASS_2' ).

    INCLUDE = 'CL_CI_VER_CLASS_2=============CU'.

    CHECK_ERR 'GLOCLS'     'CL_CI_VER_CLASS_2' 000001 0006.
    CHECK_ERR 'GLOCLSTYP'  'XCLT_TI_1'         000009 0006.
    CHECK_ERR 'GLOCLSCONS' 'XCLC_E_1'          000011 0014.
    CHECK_ERR 'GLOCLSDAT'  'XCLD_TH_1'         000013 0006.
    CHECK_ERR 'GLOCLSCDAT' 'XCLCD_TO_1'        000015 0006.
    CHECK_ERR 'GLOCLSEV'   'XCLEV_1'           000017 0011.
    CHECK_ERR 'EVNTEXP'    'XEVEX_E_1'         000019 0014.
    CHECK_ERR 'GLOCLSMETH' 'XME_1'             000021 0012.
    CHECK_ERR 'METHIMP'    'XMEIM_E_1'         000023 0009.
    CHECK_ERR 'METHEXP'    'XMEEX_E_1'         000025 0009.
    CHECK_ERR 'METHCHG'    'XMECH_E_1'         000027 0009.
    CHECK_ERR 'GLOCLSMETH' 'XME_2'             000028 0012.
    CHECK_ERR 'METHRET'    'XMERE_E_1'         000030 0014.

    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 13
                   MSG = 'Falsche Anzahl von Meldungen' ).

    RUN( P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
         P_OBJ_TYPE = 'INTF'
         P_OBJ_NAME = 'IF_CI_VER_INTERFACE_2' ).
    INCLUDE = 'IF_CI_VER_INTERFACE_2=========IU'.
    CHECK_ERR 'GLOINTF'    'IF_CI_VER_INTERFACE_2' 000001 0010.
    CHECK_ERR 'GLOCLSTYP'  'XIFT_O_1'  000005 0008.
    CHECK_ERR 'GLOCLSCONS' 'XIFC_S_1'  000007 0012.
    CHECK_ERR 'GLOCLSDAT'  'XIFD_R_1'  000008 0007.
    CHECK_ERR 'GLOCLSCDAT' 'XIFCD_B_1' 000009 0013.
    CHECK_ERR 'GLOCLSCDAT' 'XIFCD_X_1' 000010 0013.
    CHECK_ERR 'GLOCLSEV'   'XEV_1'     000012 0009.
    CHECK_ERR 'EVNTEXP'    'XEVEX_E_1' 000014 0012.
    CHECK_ERR 'GLOCLSMETH' 'XME_1'     000016 0010.
    CHECK_ERR 'METHIMP'    'XMEIM_E_1' 000018 0007.
    CHECK_ERR 'METHEXP'    'XMEEX_E_1' 000020 0007.
    CHECK_ERR 'METHCHG'    'XMECH_E_1' 000022 0007.
    CHECK_ERR 'METHRET'    'XMERE_E_1' 000025 0012.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 13
                   MSG = 'Falsche Anzahl von Meldungen' ).

    RUN(
        P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
        P_OBJ_TYPE = 'PROG'
        P_OBJ_NAME = 'RS_CI_TEST_ABAP_COMPILER_ERR' ).
    INCLUDE = 'RS_CI_TEST_ABAP_COMPILER_ERR'.
    CHECK_ERR 'SYNERR'      ' ' 7  5.


    RUN(
         P_VARIANT  = 'VERI_NAMING_CONVENTIONS'
         P_OBJ_TYPE = 'FUGR'
         P_OBJ_NAME = 'SCI_TEST_NAMING_5' ).

    INCLUDE = 'LSCI_TEST_NAMING_5U01'.
    CHECK_ERR 'FUNC'      'XX_FUNC_TEST_5' 1  9.
    INCLUDE = 'LSCI_TEST_NAMING_5$01'.
    CHECK_ERR 'FUNCIMP'    'XFUIM_1'    10 13.
    CHECK_ERR 'FUNCIMP'    'XFUIM_2'    11 13.
    ASSERT_EQUALS( ACT = LINES( RESULT_LIST )
                   EXP = 3
                   MSG = 'Falsche Anzahl von Meldungen' ).


  endmethod.                    "test


  "check
endclass.                    "lcl_test IMPLEMENTATION
