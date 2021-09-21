class ZCL_CI_002 definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public .

public section.

*"* public components of class ZCL_CI_002
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CLEAR
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.

  methods CHECK_DATA_TYPE
    importing
      !P_IDS type SCIR_ABAPID
      !P_CODE type SCI_ERRC .
  methods CHECK_TYPEPOOL .
  methods CHECK_CLASS .
  methods CHECK_DEFINE .
  methods CHECK_INTERFACE .
  methods CHECK_PROGRAM .
  methods CHECK_FUNCTION_POOL .
  methods GET_CLASS_STMT_KIND
    returning
      value(P_RESULT) type I .
  methods CHECK_METHOD .
  methods CHECK_FORM .
  methods CHECK_FUNCTION .
  methods GET_INTERFACE_STMT_KIND
    returning
      value(P_RESULT) type I .
  methods CHECK_GENERAL
    importing
      !P_IDS type SCIR_ABAPID
      !P_CODE type SCI_ERRC .
  methods QUALIFY_STATEMENT
    importing
      !P_REF_SCAN type ref to CL_CI_SCAN
    returning
      value(P_RESULT) type SYCHAR01 .
  methods CHECK_METHODS
    importing
      !P_IDS type SCIR_ABAPID
      !P_CODE type SCI_ERRC .
  methods CHECK_EVENTS
    importing
      !P_IDS type SCIR_ABAPID
      !P_CODE type SCI_ERRC .
  methods CHECK_FUNCTION_SIGNATURE
    importing
      !P_FUNCTION type STRING .
  methods CHECK_ANY .
  methods CHECK_SET .
private section.

  constants C_CLASS_DEFER type I value 3 ##NO_TEXT.
  constants C_CLASS_DEF_GLOB type I value 2 ##NO_TEXT.
  constants C_CLASS_DEF_LOC type I value 1 ##NO_TEXT.
  constants C_CLASS_FRIENDS type I value 6 ##NO_TEXT.
  constants C_CLASS_IMPL type I value 5 ##NO_TEXT.
  constants C_CLASS_LOAD type I value 4 ##NO_TEXT.
  constants C_DEFINE_CODE type SCI_ERRC value 'DEFINE' ##NO_TEXT.
  constants C_FORM_CHG_CODE type SCI_ERRC value 'FORMCHG' ##NO_TEXT.
  constants C_FORM_CODE type SCI_ERRC value 'FORM' ##NO_TEXT.
  constants C_FLDGRP_CODE type SCI_ERRC value 'FLDGRP' ##NO_TEXT.
  constants C_FORM_TAB_CODE type SCI_ERRC value 'FORMTAB' ##NO_TEXT.
  constants C_FORM_USI_CODE type SCI_ERRC value 'FORMUSI' ##NO_TEXT.
  constants C_FUNC_CHG_CODE type SCI_ERRC value 'FUNCCHG' ##NO_TEXT.
  constants C_FUNC_CODE type SCI_ERRC value 'FUNC' ##NO_TEXT.
  constants C_FUNC_EXP_CODE type SCI_ERRC value 'FUNCEXP' ##NO_TEXT.
  constants C_FUNC_IMP_CODE type SCI_ERRC value 'FUNCIMP' ##NO_TEXT.
  constants C_FUNC_TAB_CODE type SCI_ERRC value 'FUNCTAB' ##NO_TEXT.
  constants C_GLOB_CLS_CLASSDATA_CODE type SCI_ERRC value 'GLOCLSCDAT' ##NO_TEXT.
  constants C_GLOB_CLS_CODE type SCI_ERRC value 'GLOCLS' ##NO_TEXT.
  constants C_GLOB_CLS_CONSTANT_CODE type SCI_ERRC value 'GLOCLSCONS' ##NO_TEXT.
  constants C_GLOB_CLS_DATA_CODE type SCI_ERRC value 'GLOCLSDAT' ##NO_TEXT.
  constants C_GLOB_CLS_EVENT_CODE type SCI_ERRC value 'GLOCLSEV' ##NO_TEXT.
  constants C_GLOB_CLS_METH_CODE type SCI_ERRC value 'GLOCLSMETH' ##NO_TEXT.
  constants C_GLOB_CLS_TYPE_CODE type SCI_ERRC value 'GLOCLSTYP' ##NO_TEXT.
  constants C_GLOB_CONSTANT_CODE type SCI_ERRC value 'GLOCONS' ##NO_TEXT.
  constants C_GLOB_DATA_CODE type SCI_ERRC value 'GLODAT' ##NO_TEXT.
  constants C_GLOB_FS_CODE type SCI_ERRC value 'GLOFS' ##NO_TEXT.
  constants C_GLOB_INTF_CODE type SCI_ERRC value 'GLOINTF' ##NO_TEXT.
  constants C_GLOB_TYPE_CODE type SCI_ERRC value 'GLOTYP' ##NO_TEXT.
  constants C_LOC_CLS_CLASSDATA_CODE type SCI_ERRC value 'LOCCLSCDAT' ##NO_TEXT.
  constants C_LOC_CLS_CODE type SCI_ERRC value 'LOCCLS' ##NO_TEXT.
  constants C_LOC_CLS_CONSTANT_CODE type SCI_ERRC value 'LOCCLSCONS' ##NO_TEXT.
  constants C_LOC_CLS_DATA_CODE type SCI_ERRC value 'LOCCLSDAT' ##NO_TEXT.
  constants C_LOC_CLS_EVENT_CODE type SCI_ERRC value 'LOCCLSEV' ##NO_TEXT.
  constants C_LOC_CLS_METH_CODE type SCI_ERRC value 'LOCCLSMETH' ##NO_TEXT.
  constants C_LOC_CLS_TYPE_CODE type SCI_ERRC value 'LOCCLSTYP' ##NO_TEXT.
  constants C_LOC_CONSTANT_CODE type SCI_ERRC value 'LOCCONS' ##NO_TEXT.
  constants C_LOC_DATA_CODE type SCI_ERRC value 'LOCDAT' ##NO_TEXT.
  constants C_LOC_FS_CODE type SCI_ERRC value 'LOCFS' ##NO_TEXT.
  constants C_LOC_INTF_CODE type SCI_ERRC value 'LOCINTF' ##NO_TEXT.
  constants C_LOC_TYPE_CODE type SCI_ERRC value 'LOCTYP' ##NO_TEXT.
  constants C_METH_CHG_CODE type SCI_ERRC value 'METHCHG' ##NO_TEXT.
  constants C_METH_EXP_CODE type SCI_ERRC value 'METHEXP' ##NO_TEXT.
  constants C_METH_IMP_CODE type SCI_ERRC value 'METHIMP' ##NO_TEXT.
  constants C_METH_RET_CODE type SCI_ERRC value 'METHRET' ##NO_TEXT.
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_CI_002' ##NO_TEXT.
  constants C_PARAM_CODE type SCI_ERRC value 'PARAM' ##NO_TEXT.
  constants C_SELOPT_CODE type SCI_ERRC value 'SELOPT' ##NO_TEXT.
  data DEFINE_IDS type SCIR_ABAPID .
  data FORM_CHG_IDS type SCIR_ABAPID .
  data FORM_IDS type SCIR_ABAPID .
  data FORM_TAB_IDS type SCIR_ABAPID .
  data FORM_USI_IDS type SCIR_ABAPID .
  data FUNC_CHG_IDS type SCIR_ABAPID .
  data FUNC_EXP_IDS type SCIR_ABAPID .
  data FUNC_IDS type SCIR_ABAPID .
  data FUNC_IMP_IDS type SCIR_ABAPID .
  data FUNC_TAB_IDS type SCIR_ABAPID .
  data GLOB_CLS_CLASSDATA_IDS type SCIR_ABAPID .
  data GLOB_CLS_CONSTANT_IDS type SCIR_ABAPID .
  data GLOB_CLS_DATA_IDS type SCIR_ABAPID .
  data GLOB_CLS_EVENT_IDS type SCIR_ABAPID .
  data GLOB_CLS_IDS type SCIR_ABAPID .
  data GLOB_CLS_METH_IDS type SCIR_ABAPID .
  data GLOB_CLS_TYPE_IDS type SCIR_ABAPID .
  data GLOB_CONSTANT_IDS type SCIR_ABAPID .
  data GLOB_DATA_IDS type SCIR_ABAPID .
  data GLOB_FS_IDS type SCIR_ABAPID .
  data GLOB_INTF_IDS type SCIR_ABAPID .
  data GLOB_TYPE_IDS type SCIR_ABAPID .
  data LOC_CLS_CLASSDATA_IDS type SCIR_ABAPID .
  data LOC_CLS_CONSTANT_IDS type SCIR_ABAPID .
  data LOC_CLS_DATA_IDS type SCIR_ABAPID .
  data LOC_CLS_EVENT_IDS type SCIR_ABAPID .
  data LOC_CLS_IDS type SCIR_ABAPID .
  data LOC_CLS_METH_IDS type SCIR_ABAPID .
  data LOC_CLS_TYPE_IDS type SCIR_ABAPID .
  data LOC_CONSTANT_IDS type SCIR_ABAPID .
  data LOC_DATA_IDS type SCIR_ABAPID .
  data LOC_FS_IDS type SCIR_ABAPID .
  data LOC_INTF_IDS type SCIR_ABAPID .
  data LOC_TYPE_IDS type SCIR_ABAPID .
  data METH_CHG_IDS type SCIR_ABAPID .
  data METH_EXP_IDS type SCIR_ABAPID .
  data METH_IMP_IDS type SCIR_ABAPID .
  data METH_RET_IDS type SCIR_ABAPID .
  data PARAM_IDS type SCIR_ABAPID .
  data QUALIFIED_TOKENS type STOKESX_TAB .
  data SELOPT_IDS type SCIR_ABAPID .
  data STATEMENT_FROM type I .
  data STATEMENT_INDEX type I .
  data STATEMENT_TO type I .
  constants C_PROGRAM type SCI_ERRC value 'PROG' ##NO_TEXT.
  constants C_FUNCTION_POOL type SCI_ERRC value 'FUNCPOOL' ##NO_TEXT.
  constants C_EVNT_EXP_CODE type SCI_ERRC value 'EVNTEXP' ##NO_TEXT.
  data EVNT_EXP_IDS type SCIR_ABAPID .
  constants C_SYNTAX type SCI_ERRC value 'SYNERR' ##NO_TEXT.
  data FLDGRP_IDS type SCIR_ABAPID .
  data IGNORE type SYCHAR01 .
  data SCAN_WD_COMPONENT type SAP_BOOL value ABAP_FALSE ##NO_TEXT.

  methods CHECK_ANY_IN_SCOPE
    importing
      !P_DATA_CODE type SCI_ERRC
      !P_DATA_IDS type SCIR_ABAPID
      !P_FS_CODE type SCI_ERRC
      !P_FS_IDS type SCIR_ABAPID .
ENDCLASS.



CLASS ZCL_CI_002 IMPLEMENTATION.


  method CHECK_ANY.
    " local scope
    CHECK_ANY_IN_SCOPE(
      exporting
        P_DATA_IDS = LOC_DATA_IDS   P_DATA_CODE = C_LOC_DATA_CODE
        P_FS_IDS =   LOC_FS_IDS     P_FS_CODE =   C_LOC_FS_CODE ).
  endmethod.


  method CHECK_ANY_IN_SCOPE.
    data:
      L_NAME    type STRING,
      L_INCLUDE type PROGRAM,
      L_COLUMN  type TOKEN_COL.

    loop at REF_SCAN->TOKENS from STATEMENT_WA-FROM to STATEMENT_WA-TO into TOKEN_WA.

      if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
         TOKEN_WA-STR(TOKEN_WA-LEN1) = 'DATA'.
        L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
        L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        if L_NAME not in P_DATA_IDS and IGNORE = ABAP_FALSE.

          L_INCLUDE = GET_INCLUDE( ).
          INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                     P_SUB_OBJ_NAME = L_INCLUDE
                     P_LINE         = TOKEN_WA-ROW
                     P_COLUMN       = L_COLUMN
                     P_TEST         = C_MY_NAME
                     P_CODE         = P_DATA_CODE
                     P_PARAM_1      = L_NAME ).
        endif.

      elseif TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
         TOKEN_WA-STR(TOKEN_WA-LEN1) = 'FIELD-SYMBOL'.
        L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
        L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.

        if L_NAME not in P_FS_IDS and IGNORE = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                     P_SUB_OBJ_NAME = L_INCLUDE
                     P_LINE         = TOKEN_WA-ROW
                     P_COLUMN       = L_COLUMN
                     P_TEST         = C_MY_NAME
                     P_CODE         = P_FS_CODE
                     P_PARAM_1      = L_NAME ).
        endif.
      endif.
    endloop.

  endmethod.


  method CHECK_CLASS.
    data:
      L_KIND           type I,
      L_NAME           type STRING,
      L_CLS_IDS        like LOC_CLS_DATA_IDS,
      L_CLS_CODE       type SCI_ERRC,
      L_TYPE_IDS       like LOC_CLS_DATA_IDS,
      L_TYPE_CODE      type SCI_ERRC,
      L_CONST_IDS      like LOC_CLS_DATA_IDS,
      L_CONST_CODE     type SCI_ERRC,
      L_DATA_IDS       like LOC_CLS_DATA_IDS,
      L_DATA_CODE      type SCI_ERRC,
      L_CLASSDATA_IDS  like LOC_CLS_CLASSDATA_IDS,
      L_CLASSDATA_CODE type SCI_ERRC,
      L_EVENT_IDS      like LOC_CLS_DATA_IDS,
      L_EVENT_CODE     type SCI_ERRC,
      L_METH_IDS       like LOC_CLS_CLASSDATA_IDS,
      L_METH_CODE      type SCI_ERRC,
      L_INCLUDE        type PROGRAM,
      L_LINE           type I,
      L_COLUMN         type TOKEN_COL.


    L_KIND = GET_CLASS_STMT_KIND( ).

    if L_KIND = C_CLASS_LOAD or L_KIND = C_CLASS_DEFER or
       L_KIND = C_CLASS_FRIENDS.
      return.
    endif.

    if L_KIND = C_CLASS_DEF_LOC or L_KIND = C_CLASS_DEF_GLOB.
      if L_KIND = C_CLASS_DEF_GLOB.
        L_CLS_IDS        = GLOB_CLS_IDS.
        L_CLS_CODE       = C_GLOB_CLS_CODE.
        L_TYPE_IDS       = GLOB_CLS_TYPE_IDS.
        L_TYPE_CODE      = C_GLOB_CLS_TYPE_CODE.
        L_CONST_IDS      = GLOB_CLS_CONSTANT_IDS.
        L_CONST_CODE     = C_GLOB_CLS_CONSTANT_CODE.
        L_DATA_IDS       = GLOB_CLS_DATA_IDS.
        L_DATA_CODE      = C_GLOB_CLS_DATA_CODE.
        L_CLASSDATA_IDS  = GLOB_CLS_CLASSDATA_IDS.
        L_CLASSDATA_CODE = C_GLOB_CLS_CLASSDATA_CODE.
        L_EVENT_IDS      = GLOB_CLS_EVENT_IDS.
        L_EVENT_CODE     = C_GLOB_CLS_EVENT_CODE.
        L_METH_IDS       = GLOB_CLS_METH_IDS.
        L_METH_CODE      = C_GLOB_CLS_METH_CODE.
      else.
        L_CLS_IDS        = LOC_CLS_IDS.
        L_CLS_CODE       = C_LOC_CLS_CODE.
        L_TYPE_IDS       = LOC_CLS_TYPE_IDS.
        L_TYPE_CODE      = C_LOC_CLS_TYPE_CODE.
        L_CONST_IDS      = LOC_CLS_CONSTANT_IDS.
        L_CONST_CODE     = C_LOC_CLS_CONSTANT_CODE.
        L_DATA_IDS       = LOC_CLS_DATA_IDS.
        L_DATA_CODE      = C_LOC_CLS_DATA_CODE.
        L_CLASSDATA_IDS  = LOC_CLS_CLASSDATA_IDS.
        L_CLASSDATA_CODE = C_LOC_CLS_CLASSDATA_CODE.
        L_EVENT_IDS      = LOC_CLS_EVENT_IDS.
        L_EVENT_CODE     = C_LOC_CLS_EVENT_CODE.
        L_METH_IDS       = LOC_CLS_METH_IDS.
        L_METH_CODE      = C_LOC_CLS_METH_CODE.
      endif.

      L_NAME = GET_TOKEN_REL( 2 ).
      if L_NAME not in L_CLS_IDS and IGNORE = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).

        INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                P_SUB_OBJ_NAME = L_INCLUDE
                P_LINE         = L_LINE
                P_COLUMN       = L_COLUMN
                P_TEST         = C_MY_NAME
                P_CODE         = L_CLS_CODE
                P_PARAM_1      = TOKEN_WA-STR ).
      endif.

      do.
        read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
        if SY-SUBRC <> 0.
          exit.
        endif.
        add 1 to STATEMENT_INDEX.
        check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
        check STATEMENT_WA-TYPE na 'EMPSDRG'.

        case KEYWORD( ).
          when 'PUBLIC'.
          when 'PROTECTED'.
          when 'PRIVATE'.
          when 'INCLUDE'.
          when 'CLASS'.
          when 'INTERFACE'.
          when 'INTERFACES'.
          when 'TYPES'.
            CHECK_DATA_TYPE( P_IDS = L_TYPE_IDS      P_CODE = L_TYPE_CODE ).
          when 'CONSTANTS'.
            CHECK_DATA_TYPE( P_IDS = L_CONST_IDS     P_CODE = L_CONST_CODE ).
          when 'DATA'.
            CHECK_DATA_TYPE( P_IDS = L_DATA_IDS      P_CODE = L_DATA_CODE ).
          when 'CLASS-DATA'.
            CHECK_DATA_TYPE( P_IDS = L_CLASSDATA_IDS P_CODE = L_CLASSDATA_CODE ).
          when 'EVENTS' or 'CLASS-EVENTS'.
            CHECK_EVENTS( P_IDS = L_EVENT_IDS        P_CODE = L_EVENT_CODE ).
          when 'METHODS' or 'CLASS-METHODS'.
            if STRLEN( L_NAME ) < 2 or L_NAME(2) <> 'CX'.
              CHECK_METHODS( P_IDS = L_METH_IDS      P_CODE = L_METH_CODE ).
            endif.
          when 'ALIASES'.
          when 'TYPE-POOLS'.
          when 'ENDCLASS'.
            return.
          when others.
            L_INCLUDE  = GET_INCLUDE( ).
            L_LINE     = GET_LINE_REL( 1 ).
            L_COLUMN   = GET_COLUMN_REL( 1 ).
            raise exception type LCX_EXCEPTION
              exporting
                P_INCLUDE = L_INCLUDE
                P_LINE    = L_LINE
                P_COLUMN  = L_COLUMN.
        endcase.
      enddo.

    elseif L_KIND = C_CLASS_IMPL.
      do.
        read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
        if SY-SUBRC <> 0.
          exit.
        endif.
        add 1 to STATEMENT_INDEX.
        check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
        check STATEMENT_WA-TYPE na 'EMPSDRG'.

        case KEYWORD( ).
          when 'ENDCLASS'.
            return.
          when 'METHOD'.
            CHECK_METHOD( ).
          when 'INCLUDE'.

          when 'DEFINE'.
            CHECK_DEFINE( ).

          when others.
            L_INCLUDE  = GET_INCLUDE( ).
            L_LINE     = GET_LINE_REL( 1 ).
            L_COLUMN   = GET_COLUMN_REL( 1 ).
            raise exception type LCX_EXCEPTION
              exporting
                P_INCLUDE = L_INCLUDE
                P_LINE    = L_LINE
                P_COLUMN  = L_COLUMN.
        endcase.
      enddo.

    endif.
  endmethod.


  method CHECK_DATA_TYPE.
    data:
      L_NAME    type STRING,
      L_INDEX   type I,
      L_LINE    type I,
      L_COLUMN  type TOKEN_COL,
      L_INCLUDE type PROGRAM,
      L_LEVEL   type I.

    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME = 'BEGIN' and GET_TOKEN_REL( 3 ) = 'OF'.
      L_NAME = GET_TOKEN_REL( 4 ).
      L_INDEX = 4.
      L_LEVEL = 1.
    else.
      L_INDEX = 2.
    endif.

    if L_NAME not in P_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( L_INDEX ).
      L_COLUMN   = GET_COLUMN_REL( L_INDEX ).
      INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                 P_SUB_OBJ_NAME = L_INCLUDE
                 P_LINE         = L_LINE
                 P_COLUMN       = L_COLUMN
                 P_TEST         = C_MY_NAME
                 P_CODE         = P_CODE
                 P_PARAM_1      = L_NAME ).
    endif.

    while L_LEVEL > 0.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSG'.

      case KEYWORD( ).
        when 'DATA' or 'TYPES' or 'CONSTANTS' or 'STATICS'.
          L_NAME = GET_TOKEN_REL( 2 ).
          if    L_NAME = 'BEGIN' and GET_TOKEN_REL( 3 ) = 'OF'.
            add 1 to L_LEVEL.
          elseif L_NAME = 'END'  and GET_TOKEN_REL( 3 ) = 'OF'.
            subtract 1 from L_LEVEL.
          endif.
      endcase.
    endwhile.

  endmethod.


  method CHECK_DEFINE.
    CHECK_GENERAL( P_IDS  = DEFINE_IDS
                   P_CODE = C_DEFINE_CODE ).


    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      if KEYWORD( ) = 'END-OF-DEFINITION'.
        exit.
      endif.
    enddo.

  endmethod.


  method CHECK_EVENTS.
    data:
      L_IDS     like LOC_CLS_DATA_IDS,
      L_CODE    type SCI_ERRC,
      L_NAME    type STRING,
      L_INCLUDE type PROGRAM,
      L_COLUMN  type TOKEN_COL.

    L_NAME = GET_TOKEN_REL( 2 ).

    if L_NAME not in P_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE = GET_INCLUDE( ).
      INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                 P_SUB_OBJ_NAME = L_INCLUDE
                 P_LINE         = TOKEN_WA-ROW
                 P_COLUMN       = TOKEN_WA-COL
                 P_TEST         = C_MY_NAME
                 P_CODE         = P_CODE
                 P_PARAM_1      = TOKEN_WA-STR ).
    endif.

    check QUALIFY_STATEMENT( P_REF_SCAN = REF_SCAN ) = 'X'.

    loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.
      if TOKEN_WA-TYPE = SANA_TOK_WORD.
        case TOKEN_WA-STR.
          when 'EXPORTING'.
            L_IDS = EVNT_EXP_IDS. L_CODE = C_EVNT_EXP_CODE.
        endcase.
      endif.

      if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF.
        if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
           TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE'.
          L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
          L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        endif.
        if L_NAME not in L_IDS and IGNORE = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                     P_SUB_OBJ_NAME = L_INCLUDE
                     P_LINE         = TOKEN_WA-ROW
                     P_COLUMN       = L_COLUMN
                     P_TEST         = C_MY_NAME
                     P_CODE         = L_CODE
                     P_PARAM_1      = L_NAME ).
        endif.

      endif.
    endloop.

  endmethod.


  method CHECK_FORM.
    data:
      L_DATA_IDS  like LOC_CLS_DATA_IDS,
      L_DATA_CODE type SCI_ERRC,
      L_NAME      type STRING,
      L_INCLUDE   type PROGRAM,
      L_LINE      type I,
      L_COLUMN    type TOKEN_COL.

    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME not in FORM_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).
      INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
              P_SUB_OBJ_NAME = L_INCLUDE
              P_LINE         = L_LINE
              P_COLUMN       = L_COLUMN
              P_TEST         = C_MY_NAME
              P_CODE         = C_FORM_CODE
              P_PARAM_1      = TOKEN_WA-STR ).
    endif.


    check QUALIFY_STATEMENT( P_REF_SCAN = REF_SCAN ) = 'X'.

    loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.

      if TOKEN_WA-TYPE = SANA_TOK_WORD.
        case TOKEN_WA-STR.
          when 'USING'.
            L_DATA_IDS = FORM_USI_IDS. L_DATA_CODE = C_FORM_USI_CODE.
          when 'CHANGING'.
            L_DATA_IDS = FORM_CHG_IDS. L_DATA_CODE = C_FORM_CHG_CODE.
          when 'TABLES'.
            L_DATA_IDS = FORM_TAB_IDS. L_DATA_CODE = C_FORM_TAB_CODE.
          when 'RAISING'.
            exit.
        endcase.
      endif.

      if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF.
        if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
           TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE'.
          L_NAME = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
          L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        else.
          L_NAME = TOKEN_WA-STR.
          L_COLUMN = TOKEN_WA-COL.
        endif.
        if L_NAME not in L_DATA_IDS and IGNORE = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).

          INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                     P_SUB_OBJ_NAME = L_INCLUDE
                     P_LINE         = TOKEN_WA-ROW
                     P_COLUMN       = L_COLUMN
                     P_TEST         = C_MY_NAME
                     P_CODE         = L_DATA_CODE
                     P_PARAM_1      = L_NAME ).
        endif.

      endif.
    endloop.

    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSDRG'.

      case KEYWORD( ).
        when 'TYPES'.
          CHECK_DATA_TYPE( P_IDS = LOC_TYPE_IDS P_CODE = C_LOC_TYPE_CODE ).
        when 'CONSTANTS'.
          CHECK_DATA_TYPE( P_IDS = LOC_CONSTANT_IDS P_CODE = C_LOC_CONSTANT_CODE ).
        when 'DATA' or 'STATICS'.
          CHECK_DATA_TYPE( P_IDS = LOC_DATA_IDS P_CODE = C_LOC_DATA_CODE ).
        when 'FIELD-SYMBOLS'.
          CHECK_GENERAL( P_IDS = LOC_FS_IDS P_CODE = C_LOC_FS_CODE ).
        when 'RANGES'.
          CHECK_GENERAL( P_IDS = LOC_DATA_IDS P_CODE = C_LOC_DATA_CODE ).
        when 'DEFINE'.
          CHECK_DEFINE( ).
        when 'ENDFORM'.
          return.
        when 'SET'.
          CHECK_SET( ).
        when others.
          CHECK_ANY( ).

      endcase.
    enddo.


  endmethod.


  method CHECK_FUNCTION.
    data:
      L_NAME    type STRING,
      L_INCLUDE type PROGRAM,
      L_LINE    type I,
      L_COLUMN  type TOKEN_COL.

    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME not in FUNC_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).
      INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
              P_SUB_OBJ_NAME = L_INCLUDE
              P_LINE         = L_LINE
              P_COLUMN       = L_COLUMN
              P_TEST         = C_MY_NAME
              P_CODE         = C_FUNC_CODE
              P_PARAM_1      = L_NAME ).
    endif.

    CHECK_FUNCTION_SIGNATURE( L_NAME ).



    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSRDG'.

      case KEYWORD( ).
        when 'TYPES'.
          CHECK_DATA_TYPE( P_IDS = LOC_TYPE_IDS     P_CODE = C_LOC_TYPE_CODE ).
        when 'CONSTANTS'.
          CHECK_DATA_TYPE( P_IDS = LOC_CONSTANT_IDS P_CODE = C_LOC_CONSTANT_CODE ).
        when 'DATA' or 'STATICS'.
          CHECK_DATA_TYPE( P_IDS = LOC_DATA_IDS     P_CODE = C_LOC_DATA_CODE ).
        when 'FIELD-SYMBOLS'.
          CHECK_GENERAL( P_IDS = LOC_FS_IDS    P_CODE = C_LOC_FS_CODE ).
        when 'RANGES'.
          CHECK_GENERAL( P_IDS = LOC_DATA_IDS P_CODE = C_LOC_DATA_CODE ).
        when 'DEFINE'.
          CHECK_DEFINE( ).
        when 'ENDFUNCTION'.
          return.
        when 'SET'.
          CHECK_SET( ).
        when others.
          CHECK_ANY( ).

      endcase.
    enddo.


  endmethod.


  method CHECK_FUNCTION_POOL.
    data:
      L_LINE    type I,
      L_NAME    type STRING,
      L_INCLUDE type PROGRAM,
      L_COLUMN  type TOKEN_COL,
      L_POOL    type STRING,
      L_OFFSET  type I,
      L_LENGTH  type I.

    if PROGRAM_NAME(1) = '/'.
      if PROGRAM_NAME+1 cs '/'.
        L_LENGTH = SY-FDPOS + 2.
        L_OFFSET = SY-FDPOS + 6.
        concatenate PROGRAM_NAME(L_LENGTH) PROGRAM_NAME+L_OFFSET(*) into L_POOL.
      else.
        L_POOL = PROGRAM_NAME+4.
      endif.
    else.
      L_POOL = PROGRAM_NAME+4.
    endif.
    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME <> L_POOL.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).

      INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
              P_SUB_OBJ_NAME = L_INCLUDE
              P_LINE         = L_LINE
              P_COLUMN       = L_COLUMN
              P_TEST         = C_MY_NAME
              P_CODE         = C_FUNCTION_POOL
              P_PARAM_1      = TOKEN_WA-STR ).
    endif.



  endmethod.


  method CHECK_FUNCTION_SIGNATURE.
    data:
      L_PARAM_IDS   like LOC_CLS_DATA_IDS,
      L_PARAM_CODE  type SCI_ERRC,
      L_NAME        type STRING,
      L_INCLUDE     type PROGRAM,
      L_COLUMN      type TOKEN_COL,
      L_PNAME       type PROGRAM,
      L_INCNUM      type TFDIR-INCLUDE,
      L_REF_INCLUDE type ref to CL_CI_SOURCE_INCLUDE,
      L_REF_SCAN    type ref to CL_CI_SCAN.

    select single PNAME INCLUDE from  TFDIR into (L_PNAME, L_INCNUM)
           where  FUNCNAME  = P_FUNCTION  .

    L_INCLUDE = L_PNAME.
    replace 'SAPL' in L_INCLUDE with 'L'.
    concatenate L_INCLUDE '$' L_INCNUM into L_INCLUDE.

    L_REF_INCLUDE = CL_CI_SOURCE_INCLUDE=>CREATE( P_NAME = L_INCLUDE ).
    create object L_REF_SCAN exporting P_INCLUDE = L_REF_INCLUDE.
    loop at L_REF_SCAN->STATEMENTS from 2 into STATEMENT_WA.

      read table L_REF_SCAN->TOKENS index  STATEMENT_WA-FROM into TOKEN_WA.
      check TOKEN_WA-STR = 'FUNCTION'.

      if QUALIFY_STATEMENT( L_REF_SCAN ) <> 'X'. return. endif.

      loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.

        if TOKEN_WA-TYPE = SANA_TOK_WORD.
          case TOKEN_WA-STR.
            when 'IMPORTING'.
              L_PARAM_IDS = FUNC_IMP_IDS. L_PARAM_CODE = C_FUNC_IMP_CODE.
            when 'EXPORTING'.
              L_PARAM_IDS = FUNC_EXP_IDS. L_PARAM_CODE = C_FUNC_EXP_CODE.
            when 'CHANGING'.
              L_PARAM_IDS = FUNC_CHG_IDS. L_PARAM_CODE = C_FUNC_CHG_CODE.
            when 'TABLES'.
              L_PARAM_IDS = FUNC_TAB_IDS. L_PARAM_CODE = C_FUNC_TAB_CODE.
            when 'EXCEPTIONS' or 'RAISING'.
              exit.
          endcase.
        endif.

        if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF.
          if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
            ( TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE' or
              TOKEN_WA-STR(TOKEN_WA-LEN1) = 'REFERENCE' ).
            L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
            L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
          else.
            L_NAME   = TOKEN_WA-STR.
            L_COLUMN = TOKEN_WA-COL.
          endif.
          if L_NAME not in L_PARAM_IDS and IGNORE = ABAP_FALSE.
            L_INCLUDE = GET_INCLUDE( P_REF_SCAN = L_REF_SCAN ).
            INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                       P_SUB_OBJ_NAME = L_INCLUDE
                       P_LINE         = TOKEN_WA-ROW
                       P_COLUMN       = L_COLUMN
                       P_TEST         = C_MY_NAME
                       P_CODE         = L_PARAM_CODE
                       P_PARAM_1      = L_NAME ).
          endif.

        endif.
      endloop.
      exit.
    endloop.

  endmethod.


  method CHECK_GENERAL.
    data:
      L_NAME    type STRING,
      L_LINE    type I,
      L_COLUMN  type TOKEN_COL,
      L_INCLUDE type PROGRAM.

    L_NAME = GET_TOKEN_REL( 2 ).

    if L_NAME not in P_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).
      INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                 P_SUB_OBJ_NAME = L_INCLUDE
                 P_LINE         = L_LINE
                 P_COLUMN       = L_COLUMN
                 P_TEST         = C_MY_NAME
                 P_CODE         = P_CODE
                 P_PARAM_1      = L_NAME ).
    endif.
  endmethod.


  method CHECK_INTERFACE.
    data:
      L_KIND           type I,
      L_NAME           type STRING,
      L_INTF_IDS       like LOC_CLS_DATA_IDS,
      L_INTF_CODE      type SCI_ERRC,
      L_TYPE_IDS       like LOC_CLS_DATA_IDS,
      L_TYPE_CODE      type SCI_ERRC,
      L_CONST_IDS      like LOC_CLS_DATA_IDS,
      L_CONST_CODE     type SCI_ERRC,
      L_DATA_IDS       like LOC_CLS_DATA_IDS,
      L_DATA_CODE      type SCI_ERRC,
      L_CLASSDATA_IDS  like LOC_CLS_CLASSDATA_IDS,
      L_CLASSDATA_CODE type SCI_ERRC,
      L_EVENT_IDS      like LOC_CLS_DATA_IDS,
      L_EVENT_CODE     type SCI_ERRC,
      L_METH_IDS       like LOC_CLS_CLASSDATA_IDS,
      L_METH_CODE      type SCI_ERRC,
      L_INCLUDE        type PROGRAM,
      L_LINE           type I,
      L_COLUMN         type TOKEN_COL.



    L_KIND = GET_INTERFACE_STMT_KIND( ).

    if L_KIND = C_CLASS_LOAD or L_KIND = C_CLASS_DEFER.
      return.
    endif.

    if L_KIND = C_CLASS_DEF_LOC or L_KIND = C_CLASS_DEF_GLOB.
      if L_KIND = C_CLASS_DEF_GLOB.
        L_INTF_IDS       = GLOB_INTF_IDS.
        L_INTF_CODE      = C_GLOB_INTF_CODE.
        L_TYPE_IDS       = GLOB_CLS_TYPE_IDS.
        L_TYPE_CODE      = C_GLOB_CLS_TYPE_CODE.
        L_CONST_IDS      = GLOB_CLS_CONSTANT_IDS.
        L_CONST_CODE     = C_GLOB_CLS_CONSTANT_CODE.
        L_DATA_IDS       = GLOB_CLS_DATA_IDS.
        L_DATA_CODE      = C_GLOB_CLS_DATA_CODE.
        L_CLASSDATA_IDS  = GLOB_CLS_CLASSDATA_IDS.
        L_CLASSDATA_CODE = C_GLOB_CLS_CLASSDATA_CODE.
        L_EVENT_IDS      = GLOB_CLS_EVENT_IDS.
        L_EVENT_CODE     = C_GLOB_CLS_EVENT_CODE.
        L_METH_IDS       = GLOB_CLS_METH_IDS.
        L_METH_CODE      = C_GLOB_CLS_METH_CODE.
      else.
        L_INTF_IDS       = LOC_INTF_IDS.
        L_INTF_CODE      = C_LOC_INTF_CODE.
        L_TYPE_IDS       = LOC_CLS_TYPE_IDS.
        L_TYPE_CODE      = C_LOC_CLS_TYPE_CODE.
        L_CONST_IDS      = LOC_CLS_CONSTANT_IDS.
        L_CONST_CODE     = C_LOC_CLS_CONSTANT_CODE.
        L_DATA_IDS       = LOC_CLS_DATA_IDS.
        L_DATA_CODE      = C_LOC_CLS_DATA_CODE.
        L_CLASSDATA_IDS  = LOC_CLS_CLASSDATA_IDS.
        L_CLASSDATA_CODE = C_LOC_CLS_CLASSDATA_CODE.
        L_EVENT_IDS      = LOC_CLS_EVENT_IDS.
        L_EVENT_CODE     = C_LOC_CLS_EVENT_CODE.
        L_METH_IDS       = LOC_CLS_METH_IDS.
        L_METH_CODE      = C_LOC_CLS_METH_CODE.
      endif.

      L_NAME = GET_TOKEN_REL( 2 ).
      if L_NAME not in L_INTF_IDS and IGNORE = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).

        INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                P_SUB_OBJ_NAME = L_INCLUDE
                P_LINE         = L_LINE
                P_COLUMN       = L_COLUMN
                P_TEST         = C_MY_NAME
                P_CODE         = L_INTF_CODE
                P_PARAM_1      = TOKEN_WA-STR ).
      endif.



      do.
        read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
        if SY-SUBRC <> 0.
          exit.
        endif.
        add 1 to STATEMENT_INDEX.
        check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
        check STATEMENT_WA-TYPE na 'EMPSDRG'.

        case KEYWORD( ).
          when 'INCLUDE'.
          when 'CLASS'.
          when 'INTERFACE'.
          when 'INTERFACES'.
          when 'TYPES'.
            CHECK_DATA_TYPE( P_IDS = L_TYPE_IDS      P_CODE = L_TYPE_CODE ).
          when 'CONSTANTS'.
            CHECK_DATA_TYPE( P_IDS = L_CONST_IDS     P_CODE = L_CONST_CODE ).
          when 'DATA'.
            CHECK_DATA_TYPE( P_IDS = L_DATA_IDS      P_CODE = L_DATA_CODE ).
          when 'CLASS-DATA'.
            CHECK_DATA_TYPE( P_IDS = L_CLASSDATA_IDS P_CODE = L_CLASSDATA_CODE ).
          when 'EVENTS' or 'CLASS-EVENTS'.
            CHECK_EVENTS( P_IDS = L_EVENT_IDS        P_CODE = L_EVENT_CODE ).
          when 'METHODS' or 'CLASS-METHODS'.
            CHECK_METHODS( P_IDS = L_METH_IDS        P_CODE = L_METH_CODE ).
          when 'ALIASES'.
          when 'TYPE-POOLS'.
          when 'ENDINTERFACE'.
            return.
          when others.
            L_INCLUDE  = GET_INCLUDE( ).
            L_LINE     = GET_LINE_REL( 1 ).
            L_COLUMN   = GET_COLUMN_REL( 1 ).
            raise exception type LCX_EXCEPTION
              exporting
                P_INCLUDE = L_INCLUDE
                P_LINE    = L_LINE
                P_COLUMN  = L_COLUMN.
        endcase.
      enddo.

    endif.

  endmethod.


  method CHECK_METHOD.

    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSDRG'.

      case KEYWORD( ).
        when 'TYPES'.
          CHECK_DATA_TYPE( P_IDS = LOC_TYPE_IDS P_CODE = C_LOC_TYPE_CODE ).
        when 'CONSTANTS'.
          CHECK_DATA_TYPE( P_IDS = LOC_CONSTANT_IDS P_CODE = C_LOC_CONSTANT_CODE ).
        when 'DATA' or 'STATICS'.
          CHECK_DATA_TYPE( P_IDS = LOC_DATA_IDS P_CODE = C_LOC_DATA_CODE ).
        when 'FIELD-SYMBOLS'.
          CHECK_GENERAL( P_IDS = LOC_FS_IDS P_CODE = C_LOC_FS_CODE ).
        when 'DEFINE'.
          CHECK_DEFINE( ).
        when 'ENDMETHOD'.
          return.
        when 'SET'.
          CHECK_SET( ).
        when others.
          CHECK_ANY( ).

      endcase.
    enddo.


  endmethod.


  method CHECK_METHODS.
    data:
      L_IDS     like LOC_CLS_DATA_IDS,
      L_CODE    type SCI_ERRC,
      L_LINE    type I,
      L_NAME    type STRING,
      L_INCLUDE type PROGRAM,
      L_COLUMN  type TOKEN_COL.


    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME not in P_IDS and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).
      INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
              P_SUB_OBJ_NAME = L_INCLUDE
              P_LINE         = L_LINE
              P_COLUMN       = L_COLUMN
              P_TEST         = C_MY_NAME
              P_CODE         = P_CODE
              P_PARAM_1      = TOKEN_WA-STR ).
    endif.

    check QUALIFY_STATEMENT( P_REF_SCAN = REF_SCAN ) = 'X'.

    loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.
      if TOKEN_WA-TYPE = SANA_TOK_WORD.
        case TOKEN_WA-STR.
          when 'IMPORTING'.
            L_IDS = METH_IMP_IDS. L_CODE = C_METH_IMP_CODE.
          when 'EXPORTING'.
            L_IDS = METH_EXP_IDS. L_CODE = C_METH_EXP_CODE.
          when 'CHANGING'.
            L_IDS = METH_CHG_IDS. L_CODE = C_METH_CHG_CODE.
          when 'RETURNING'.
            L_IDS = METH_RET_IDS. L_CODE = C_METH_RET_CODE.
          when 'EXCEPTIONS' or 'RAISING'.
            return.
          when 'FOR'.
            return.
        endcase.
      endif.

      if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF.
        if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
           TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE'.
          L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
          L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        else.
          L_NAME = TOKEN_WA-STR.
          L_COLUMN  = TOKEN_WA-COL.
        endif.
        if L_NAME not in L_IDS and IGNORE = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          INFORM(    P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                     P_SUB_OBJ_NAME = L_INCLUDE
                     P_LINE         = TOKEN_WA-ROW
                     P_COLUMN       = L_COLUMN
                     P_TEST         = C_MY_NAME
                     P_CODE         = L_CODE
                     P_PARAM_1      = L_NAME ).
        endif.

      endif.
    endloop.

  endmethod.


  method CHECK_PROGRAM.
    data:
      L_LINE    type I,
      L_INCLUDE type PROGRAM,
      L_COLUMN  type TOKEN_COL,
      L_NAME    type STRING.

    L_NAME = GET_TOKEN_REL( 2 ).
    if  L_NAME <> PROGRAM_NAME.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).

      INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
              P_SUB_OBJ_NAME = L_INCLUDE
              P_LINE         = L_LINE
              P_COLUMN       = L_COLUMN
              P_TEST         = C_MY_NAME
              P_CODE         = C_PROGRAM
              P_PARAM_1      = TOKEN_WA-STR ).
    endif.

  endmethod.


  method CHECK_SET.

    check GET_TOKEN_REL( 2 ) = 'EXTENDED' and GET_TOKEN_REL( 3 ) = 'CHECK'.

    case GET_TOKEN_REL( 4 ).
      when 'OFF'.
        IGNORE = ABAP_TRUE.
      when 'ON'.
        IGNORE = ABAP_FALSE.

    endcase.

  endmethod.


  method CHECK_TYPEPOOL.

    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
    enddo.


  endmethod.


  method CLEAR.

    SUPER->CLEAR( ).


  endmethod.


  method CONSTRUCTOR.
    data:
      L_NAME like line of GLOB_DATA_IDS.

    SUPER->CONSTRUCTOR( ).

    DESCRIPTION       = '2.- Convención de nombres objetos internos'(000).
    CATEGORY          = 'ZCL_CI_CATEGORY_CYT'.
    ADD_OBJ_TYPE( C_TYPE_PROGRAM ).
*    HAS_DOCUMENTATION = 'X'.
*    HAS_ATTRIBUTES    = 'X'.
    ATTRIBUTES_OK     = 'X'.

    define FILL_NAME.
      L_NAME-SIGN   = &1.
      L_NAME-OPTION = &2.
      L_NAME-LOW    = &3.
      L_NAME-HIGH   = SPACE.
      append L_NAME to &4.
    end-of-definition.

    FILL_NAME 'I' 'CP' 'GV_*'  GLOB_TYPE_IDS.
    FILL_NAME 'I' 'CP' 'LV_*'  LOC_TYPE_IDS.
    FILL_NAME 'I' 'CP' 'GC_*'  GLOB_CONSTANT_IDS.
    FILL_NAME 'I' 'CP' 'LC_*'  LOC_CONSTANT_IDS.
    FILL_NAME 'I' 'CP' 'GD_*'  GLOB_DATA_IDS.
    FILL_NAME 'I' 'CP' 'LD_*'  LOC_DATA_IDS.
    FILL_NAME 'I' 'CP' '<GF_*' GLOB_FS_IDS.
    FILL_NAME 'I' 'CP' '<LF_*' LOC_FS_IDS.




    define FILL_MESSAGE.
      clear SMSG.
      SMSG-TEST = C_MY_NAME.
      SMSG-CODE = &1.        "message code
      SMSG-KIND = C_WARNING. "message priority
      SMSG-TEXT = &2.        "message text
      SMSG-PCOM = ''.        "pseudocomment
      SMSG-PCOM_ALT = ''.    "alternative pseudocomment
      insert SMSG into table SCIMESSAGES.
    end-of-definition.



    FILL_MESSAGE C_SYNTAX                  'Syntax-Fehler. Die Analyse wurde abgebrochen'(059).
    FILL_MESSAGE C_DEFINE_CODE             'Unerlaubter Name &1 für DEFINE'(013).
    FILL_MESSAGE C_GLOB_TYPE_CODE          'Unerlaubter Name &1 für TYPES (global)'(014).
    FILL_MESSAGE C_LOC_TYPE_CODE           'Unerlaubter Name &1 für TYPES (lokal)'(015).
    FILL_MESSAGE C_GLOB_CONSTANT_CODE      'Unerlaubter Name &1 für CONSTANTS (global)'(016).
    FILL_MESSAGE C_LOC_CONSTANT_CODE       'Unerlaubter Name &1 für CONSTANTS (lokal)'(017).
    FILL_MESSAGE C_GLOB_DATA_CODE          'Unerlaubter Name &1 für DATA (global)'(018).
    FILL_MESSAGE C_LOC_DATA_CODE           'Unerlaubter Name &1 für DATA/STATICS (lokal)'(019).
    FILL_MESSAGE C_GLOB_FS_CODE            'Unerlaubter Name &1 für FIELD-SYMBOLS (global)'(020).
    FILL_MESSAGE C_LOC_FS_CODE             'Unerlaubter Name &1 für FIELD-SYMBOLS (lokal)'(021).
    FILL_MESSAGE C_PARAM_CODE              'Unerlaubter Name &1 für PARAMETER(S)'(022).
    FILL_MESSAGE C_SELOPT_CODE             'Unerlaubter Name &1 für SELECT-OPTIONS'(023).
    FILL_MESSAGE C_FLDGRP_CODE             'Unerlaubter Name &1 für FIELD-GROUPS'(024).
    FILL_MESSAGE C_GLOB_INTF_CODE          'Unerlaubter Name &1 für INTERFACE (global)'(025).
    FILL_MESSAGE C_GLOB_CLS_CODE           'Unerlaubter Name &1 für CLASS (global)'(026).
    FILL_MESSAGE C_GLOB_CLS_TYPE_CODE      'Unerlaubter Name &1 für TYPES (globale Klasse)'(027).
    FILL_MESSAGE C_GLOB_CLS_CONSTANT_CODE  'Unerlaubter Name &1 für CONSTANTS (globale Klasse)'(028).
    FILL_MESSAGE C_GLOB_CLS_DATA_CODE      'Unerlaubter Name &1 für DATA (globale Klasse)'(029).
    FILL_MESSAGE C_GLOB_CLS_CLASSDATA_CODE 'Unerlaubter Name &1 für CLASS-DATA (globale Klasse)'(030).
    FILL_MESSAGE C_GLOB_CLS_EVENT_CODE     'Unerlaubter Name &1 für EVENTS (globale Klasse)'(031).
    FILL_MESSAGE C_GLOB_CLS_METH_CODE      'Unerlaubter Name &1 für METHODS (globale Klasse)'(032).
    FILL_MESSAGE C_LOC_INTF_CODE           'Unerlaubter Name &1 für INTERFACE (lokal)'(033).
    FILL_MESSAGE C_LOC_CLS_CODE            'Unerlaubter Name &1 für CLASS (lokal)'(034).
    FILL_MESSAGE C_LOC_CLS_TYPE_CODE       'Unerlaubter Name &1 für TYPES (lokale Klasse)'(035).
    FILL_MESSAGE C_LOC_CLS_CONSTANT_CODE   'Unerlaubter Name &1 für CONSTANTS (lokale Klasse)'(036).
    FILL_MESSAGE C_LOC_CLS_DATA_CODE       'Unerlaubter Name &1 für DATA (lokale Klasse)'(037).
    FILL_MESSAGE C_LOC_CLS_CLASSDATA_CODE  'Unerlaubter Name &1 für CLASS-DATA (lokale Klasse)'(038).
    FILL_MESSAGE C_LOC_CLS_EVENT_CODE      'Unerlaubter Name &1 für EVENTS (lokale Klasse)'(039).
    FILL_MESSAGE C_LOC_CLS_METH_CODE       'Unerlaubter Name &1 für METHODS (lokale Klasse)'(040).
    FILL_MESSAGE C_METH_IMP_CODE           'Unerlaubter Name &1 für IMPORTING-Parameter (METHODS)'(041).
    FILL_MESSAGE C_METH_EXP_CODE           'Unerlaubter Name &1 für EXPORTING-Parameter (METHODS)'(042).
    FILL_MESSAGE C_METH_CHG_CODE           'Unerlaubter Name &1 für CHANGING-Parameter  (METHODS)'(043).
    FILL_MESSAGE C_METH_RET_CODE           'Unerlaubter Name &1 für RETURNING-Parameter (METHODS)'(044).
    FILL_MESSAGE C_EVNT_EXP_CODE           'Unerlaubter Name &1 für EXPORTING-Parameter (EVENTS)'(058).
    FILL_MESSAGE C_FUNC_CODE               'Unerlaubter Name &1 für FUNCTION'(045).
    FILL_MESSAGE C_FUNC_IMP_CODE           'Unerlaubter Name &1 für IMPORTING-Parameter (FUNCTION)'(046).
    FILL_MESSAGE C_FUNC_EXP_CODE           'Unerlaubter Name &1 für EXPORTING-Parameter (FUNCTION)'(047).
    FILL_MESSAGE C_FUNC_CHG_CODE           'Unerlaubter Name &1 für CHANGING-Parameter  (FUNCTION)'(048).
    FILL_MESSAGE C_FUNC_TAB_CODE           'Unerlaubter Name &1 für TABLES-Parameter    (FUNCTION)'(049).
    FILL_MESSAGE C_FORM_CODE               'Unerlaubter Name &1 für FORM'(050).
    FILL_MESSAGE C_FORM_USI_CODE           'Unerlaubter Name &1 für USING-Parameter     (FORM)'(051).
    FILL_MESSAGE C_FORM_CHG_CODE           'Unerlaubter Name &1 für CHANGING-Parameter  (FORM)'(052).
    FILL_MESSAGE C_FORM_TAB_CODE           'Unerlaubter Name &1 für TABLES-Parameter    (FORM)'(053).
    FILL_MESSAGE C_PROGRAM                 'Falscher Name &1 PROGRAM/REPORT'(054).
    FILL_MESSAGE C_FUNCTION_POOL           'Falscher Name &1 FUNCTION-POOL'(055).












  endmethod.


  method GET_ATTRIBUTES.
    export
      DEFINE_IDS              = DEFINE_IDS
      GLOB_TYPE_IDS           = GLOB_TYPE_IDS
      LOC_TYPE_IDS            = LOC_TYPE_IDS
      GLOB_CONSTANT_IDS       = GLOB_CONSTANT_IDS
      LOC_CONSTANT_IDS        = LOC_CONSTANT_IDS
      GLOB_DATA_IDS           = GLOB_DATA_IDS
      LOC_DATA_IDS            = LOC_DATA_IDS
      GLOB_FS_IDS             = GLOB_FS_IDS
      LOC_FS_IDS              = LOC_FS_IDS
      PARAM_IDS               = PARAM_IDS
      SELOPT_IDS              = SELOPT_IDS
      FLDGRP_IDS              = FLDGRP_IDS
      GLOB_INTF_IDS           = GLOB_INTF_IDS
      GLOB_CLS_IDS            = GLOB_CLS_IDS
      GLOB_CLS_TYPE_IDS       = GLOB_CLS_TYPE_IDS
      GLOB_CLS_CONSTANT_IDS   = GLOB_CLS_CONSTANT_IDS
      GLOB_CLS_DATA_IDS       = GLOB_CLS_DATA_IDS
      GLOB_CLS_CLASSDATA_IDS  = GLOB_CLS_CLASSDATA_IDS
      GLOB_CLS_EVENT_IDS      = GLOB_CLS_EVENT_IDS
      GLOB_CLS_METH_IDS       = GLOB_CLS_METH_IDS
      LOC_INTF_IDS            = LOC_INTF_IDS
      LOC_CLS_IDS             = LOC_CLS_IDS
      LOC_CLS_TYPE_IDS        = LOC_CLS_TYPE_IDS
      LOC_CLS_CONSTANT_IDS    = LOC_CLS_CONSTANT_IDS
      LOC_CLS_DATA_IDS        = LOC_CLS_DATA_IDS
      LOC_CLS_CLASSDATA_IDS   = LOC_CLS_CLASSDATA_IDS
      LOC_CLS_EVENT_IDS       = LOC_CLS_EVENT_IDS
      LOC_CLS_METH_IDS        = LOC_CLS_METH_IDS
      METH_IMP_IDS            = METH_IMP_IDS
      METH_EXP_IDS            = METH_EXP_IDS
      METH_CHG_IDS            = METH_CHG_IDS
      METH_RET_IDS            = METH_RET_IDS
      EVNT_EXP_IDS            = EVNT_EXP_IDS
      FUNC_IDS                = FUNC_IDS
      FUNC_IMP_IDS            = FUNC_IMP_IDS
      FUNC_EXP_IDS            = FUNC_EXP_IDS
      FUNC_CHG_IDS            = FUNC_CHG_IDS
      FUNC_TAB_IDS            = FUNC_TAB_IDS
      FORM_IDS                = FORM_IDS
      FORM_USI_IDS            = FORM_USI_IDS
      FORM_CHG_IDS            = FORM_CHG_IDS
      FORM_TAB_IDS            = FORM_TAB_IDS
      SCAN_WD_COMPONENT       = SCAN_WD_COMPONENT
    to data buffer P_ATTRIBUTES.
  endmethod.


 method GET_CLASS_STMT_KIND.
    data:
      L_INDEX type I.

    if GET_TOKEN_REL( 3 ) = 'IMPLEMENTATION'.
      P_RESULT = C_CLASS_IMPL.
      return.
    endif.

    case GET_TOKEN_REL( 4 ).
      when 'LOAD'.
        P_RESULT = C_CLASS_LOAD.
        return.
      when 'DEFERRED'.
        P_RESULT = C_CLASS_LOAD.
        return.
      when 'LOCAL'.
        P_RESULT = C_CLASS_DEFER.
        return.
      when 'PUBLIC'.
        P_RESULT = C_CLASS_DEF_GLOB.
        return.
    endcase.

    L_INDEX = 4.
    do.
      case GET_TOKEN_REL( L_INDEX ).
        when 'PUBLIC'.
          P_RESULT = C_CLASS_DEF_GLOB.
          return.
        when 'CREATE'.
          add 1 to L_INDEX.
        when ''.
          P_RESULT = C_CLASS_DEF_LOC.
          return.
      endcase.
      add 1 to L_INDEX.
    enddo.

  endmethod.


  method GET_INTERFACE_STMT_KIND.

    case GET_TOKEN_REL( 3 ).
      when 'LOAD'.
        P_RESULT = C_CLASS_LOAD.
        return.
      when 'DEFERRED'.
        P_RESULT = C_CLASS_LOAD.
        return.
      when 'PUBLIC'.
        P_RESULT = C_CLASS_DEF_GLOB.
        return.
      when others.
        P_RESULT = C_CLASS_DEF_LOC.
        return.
    endcase.

  endmethod.


  method IF_CI_TEST~QUERY_ATTRIBUTES.

    data:
      L_ATTRIBUTES  type SCI_ATTTAB,
      L_ATTRIBUTE   like line of L_ATTRIBUTES,
      L_MESSAGE(72) type C.

    define FILL_ATT.
      get reference of &1 into L_ATTRIBUTE-REF.
      L_ATTRIBUTE-TEXT = &2.
      L_ATTRIBUTE-KIND = &3.
      append L_ATTRIBUTE to L_ATTRIBUTES.
    end-of-definition.

*-- fill attribute table
    FILL_ATT SY-INDEX                'Programm-Global'(001)             'G'.
    FILL_ATT DEFINE_IDS              'DEFINE'                           'S'.
    FILL_ATT GLOB_TYPE_IDS           'TYPES'                            'S'.
    FILL_ATT GLOB_CONSTANT_IDS       'CONSTANTS'                        'S'.
    FILL_ATT GLOB_DATA_IDS           'DATA'                             'S'.
    FILL_ATT GLOB_FS_IDS             'FIELD-SYMBOLS'                    'S'.
    FILL_ATT PARAM_IDS               'PARAMETER(S)'                     'S'.
    FILL_ATT SELOPT_IDS              'SELECT-OPTIONS'                   'S'.

    FILL_ATT SY-INDEX                'Prozedur-Lokal'(002)              'G'.
    FILL_ATT LOC_TYPE_IDS            'TYPES'                            'S'.
    FILL_ATT LOC_CONSTANT_IDS        'CONSTANTS'                        'S'.
    FILL_ATT LOC_DATA_IDS            'DATA'                             'S'.
    FILL_ATT LOC_FS_IDS              'FIELD-SYMBOLS'                    'S'.


    FILL_ATT SY-INDEX                'Globale Klassen/Interfaces'(003)  'G'.
    FILL_ATT GLOB_CLS_IDS            'CLASS'                            'S'.
    FILL_ATT GLOB_INTF_IDS           'INTERFACE'                        'S'.
    FILL_ATT GLOB_CLS_TYPE_IDS       'TYPE'                             'S'.
    FILL_ATT GLOB_CLS_CONSTANT_IDS   'CONSTANTS'                        'S'.
    FILL_ATT GLOB_CLS_DATA_IDS       'DATA'                             'S'.
    FILL_ATT GLOB_CLS_CLASSDATA_IDS  'CLASS-DATA'                       'S'.
    FILL_ATT GLOB_CLS_EVENT_IDS      'EVENTS'                           'S'.
    FILL_ATT GLOB_CLS_METH_IDS       'METHODS'                          'S'.

    FILL_ATT SY-INDEX                'Lokale Klassen/Interfaces'(004)   'G'.
    FILL_ATT LOC_CLS_IDS             'CLASS'                            'S'.
    FILL_ATT LOC_INTF_IDS            'INTERFACE '                       'S'.
    FILL_ATT LOC_CLS_TYPE_IDS        'TYPES'                            'S'.
    FILL_ATT LOC_CLS_CONSTANT_IDS    'CONSTANTS'                        'S'.
    FILL_ATT LOC_CLS_DATA_IDS        'DATA'                             'S'.
    FILL_ATT LOC_CLS_CLASSDATA_IDS   'CLASS-DATA'                       'S'.
    FILL_ATT LOC_CLS_EVENT_IDS       'EVENTS'                           'S'.
    FILL_ATT LOC_CLS_METH_IDS        'METHODS'                          'S'.

    FILL_ATT SY-INDEX                'Methoden-Parameter'(005)          'G'.
    FILL_ATT METH_IMP_IDS            'IMPORTING'                        'S'.
    FILL_ATT METH_EXP_IDS            'EXPORTING'                        'S'.
    FILL_ATT METH_CHG_IDS            'CHANGING'                         'S'.
    FILL_ATT METH_RET_IDS            'RETURNING'                        'S'.

    FILL_ATT SY-INDEX                'Ereignis-Parameter'(057)          'G'.
    FILL_ATT EVNT_EXP_IDS            'EXPORTING'                        'S'.

    FILL_ATT SY-INDEX                'Funktionen'(006)                  'G'.
    FILL_ATT FUNC_IDS                'FUNCTION'                         'S'.
    FILL_ATT FUNC_IMP_IDS            'IMPORTING-Parameter'(007)         'S'.
    FILL_ATT FUNC_EXP_IDS            'EXPORTING-Parameter'(008)         'S'.
    FILL_ATT FUNC_CHG_IDS            'CHANGING-Parameter'(009)          'S'.
    FILL_ATT FUNC_TAB_IDS            'TABLES-Parameter'(010)            'S'.

    FILL_ATT SY-INDEX                'Forms'(056)                       'G'.
    FILL_ATT FORM_IDS                'FORM'                             'S'.
    FILL_ATT FORM_USI_IDS            'USING-Parameter'(011)             'S'.
    FILL_ATT FORM_CHG_IDS            'CHANGING-Parameter'(009)          'S'.
    FILL_ATT FORM_TAB_IDS            'TABLES-Parameter'(010)            'S'.

    FILL_ATT SY-INDEX                'Einstellungen'(101)               'G'.
    FILL_ATT SCAN_WD_COMPONENT       'Web Dynpro scannen'(102)          'C'.



    if CL_CI_QUERY_ATTRIBUTES=>GENERIC(
                       P_NAME       = C_MY_NAME
                       P_TITLE      = 'Namenskonventionen für ABAP-Objekte'(012)
                       P_ATTRIBUTES = L_ATTRIBUTES
                       P_MESSAGE    = L_MESSAGE
                       P_DISPLAY    = P_DISPLAY ) = 'X'.
*-- = 'X' --> 'Exit' Button pressed on PopUp
      return.
    endif.


  endmethod.


  method PUT_ATTRIBUTES.
    import
      DEFINE_IDS              = DEFINE_IDS
      GLOB_TYPE_IDS           = GLOB_TYPE_IDS
      LOC_TYPE_IDS            = LOC_TYPE_IDS
      GLOB_CONSTANT_IDS       = GLOB_CONSTANT_IDS
      LOC_CONSTANT_IDS        = LOC_CONSTANT_IDS
      GLOB_DATA_IDS           = GLOB_DATA_IDS
      LOC_DATA_IDS            = LOC_DATA_IDS
      GLOB_FS_IDS             = GLOB_FS_IDS
      LOC_FS_IDS              = LOC_FS_IDS
      FLDGRP_IDS              = FLDGRP_IDS
      PARAM_IDS               = PARAM_IDS
      SELOPT_IDS              = SELOPT_IDS
      GLOB_INTF_IDS           = GLOB_INTF_IDS
      GLOB_CLS_IDS            = GLOB_CLS_IDS
      GLOB_CLS_TYPE_IDS       = GLOB_CLS_TYPE_IDS
      GLOB_CLS_CONSTANT_IDS   = GLOB_CLS_CONSTANT_IDS
      GLOB_CLS_DATA_IDS       = GLOB_CLS_DATA_IDS
      GLOB_CLS_CLASSDATA_IDS  = GLOB_CLS_CLASSDATA_IDS
      GLOB_CLS_EVENT_IDS      = GLOB_CLS_EVENT_IDS
      GLOB_CLS_METH_IDS       = GLOB_CLS_METH_IDS
      LOC_INTF_IDS            = LOC_INTF_IDS
      LOC_CLS_IDS             = LOC_CLS_IDS
      LOC_CLS_TYPE_IDS        = LOC_CLS_TYPE_IDS
      LOC_CLS_CONSTANT_IDS    = LOC_CLS_CONSTANT_IDS
      LOC_CLS_DATA_IDS        = LOC_CLS_DATA_IDS
      LOC_CLS_CLASSDATA_IDS   = LOC_CLS_CLASSDATA_IDS
      LOC_CLS_EVENT_IDS       = LOC_CLS_EVENT_IDS
      LOC_CLS_METH_IDS        = LOC_CLS_METH_IDS
      METH_IMP_IDS            = METH_IMP_IDS
      METH_EXP_IDS            = METH_EXP_IDS
      METH_CHG_IDS            = METH_CHG_IDS
      METH_RET_IDS            = METH_RET_IDS
      EVNT_EXP_IDS            = EVNT_EXP_IDS
      FUNC_IDS                = FUNC_IDS
      FUNC_IMP_IDS            = FUNC_IMP_IDS
      FUNC_EXP_IDS            = FUNC_EXP_IDS
      FUNC_CHG_IDS            = FUNC_CHG_IDS
      FUNC_TAB_IDS            = FUNC_TAB_IDS
      FORM_IDS                = FORM_IDS
      FORM_USI_IDS            = FORM_USI_IDS
      FORM_CHG_IDS            = FORM_CHG_IDS
      FORM_TAB_IDS            = FORM_TAB_IDS
      SCAN_WD_COMPONENT       = SCAN_WD_COMPONENT
    from data buffer P_ATTRIBUTES.
  endmethod.


  method QUALIFY_STATEMENT.

    clear QUALIFIED_TOKENS.

    append lines of P_REF_SCAN->TOKENS
           from STATEMENT_WA-FROM
           to   STATEMENT_WA-TO to QUALIFIED_TOKENS.

    STATEMENT_FROM = 1.
    STATEMENT_TO   = LINES( QUALIFIED_TOKENS ).


    call function 'RS_QUALIFY_ABAP_TOKENS_STR'
      exporting
        STATEMENT_TYPE        = STATEMENT_WA-TYPE
        INDEX_FROM            = STATEMENT_FROM
        INDEX_TO              = STATEMENT_TO
*       SIMPLIFIED            = 'X'
      changing
        STOKESX_TAB           = QUALIFIED_TOKENS
      exceptions
        ERROR_LOAD_PATTERN    = 1
        UNKNOWN_KEYWORD       = 2
        NO_MATCHING_STATEMENT = 3
        MEANINGLESS_STATEMENT = 4
        others                = 5.
    if SY-SUBRC = 0.
      P_RESULT = 'X'.
    endif.


  endmethod.


  method RUN.
    data:
      L_EXCP type ref to LCX_EXCEPTION.

    if OBJECT_TYPE = 'WDYN' and SCAN_WD_COMPONENT = ABAP_FALSE.
      return.
    endif.


    if REF_SCAN is initial.
      check GET( ) = 'X'.
    endif.

    check REF_SCAN->SUBRC = 0.

    STATEMENT_INDEX = 1.
    clear IGNORE.

    try.
        do.
          read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
          if SY-SUBRC <> 0. exit. endif.
          add 1 to STATEMENT_INDEX.
          check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*-- avoid native SQL
          check STATEMENT_WA-TYPE na 'EMPSDRG'.

          case KEYWORD( ).
            when 'PROGRAM' or 'REPORT'.
              CHECK_PROGRAM( ).
            when 'FUNCTION-POOL'.
              CHECK_FUNCTION_POOL( ).
            when 'TYPES'.
              CHECK_DATA_TYPE( P_IDS = GLOB_TYPE_IDS P_CODE = C_GLOB_TYPE_CODE ).
            when 'DATA'.
              CHECK_DATA_TYPE( P_IDS = GLOB_DATA_IDS P_CODE = C_GLOB_DATA_CODE ).
            when 'CONSTANTS'.
              CHECK_DATA_TYPE( P_IDS = GLOB_CONSTANT_IDS P_CODE = C_GLOB_CONSTANT_CODE ).
            when 'FIELD-GROUPS'.
              CHECK_GENERAL(   P_IDS = FLDGRP_IDS    P_CODE = C_FLDGRP_CODE ).
            when 'SELECT-OPTIONS'.
              CHECK_GENERAL(   P_IDS = SELOPT_IDS    P_CODE = C_SELOPT_CODE ).
            when 'PARAMETERS' or 'PARAMETER'.
              CHECK_GENERAL(   P_IDS = PARAM_IDS     P_CODE = C_PARAM_CODE ).
            when 'FIELD-SYMBOLS'.
              CHECK_GENERAL(   P_IDS = GLOB_FS_IDS   P_CODE = C_GLOB_FS_CODE ).
            when 'RANGES'.
              CHECK_GENERAL(   P_IDS = GLOB_DATA_IDS P_CODE = C_GLOB_DATA_CODE ).
            when 'CLASS'.
              CHECK_CLASS( ).
            when 'INTERFACE'.
              CHECK_INTERFACE( ).
            when 'FORM'.
              CHECK_FORM( ).
            when 'FUNCTION'.
              CHECK_FUNCTION( ).
            when 'TYPE-POOL'.
              CHECK_TYPEPOOL( ).
            when 'DEFINE'.
              CHECK_DEFINE( ).
            when 'SET'.
              CHECK_SET( ).
            when others.
              CHECK_ANY_IN_SCOPE(
                exporting
                  P_DATA_IDS = GLOB_DATA_IDS   P_DATA_CODE = C_GLOB_DATA_CODE
                  P_FS_IDS =   GLOB_FS_IDS     P_FS_CODE =   C_GLOB_FS_CODE ).
          endcase.
        enddo.
      catch LCX_EXCEPTION into L_EXCP.
        INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                P_SUB_OBJ_NAME = L_EXCP->INCLUDE
                P_LINE         = L_EXCP->LINE
                P_COLUMN       = L_EXCP->COLUMN
                P_TEST         = C_MY_NAME
                P_CODE         = C_SYNTAX ).
    endtry.
  endmethod.
ENDCLASS.
