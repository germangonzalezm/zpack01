class ZCL_CI_001 definition
  public
  inheriting from CL_CI_TEST_SCAN
  final
  create public

  global friends IF_CRM_OBJECT_CHECKER .

public section.

  types:
    begin of T_S_OPTION,
        SCAN_WEB_DYNPRO type ABAP_BOOL,
      end of T_S_OPTION .

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


private section.

  constants C_CLASS_DEFER type I value 3 ##NO_TEXT.     "
  constants C_CLASS_DEF_GLOB type I value 2 ##NO_TEXT.
  constants C_CLASS_DEF_LOC type I value 1 ##NO_TEXT.
  constants C_CLASS_FRIENDS type I value 6 ##NO_TEXT.
  constants C_CLASS_IMPL type I value 5 ##NO_TEXT.
  constants C_CLASS_LOAD type I value 4 ##NO_TEXT.
  constants:
    begin of C_CODE,
        DEFINE           type SCI_ERRC value 'DEFINE',
        FLDGRP           type SCI_ERRC value 'FLD_GRP',
        REPT_NAME_C      type SCI_ERRC value 'REPT_NAMEC',
        REPT_NAME        type SCI_ERRC value 'REPT_NAME',
        FORM_NAME        type SCI_ERRC value 'FORM_NAME',
        FORM_CHG         type SCI_ERRC value 'FORM_CHG',
        FORM_TAB         type SCI_ERRC value 'FORM_TAB',
        FORM_USI         type SCI_ERRC value 'FORM_USI',
        FUGR_NAME_C      type SCI_ERRC value 'FUGR_NAMEC',
        FUGR_NAME        type SCI_ERRC value 'FUGR_NAME',
        FUNC_NAME        type SCI_ERRC value 'FUNC',
        FUNC_CHG         type SCI_ERRC value 'FUNC_CHG',
        FUNC_EXP         type SCI_ERRC value 'FUNC_EXP',
        FUNC_IMP         type SCI_ERRC value 'FUNC_IMP',
        FUNC_TAB         type SCI_ERRC value 'FUNC_TAB',
        GLOB_CONSTANT    type SCI_ERRC value 'GLOB_CONS',
        GLOB_DATA        type SCI_ERRC value 'GLOB_DAT',
        GLOB_FS          type SCI_ERRC value 'GLOB_FS',
        GLOB_TYPE        type SCI_ERRC value 'GLOB_TYPE',
        LOCL_CONSTANT    type SCI_ERRC value 'LOCL_CONST',
        LOCL_DATA        type SCI_ERRC value 'LOCL_DAT',
        LOCL_DATA_STATIC type SCI_ERRC value 'LOCL_STA',
        LOCL_FS          type SCI_ERRC value 'LOCL_FS',
        LOCL_TYPE        type SCI_ERRC value 'LOCL_TYPE',
        CLAS_LOCL_NAME   type SCI_ERRC value 'CLAS_LOCL',
        CLAS_GLOB_NAME   type SCI_ERRC value 'CLAS_GLOB',
        CLAS_CONSTANT    type SCI_ERRC value 'CLAS_CONST',
        CLAS_DATA        type SCI_ERRC value 'CLAS_DAT',
        CLAS_CLASSDATA   type SCI_ERRC value 'CLAS_DATCL',
        CLAS_TYPE        type SCI_ERRC value 'CLAS_TYPE',
        INTF_LOCL_NAME   type SCI_ERRC value 'INTF_LOCL',
        INTF_GLOB_NAME   type SCI_ERRC value 'INTF_GLOB',
        INTF_CONSTANT    type SCI_ERRC value 'INTF_CONST',
        INTF_DATA        type SCI_ERRC value 'INTF_DAT',
        INTF_CLASSDATA   type SCI_ERRC value 'INTF_DATCL',
        INTF_TYPE        type SCI_ERRC value 'INTF_TYPE',
        METH_NAME        type SCI_ERRC value 'METH_NAME',
        METH_CHG         type SCI_ERRC value 'METH_CHG',
        METH_EXP         type SCI_ERRC value 'METH_EXP',
        METH_IMP         type SCI_ERRC value 'METH_IMP',
        METH_RET         type SCI_ERRC value 'METH_RET',
        EVNT_NAME        type SCI_ERRC value 'EVNT_NAME',
        EVNT_EXP         type SCI_ERRC value 'EVNT_EXP',
        PARAM            type SCI_ERRC value 'SCR_PARAM',
        SELOPT           type SCI_ERRC value 'SCR_SELOPT',
        SYNTAX           type SCI_ERRC value 'SYNERR',
        SYMBOL           type SCI_ERRC value 'SYMBOL',
      end of C_CODE .
  data M_SETUP type SCI_S_NAMING_CONVENTIONS_SETUP .
  data M_OPTION type T_S_OPTION .
  data QUALIFIED_TOKENS type STOKESX_TAB .
  data STATEMENT_FROM type I .
  data STATEMENT_INDEX type I .
  data STATEMENT_TO type I .
  data COMP type ref to CL_ABAP_COMPILER .
  data IGNORE type SYCHAR01 .

  methods RAISE_MESSAGE
    importing
      !P_INCLUDE type SOBJ_NAME
      !P_PREFIX type STRING
      !P_LINE type TOKEN_ROW
      !P_COLUMN type TOKEN_COL
      !P_CODE type SCI_ERRC
      !P_PARAM_1 type CSEQUENCE .
  methods CHECK_CLASS
    importing
      !P_PREFIX type STRING .
  methods CHECK_DATA
    importing
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_CODE type SCI_ERRC
      !P_PREFIX type STRING .
  methods CHECK_DATA_TYPE
    importing
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_CODE type SCI_ERRC
      !P_PREFIX type STRING
      !P_TAG type SCR_TAG .
  methods CHECK_DEFINE
    importing
      !P_PREFIX type STRING .
  methods CHECK_EVENTS
    importing
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_CODE type SCI_ERRC
      !P_PREFIX type STRING .
  methods CHECK_FORM
    importing
      !P_PREFIX type STRING .
  methods CHECK_FUNCTION
    importing
      !P_PREFIX type STRING .
  methods CHECK_FUNCTION_POOL
    importing
      !P_PREFIX type STRING .
  methods CHECK_FUNCTION_SIGNATURE
    importing
      !P_FUNCTION type STRING
      !P_PREFIX type STRING
      !P_PREFIX_2 type STRING .
  methods CHECK_GENERAL
    importing
      !P_PREFIX type STRING
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_CODE type SCI_ERRC .
  methods CHECK_INTERFACE
    importing
      !P_PREFIX type STRING .
  methods CHECK_METHOD
    importing
      !P_PREFIX type STRING .
  methods CHECK_METHODS
    importing
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_CODE type SCI_ERRC
      !P_PREFIX type STRING .
  methods CHECK_MODULE_LOCAL
    importing
      !P_PREFIX type STRING .
  methods CHECK_NAME
    importing
      !P_NAME type STRING
      !P_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_TAG type SCR_TAG
      !P_PREFIX type STRING
      !P_PREFIX_2 type STRING optional
    returning
      value(P_RESULT) type SYCHAR01 .
  methods CHECK_PROGRAM
    importing
      !P_PREFIX type STRING .
  methods CHECK_TYPEPOOL .
  methods GET_CLASS_STMT_KIND
    returning
      value(P_RESULT) type I .
  methods GET_INTERFACE_STMT_KIND
    returning
      value(P_RESULT) type I .
  methods INIT_PREFIXES .
  methods QUALIFY_STATEMENT
    importing
      !P_REF_SCAN type ref to CL_CI_SCAN
    returning
      value(P_RESULT) type SYCHAR01 .
  methods CHECK_AT_SELECTION_SCREEN
    importing
      !P_PREFIX type STRING .
  methods CHECK_ANY
    importing
      !P_PREFIX type STRING
      !P_DATA_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_DATA_CODE type SCI_ERRC
      !P_FS_IDS type SCI_T_REGULAR_EXPRESSIONS
      !P_FS_CODE type SCI_ERRC .
  methods CHECK_SET .
  methods GET_SUPER_CLASS
    importing
      value(P_TYPE_SYMBOL_CLASS) type ref to CL_ABAP_COMP_CLASS
    returning
      value(P_SUPER_CLASS) type STRING .
ENDCLASS.



CLASS ZCL_CI_001 IMPLEMENTATION.


  method CHECK_ANY.

    data:
      L_NAME    type STRING,
      L_COLUMN  type TOKEN_COL,
      L_INCLUDE type PROGRAM.

    loop at REF_SCAN->TOKENS from STATEMENT_WA-FROM to STATEMENT_WA-TO into TOKEN_WA.

      if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
         TOKEN_WA-STR(TOKEN_WA-LEN1) = 'DATA'.
        L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
        L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.

        if CHECK_NAME( P_TAG    = 'DA'
                       P_NAME   = L_NAME
                       P_IDS    = P_DATA_IDS
                       P_PREFIX = P_PREFIX  ) = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                         P_INCLUDE = L_INCLUDE
                         P_LINE    = TOKEN_WA-ROW
                         P_COLUMN  = L_COLUMN
                         P_CODE    = P_DATA_CODE
                         P_PARAM_1 = L_NAME ).
        endif.
      elseif TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
             TOKEN_WA-STR(TOKEN_WA-LEN1) = 'FIELD-SYMBOL'.
        L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
        L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.

        if CHECK_NAME( P_TAG    = 'DA'
                       P_NAME   = L_NAME
                       P_IDS    = P_FS_IDS
                       P_PREFIX = P_PREFIX  ) = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                         P_INCLUDE = L_INCLUDE
                         P_LINE    = TOKEN_WA-ROW
                         P_COLUMN  = L_COLUMN
                         P_CODE    = P_FS_CODE
                         P_PARAM_1 = L_NAME ).
        endif.
      endif.
    endloop.

  endmethod.


  method CHECK_AT_SELECTION_SCREEN.

    data:
      L_NAME   type STRING,
      L_PREFIX type STRING.

    L_NAME = GET_TOKEN_REL( 3 ).

    case GET_TOKEN_REL( 3 ).
      when 'OUTPUT'.
        L_NAME = CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_OUTPUT.
      when 'ON'.
        case GET_TOKEN_REL( 4 ).
          when 'END'.
            L_NAME = GET_TOKEN_REL( 6 ).
            concatenate CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_ON_END ':'  L_NAME into L_NAME.
          when 'BLOCK'.
            L_NAME = GET_TOKEN_REL( 5 ).
            concatenate CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_BLOCK ':'  L_NAME into L_NAME.
          when 'RADIOBUTTON'.
            L_NAME = GET_TOKEN_REL( 6 ).
            concatenate   CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_RADIO ':' L_NAME into L_NAME.
          when 'HELP-REQUEST'.
            L_NAME = GET_TOKEN_REL( 6 ).
            concatenate  CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_HELP ':'  L_NAME into L_NAME.
          when 'VALUE-REQUEST'.
            L_NAME = GET_TOKEN_REL( 6 ).
            concatenate  CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_VALUE ':' L_NAME into L_NAME.
          when 'EXIT-COMMAND'.
            L_NAME = CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_EXIT.
          when others.
            L_NAME = GET_TOKEN_REL( 4 ).
            concatenate CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN_ON ':' L_NAME into L_NAME.

        endcase.

      when others.
        L_NAME = CL_ABAP_COMPILER=>TAG_AT_SELECTION_SCREEN.

    endcase.

    concatenate P_PREFIX  '\' L_NAME into L_PREFIX.

    CHECK_MODULE_LOCAL( L_PREFIX ).

  endmethod.


  method CHECK_CLASS.

    data:
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I,
      L_KIND      type I,
      L_NAME      type STRING,
      L_CLS_CODE  type SCI_ERRC,
      L_INCLUDE   type PROGRAM,
      L_LINE      type I,
      L_COLUMN    type TOKEN_COL,
      L_PREFIX    type STRING,
      L_KEYWORD   like TOKEN_WA-STR.

    field-symbols:
      <L_NAME_PATTERNS> type SCI_T_REGULAR_EXPRESSIONS,
      <L_PATTERN>       type SCI_E_REGULAR_EXPRESSION.

    L_KIND = GET_CLASS_STMT_KIND( ).

    case L_KIND.

      when  C_CLASS_LOAD or C_CLASS_DEFER or C_CLASS_FRIENDS.
        return.

      when  C_CLASS_DEF_LOC or C_CLASS_DEF_GLOB.
        L_NAME = GET_TOKEN_REL( 2 ).

        if L_KIND = C_CLASS_DEF_GLOB.
          concatenate '\TY:' L_NAME into L_PREFIX.
          assign ME->M_SETUP-PATTERNS_CLAS_NAME_GLOBAL to <L_NAME_PATTERNS>.
          L_CLS_CODE = C_CODE-CLAS_GLOB_NAME.
        else.
          concatenate P_PREFIX '\TY:' L_NAME into L_PREFIX.
          assign ME->M_SETUP-PATTERNS_CLAS_NAME_LOCAL to <L_NAME_PATTERNS>.
          L_CLS_CODE = C_CODE-CLAS_LOCL_NAME.
        endif.

        " check class name
        if <L_NAME_PATTERNS> is not initial and IGNORE = ABAP_FALSE.
          L_HAS_MATCH = ABAP_FALSE.
          loop at <L_NAME_PATTERNS> assigning <L_PATTERN>.
            find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
            if SY-SUBRC = 0 and L_OFFSET = 0.
              L_HAS_MATCH = ABAP_TRUE.
              exit. " loop.
            endif.
          endloop.
          if L_HAS_MATCH = ABAP_FALSE.
            L_INCLUDE  = GET_INCLUDE( ).
            L_LINE     = GET_LINE_REL( 2 ).
            L_COLUMN   = GET_COLUMN_REL( 2 ).
            RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                           P_INCLUDE = L_INCLUDE
                           P_LINE    = L_LINE
                           P_COLUMN  = L_COLUMN
                           P_CODE    = L_CLS_CODE
                           P_PARAM_1 = L_NAME ).
          endif.
        endif.

        " check components
        do.
          read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
          if SY-SUBRC <> 0.
            exit.
          endif.
          add 1 to STATEMENT_INDEX.
          check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*    -- avoid native SQL
          check STATEMENT_WA-TYPE na 'EMPSDRG'.

          L_KEYWORD = KEYWORD( ).
          case L_KEYWORD.
            when 'PUBLIC'.
            when 'PROTECTED'.
            when 'PACKAGE'.
            when 'PRIVATE'.
            when 'INCLUDE'.
            when 'CLASS'.
            when 'INTERFACE'.
            when 'INTERFACES'.
            when 'TYPES'.
              CHECK_DATA_TYPE(
                P_IDS =    ME->M_SETUP-PATTERNS_CLAS_TYPE
                P_CODE =   C_CODE-CLAS_TYPE
                P_PREFIX = L_PREFIX
                P_TAG = 'TY' ).

            when 'CONSTANTS'.
              CHECK_DATA_TYPE(
                P_IDS =    ME->M_SETUP-PATTERNS_CLAS_CONSTANT
                P_CODE =   C_CODE-CLAS_CONSTANT
                P_PREFIX = L_PREFIX
                P_TAG =    'DA' ).

            when 'DATA'.
              CHECK_DATA_TYPE(
                P_IDS =    ME->M_SETUP-PATTERNS_CLAS_MEMBER
                P_CODE =   C_CODE-CLAS_DATA
                P_PREFIX = L_PREFIX
                P_TAG =    'DA' ).

            when 'CLASS-DATA'.
              CHECK_DATA_TYPE(
                P_IDS =    ME->M_SETUP-PATTERNS_CLAS_MEMBER_CLASS
                P_CODE =   C_CODE-CLAS_CLASSDATA
                P_PREFIX = L_PREFIX
                P_TAG = 'DA' ).

            when 'EVENTS' or 'CLASS-EVENTS'.
              CHECK_EVENTS(
                P_IDS =    ME->M_SETUP-PATTERNS_EVNT_NAME
                P_CODE =   C_CODE-EVNT_NAME
                P_PREFIX = L_PREFIX ).

            when 'METHODS' or 'CLASS-METHODS'.
              if STRLEN( L_NAME ) < 2 or L_NAME(2) <> 'CX'.
                CHECK_METHODS(
                  P_IDS =   ME->M_SETUP-PATTERNS_METD_NAME
                  P_CODE =  C_CODE-METH_NAME
                  P_PREFIX = L_PREFIX ).
              endif.
            when 'ALIASES'.
            when 'TYPE-POOLS'.
            when 'ENHANCEMENT-POINT'.
            when 'SET'.
            when 'SYNTAX-TRACE'.
            when 'ENDCLASS'.
              return.
            when others.
              L_INCLUDE  = GET_INCLUDE( ).
              L_LINE     = GET_LINE_REL( 1 ).
              L_COLUMN   = GET_COLUMN_REL( 1 ).
              raise exception type LCX_EXCEPTION
                exporting
                  P_CODE    = C_CODE-SYNTAX
                  P_INCLUDE = L_INCLUDE
                  P_LINE    = L_LINE
                  P_COLUMN  = L_COLUMN.

          endcase.
        enddo.

      when C_CLASS_IMPL.

        L_NAME    = GET_TOKEN_REL( 2 ).
        L_INCLUDE = GET_INCLUDE( ).
        translate L_INCLUDE(30) using '= '.
        if L_INCLUDE+30(1) = 'C' and L_INCLUDE(30) = L_NAME.
          concatenate '\TY:' L_NAME into L_PREFIX.
        else.
          concatenate P_PREFIX '\TY:' L_NAME into L_PREFIX.
        endif.
        do.
          read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
          if SY-SUBRC <> 0.
            exit.
          endif.
          add 1 to STATEMENT_INDEX.
          check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*    -- avoid native SQL
          check STATEMENT_WA-TYPE na 'EMPSDRG'.

          case KEYWORD( ).
            when 'ENDCLASS'.
              return.
            when 'METHOD'.
              CHECK_METHOD( L_PREFIX ).
            when 'INCLUDE'.
            when 'CLASS'.
            when 'INTERFACE'.
            when 'TYPE-POOLS'.

            when 'DEFINE'.
              CHECK_DEFINE( L_PREFIX ).

            when others.
              L_INCLUDE  = GET_INCLUDE( ).
              L_LINE     = GET_LINE_REL( 1 ).
              L_COLUMN   = GET_COLUMN_REL( 1 ).
              raise exception type LCX_EXCEPTION
                exporting
                  P_CODE    = C_CODE-SYNTAX
                  P_INCLUDE = L_INCLUDE
                  P_LINE    = L_LINE
                  P_COLUMN  = L_COLUMN.

          endcase.
        enddo.
    endcase.
  endmethod.


  method CHECK_DATA.
    data:
      L_NAME    type STRING,
      L_LINE    type I,
      L_COLUMN  type TOKEN_COL,
      L_INCLUDE type PROGRAM.

    L_NAME = GET_TOKEN_REL( 2 ).

    if CHECK_NAME( P_NAME = L_NAME P_IDS = P_IDS P_TAG = 'DA' P_PREFIX = P_PREFIX ) <> 'X'.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).

      RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                     P_INCLUDE = L_INCLUDE
                     P_LINE    = L_LINE
                     P_COLUMN  = L_COLUMN
                     P_CODE    = P_CODE
                     P_PARAM_1 = L_NAME ).
    endif.
  endmethod.


  method CHECK_DATA_TYPE.

    data:
      L_NAME    type STRING,
      L_INDEX   type I,
      L_LINE    type I,
      L_COLUMN  type TOKEN_COL,
      L_INCLUDE type PROGRAM,
      L_LEVEL   type I,
      L_KEYWORD like TOKEN_WA-STR,
      L_DUMMY   type STRING ##needed.
    check P_IDS is not initial.
    L_NAME = GET_TOKEN_REL( 2 ).

    split L_NAME at '(' into L_NAME L_DUMMY.

    if L_NAME = 'BEGIN' and GET_TOKEN_REL( 3 ) = 'OF'.
      L_NAME = GET_TOKEN_REL( 4 ).
      if L_NAME = 'COMMON' and GET_TOKEN_REL( 5 ) = 'PART'.
        return.
      endif.
      L_INDEX = 4.
      L_LEVEL = 1.
    elseif  L_NAME = 'END' and GET_TOKEN_REL( 3 ) = 'OF' and GET_TOKEN_REL( 4 ) = 'COMMON' and GET_TOKEN_REL( 5 ) = 'PART'.
      return.
    else.
      L_INDEX = 2.
    endif.


    if CHECK_NAME( P_NAME = L_NAME P_IDS = P_IDS P_TAG = P_TAG P_PREFIX = P_PREFIX ) <> 'X'.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( L_INDEX ).
      L_COLUMN   = GET_COLUMN_REL( L_INDEX ).

      RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                     P_INCLUDE = L_INCLUDE
                     P_LINE    = L_LINE
                     P_COLUMN  = L_COLUMN
                     P_CODE    = P_CODE
                     P_PARAM_1 = L_NAME ).
    endif.

    while L_LEVEL > 0.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*  -- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSG'.

      L_KEYWORD = KEYWORD( ).
      case L_KEYWORD.
        when 'DATA' or 'TYPES' or 'CONSTANTS' or 'STATICS' or 'CLASS-DATA'.
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

    CHECK_GENERAL( P_PREFIX = P_PREFIX
                   P_IDS    = ME->M_SETUP-PATTERNS_PROG_DEFINE
                   P_CODE   = C_CODE-DEFINE ).
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
      L_NAME      type STRING,
      L_INCLUDE   type PROGRAM,
      L_COLUMN    type TOKEN_COL,
      L_PREFIX    type STRING,
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I.

    field-symbols:
      <L_PATTERN>       type SCI_E_REGULAR_EXPRESSION.

    check IGNORE = ABAP_FALSE.

    L_NAME = GET_TOKEN_REL( 2 ).

    if P_IDS is not initial.

      loop at P_IDS assigning <L_PATTERN>.

        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. " loop.
        endif.
      endloop.

      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE = GET_INCLUDE( ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = TOKEN_WA-ROW
                       P_COLUMN  = TOKEN_WA-COL
                       P_CODE    = P_CODE
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

    concatenate P_PREFIX '\EV:' L_NAME into L_PREFIX.


    check QUALIFY_STATEMENT( REF_SCAN ) = 'X'.

    loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.

      if TOKEN_WA-TYPE = SANA_TOK_WORD.
        case TOKEN_WA-STR.
          when 'EXPORTING'.
            " fine
          when others.
            continue.
        endcase.
      endif.

      if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF.
        if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
           TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE'.
          L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
          L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        else.
          L_NAME = TOKEN_WA-STR.
          L_COLUMN = TOKEN_WA-COL.
        endif.

        L_HAS_MATCH = CHECK_NAME(
            P_TAG = 'DA'
            P_NAME = L_NAME
            P_IDS =  ME->M_SETUP-PATTERNS_EVNT_EXPORTING
            P_PREFIX = L_PREFIX  ).
        if L_HAS_MATCH = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).
          RAISE_MESSAGE( P_PREFIX  = L_PREFIX
                         P_INCLUDE = L_INCLUDE
                         P_LINE    = TOKEN_WA-ROW
                         P_COLUMN  = L_COLUMN
                         P_CODE    = C_CODE-EVNT_EXP
                         P_PARAM_1 = L_NAME ).
        endif.

      endif.
    endloop.

  endmethod.


  method CHECK_FORM.

    data:
      L_DATA_CODE  type SCI_ERRC,
      L_NAME       type STRING,
      L_INCLUDE    type PROGRAM,
      L_LINE       type I,
      L_COLUMN     type TOKEN_COL,
      L_PREFIX     type STRING,
      L_HAS_MATCH  type ABAP_BOOL,
      L_OFFSET     type I,
      L_DEFINITION type ABAP_BOOL.

    field-symbols:
      <L_PARAM_PATTERNS> type SCI_T_REGULAR_EXPRESSIONS,
      <L_PATTERN>        type SCI_E_REGULAR_EXPRESSION.

    check IGNORE = ABAP_FALSE.

    L_NAME = GET_TOKEN_REL( 2 ).

    if GET_TOKEN_REL( 3 ) = 'DEFINITION'.
      L_DEFINITION = ABAP_TRUE.
    endif.

    if ME->M_SETUP-PATTERNS_FORM_NAME is not initial.
      loop at ME->M_SETUP-PATTERNS_FORM_NAME assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop.
        endif.
      endloop.
      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = C_CODE-FORM_NAME
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

    concatenate P_PREFIX '\FO:' L_NAME into L_PREFIX.

    if QUALIFY_STATEMENT( REF_SCAN ) = 'X'.

      loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.

        if TOKEN_WA-TYPE = SANA_TOK_WORD.
          case TOKEN_WA-STR.
            when 'USING'.
              if ME->M_SETUP-PATTERNS_FORM_USING is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FORM_USING to <L_PARAM_PATTERNS>.
              endif.
              L_DATA_CODE = C_CODE-FORM_USI.
            when 'CHANGING'.
              if ME->M_SETUP-PATTERNS_FORM_CHANGING is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FORM_CHANGING to <L_PARAM_PATTERNS>.
              endif.
              L_DATA_CODE = C_CODE-FORM_CHG.
            when 'TABLES'.
              if ME->M_SETUP-PATTERNS_FORM_TABLE is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FORM_TABLE to <L_PARAM_PATTERNS>.
              endif.
              L_DATA_CODE = C_CODE-FORM_TAB.
            when 'EXCEPTIONS' or 'RAISING'.
              exit. "loop
          endcase.
        endif.

        if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF and <L_PARAM_PATTERNS> is assigned.
          if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
             TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE'.
            L_NAME = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
            L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
          else.
            L_NAME = TOKEN_WA-STR.
            L_COLUMN = TOKEN_WA-COL.
          endif.

          L_HAS_MATCH = CHECK_NAME(
            P_TAG = 'DA'
            P_NAME = L_NAME
            P_IDS =  <L_PARAM_PATTERNS>
            P_PREFIX = L_PREFIX  ).
          if L_HAS_MATCH = ABAP_FALSE.
            L_INCLUDE = GET_INCLUDE( ).
            RAISE_MESSAGE( P_PREFIX  = L_PREFIX
                           P_INCLUDE = L_INCLUDE
                           P_LINE    = TOKEN_WA-ROW
                           P_COLUMN  = L_COLUMN
                           P_CODE    = L_DATA_CODE
                           P_PARAM_1 = L_NAME ).
          endif.
        endif.
      endloop.
    endif.

    if L_DEFINITION = ABAP_FALSE.
      CHECK_MODULE_LOCAL( L_PREFIX ).
    endif.

  endmethod.


  method CHECK_FUNCTION.

    data:
      L_NAME      type STRING,
      L_INCLUDE   type PROGRAM,
      L_PREFIX    type STRING,
      L_LINE      type I,
      L_COLUMN    type TOKEN_COL,
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I.

    field-symbols:
      <L_PATTERN>       type SCI_E_REGULAR_EXPRESSION.

    check IGNORE = ABAP_FALSE.

    L_NAME = GET_TOKEN_REL( 2 ).

    if ME->M_SETUP-PATTERNS_FUNC_NAME is not initial.
      loop at ME->M_SETUP-PATTERNS_FUNC_NAME assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop.
        endif.
      endloop.
      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = C_CODE-FUNC_NAME
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

    concatenate '\FU:' L_NAME into L_PREFIX.

    CHECK_FUNCTION_SIGNATURE( P_FUNCTION = L_NAME P_PREFIX = L_PREFIX P_PREFIX_2 = P_PREFIX ).

    CHECK_MODULE_LOCAL( L_PREFIX ).

  endmethod.


  method CHECK_FUNCTION_POOL.

    data:
      L_LINE      type I,
      L_NAME      type STRING,
      L_INCLUDE   type PROGRAM,
      L_COLUMN    type TOKEN_COL,
      L_POOL      type STRING,
      L_OFFSET    type I,
      L_LENGTH    type I,
      L_HAS_MATCH type ABAP_BOOL.

    field-symbols:
      <L_PATTERN>        type SCI_E_REGULAR_EXPRESSION.

    check IGNORE = ABAP_FALSE.

    " consistency check
    if PROGRAM_NAME(1) = '/'.
      if PROGRAM_NAME+1 cs '/'.
        L_LENGTH = SY-FDPOS + 2.
        L_OFFSET   = SY-FDPOS + 6.
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
      RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                     P_INCLUDE = L_INCLUDE
                     P_LINE    = L_LINE
                     P_COLUMN  = L_COLUMN
                     P_CODE    = C_CODE-FUGR_NAME_C
                     P_PARAM_1 = TOKEN_WA-STR ).
    endif.

    " pattern check
    if ME->M_SETUP-PATTERNS_FUGR_NAME is not initial.
      loop at ME->M_SETUP-PATTERNS_FUGR_NAME assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop.
        endif.
      endloop.
      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = C_CODE-FUGR_NAME
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

  endmethod.


  method CHECK_FUNCTION_SIGNATURE.

    data:
      L_PARAM_CODE  type SCI_ERRC,
      L_NAME        type STRING,
      L_INCLUDE     type PROGRAM,
      L_COLUMN      type TOKEN_COL,
      L_PNAME       type PROGRAM,
      L_INCNUM      type TFDIR-INCLUDE,
      L_HAS_MATCH   type ABAP_BOOL,
      L_REF_INCLUDE type ref to CL_CI_SOURCE_INCLUDE,
      L_REF_SCAN    type ref to CL_CI_SCAN.

    field-symbols:
      <L_PARAM_PATTERNS> type SCI_T_REGULAR_EXPRESSIONS.

    check IGNORE = ABAP_FALSE.

    select single PNAME INCLUDE from  TFDIR into (L_PNAME, L_INCNUM)
           where  FUNCNAME  = P_FUNCTION .

    L_INCLUDE = L_PNAME.
    replace 'SAPL' in L_INCLUDE with 'L'.
    concatenate L_INCLUDE '$' L_INCNUM into L_INCLUDE.

    L_REF_INCLUDE = CL_CI_SOURCE_INCLUDE=>CREATE( P_NAME = L_INCLUDE ).
    create object L_REF_SCAN
      exporting
        P_INCLUDE = L_REF_INCLUDE.
    loop at L_REF_SCAN->STATEMENTS from 2 into STATEMENT_WA.

      read table L_REF_SCAN->TOKENS index STATEMENT_WA-FROM into TOKEN_WA.

      check TOKEN_WA-STR = 'FUNCTION'.

      if QUALIFY_STATEMENT( L_REF_SCAN ) <> 'X'. return. endif.

      loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.

        if TOKEN_WA-TYPE = SANA_TOK_WORD.
          case TOKEN_WA-STR.
            when 'IMPORTING'.
              if ME->M_SETUP-PATTERNS_FUNC_IMPORTING is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FUNC_IMPORTING to <L_PARAM_PATTERNS>.
              endif.
              L_PARAM_CODE = C_CODE-FUNC_IMP.
            when 'EXPORTING'.
              if ME->M_SETUP-PATTERNS_FUNC_EXPORTING is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FUNC_EXPORTING to <L_PARAM_PATTERNS>.
              endif.
              L_PARAM_CODE = C_CODE-FUNC_EXP.
            when 'CHANGING'.
              if ME->M_SETUP-PATTERNS_FUNC_CHANGING is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FUNC_CHANGING to <L_PARAM_PATTERNS>.
              endif.
              L_PARAM_CODE = C_CODE-FUNC_CHG.
            when 'TABLES'.
              if ME->M_SETUP-PATTERNS_FUNC_TABLES is initial.
                unassign <L_PARAM_PATTERNS>.
              else.
                assign ME->M_SETUP-PATTERNS_FUNC_TABLES to <L_PARAM_PATTERNS>.
              endif.
              L_PARAM_CODE = C_CODE-FUNC_TAB.
            when 'EXCEPTIONS'.
              exit.
          endcase.
        endif.

        if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF and <L_PARAM_PATTERNS> is assigned.
          if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
            ( TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE' or
              TOKEN_WA-STR(TOKEN_WA-LEN1) = 'REFERENCE' ).
            L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
            L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
          else.
            L_NAME   = TOKEN_WA-STR.
            L_COLUMN = TOKEN_WA-COL.
          endif.
          L_HAS_MATCH = CHECK_NAME(
            P_TAG = 'DA'
            P_NAME = L_NAME
            P_IDS =  <L_PARAM_PATTERNS>
            P_PREFIX   = P_PREFIX
            P_PREFIX_2 = P_PREFIX_2 ).
          if L_HAS_MATCH = ABAP_FALSE.
            L_INCLUDE = GET_INCLUDE( P_REF_SCAN = L_REF_SCAN ).
            RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                           P_INCLUDE = L_INCLUDE
                           P_LINE    = TOKEN_WA-ROW
                           P_COLUMN  = L_COLUMN
                           P_CODE    = L_PARAM_CODE
                           P_PARAM_1 = L_NAME ).
          endif.
        endif.
      endloop.
      exit.
    endloop.


  endmethod.


  method CHECK_GENERAL.

    data:
      L_NAME      type STRING,
      L_LINE      type I,
      L_COLUMN    type TOKEN_COL,
      L_INCLUDE   type PROGRAM,
      L_HAS_MATCH type ABAP_BOOL.

    field-symbols:
      <L_PATTERN> like line of P_IDS.

    check IGNORE = ABAP_FALSE.

    L_NAME = GET_TOKEN_REL( 2 ).

    if P_IDS is not initial.
      loop at P_IDS assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset SY-FDPOS ignoring case.
        if SY-SUBRC = 0 and SY-FDPOS = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop
        endif.
      endloop.

      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = P_CODE
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

  endmethod.


  method CHECK_INTERFACE.

    data:
      L_KIND      type I,
      L_NAME      type STRING,
      L_INTF_CODE type SCI_ERRC,
      L_INCLUDE   type PROGRAM,
      L_LINE      type I,
      L_COLUMN    type TOKEN_COL,
      L_PREFIX    type STRING,
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I.
    field-symbols:
      <L_NAME_PATTERNS> type SCI_T_REGULAR_EXPRESSIONS,
      <L_PATTERN>       type SCI_E_REGULAR_EXPRESSION.

    L_KIND = GET_INTERFACE_STMT_KIND( ).

    if L_KIND <> C_CLASS_DEF_LOC and L_KIND <> C_CLASS_DEF_GLOB.
      return.
    endif.
    L_NAME = GET_TOKEN_REL( 2 ).


    if L_KIND = C_CLASS_DEF_GLOB.
      assign ME->M_SETUP-PATTERNS_INTF_NAME_GLOBAL to <L_NAME_PATTERNS>.
      L_INTF_CODE      = C_CODE-INTF_GLOB_NAME.
      concatenate '\TY:' L_NAME into L_PREFIX.
    else.
      assign ME->M_SETUP-PATTERNS_INTF_NAME_LOCAL to <L_NAME_PATTERNS>.
      L_INTF_CODE      = C_CODE-INTF_LOCL_NAME.
      concatenate P_PREFIX '\TY:' L_NAME into L_PREFIX.
    endif.

*   check interface name
    if <L_NAME_PATTERNS> is not initial and IGNORE = ABAP_FALSE.
      L_HAS_MATCH = ABAP_FALSE.
      loop at <L_NAME_PATTERNS> assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop.
        endif.
      endloop.

      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = L_INTF_CODE
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

*   check components
    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*  -- avoid native SQL
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
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_INTF_TYPE
            P_CODE =   C_CODE-INTF_TYPE
            P_PREFIX = L_PREFIX
            P_TAG = 'TY' ).

        when 'CONSTANTS'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_INTF_CONSTANT
            P_CODE =   C_CODE-INTF_CONSTANT
            P_PREFIX = L_PREFIX
            P_TAG =    'DA' ).

        when 'DATA'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_INTF_MEMBER
            P_CODE =   C_CODE-INTF_DATA
            P_PREFIX = L_PREFIX
            P_TAG =    'DA' ).

        when 'CLASS-DATA'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_INTF_MEMBER_CLASS
            P_CODE =   C_CODE-INTF_CLASSDATA
            P_PREFIX = L_PREFIX
            P_TAG = 'DA' ).

        when 'EVENTS' or 'CLASS-EVENTS'.
          CHECK_EVENTS(
            P_IDS =    ME->M_SETUP-PATTERNS_EVNT_NAME
            P_CODE =   C_CODE-EVNT_NAME
            P_PREFIX = L_PREFIX ).

        when 'METHODS' or 'CLASS-METHODS'.
          if STRLEN( L_NAME ) < 2 or L_NAME(2) <> 'CX'.
            CHECK_METHODS(
              P_IDS =   ME->M_SETUP-PATTERNS_METD_NAME
              P_CODE =  C_CODE-METH_NAME
              P_PREFIX = L_PREFIX ).
          endif.
        when 'ALIASES'.
        when 'TYPE-POOLS'.
        when 'ENHANCEMENT-POINT'.
        when 'SET'.
        when 'SYNTAX-TRACE'.
        when 'ENDINTERFACE'.
          return.
        when others.
          L_INCLUDE  = GET_INCLUDE( ).
          L_LINE     = GET_LINE_REL( 1 ).
          L_COLUMN   = GET_COLUMN_REL( 1 ).
          raise exception type LCX_EXCEPTION
            exporting
              P_CODE    = C_CODE-SYNTAX
              P_INCLUDE = L_INCLUDE
              P_LINE    = L_LINE
              P_COLUMN  = L_COLUMN.

      endcase.
    enddo.

    " fix slin issue
    if P_PREFIX is initial.
      clear SY-SUBRC.
    endif.


  endmethod.


  method CHECK_METHOD.
    data:
      L_PREFIX    type STRING,
      L_NAME      type STRING,
      L_INTERFACE type STRING.

    L_NAME = GET_TOKEN_REL( 2 ).

    if L_NAME cs '~'.
      split L_NAME at '~' into L_INTERFACE L_NAME.
      concatenate P_PREFIX '\IN:' L_INTERFACE '\ME:' L_NAME into L_PREFIX.
    else.
      concatenate P_PREFIX '\ME:' L_NAME into L_PREFIX.
    endif.

    CHECK_MODULE_LOCAL( L_PREFIX ).

  endmethod.


  method CHECK_METHODS.

    data:
      L_CODE      type SCI_ERRC,
      L_LINE      type I,
      L_NAME      type STRING,
      L_INCLUDE   type PROGRAM,
      L_COLUMN    type TOKEN_COL,
      L_PREFIX    type STRING,
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I.

    field-symbols:
      <L_PARAM_PATTERNS> type SCI_T_REGULAR_EXPRESSIONS,
      <L_PATTERN>        type SCI_E_REGULAR_EXPRESSION.

    check IGNORE = ABAP_FALSE.

    L_NAME = GET_TOKEN_REL( 2 ).
    if P_IDS is not initial and IGNORE = ABAP_FALSE.
      loop at P_IDS assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop
        endif.
      endloop.

      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = P_CODE
                       P_PARAM_1 = L_NAME ).
      endif.
    endif.

    concatenate P_PREFIX '\ME:' L_NAME into L_PREFIX.

    check QUALIFY_STATEMENT( REF_SCAN ) = 'X'.

    loop at QUALIFIED_TOKENS from STATEMENT_FROM to STATEMENT_TO into TOKEN_WA.
      if TOKEN_WA-TYPE = SANA_TOK_WORD.
        case TOKEN_WA-STR.
          when 'IMPORTING'.
            if ME->M_SETUP-PATTERNS_METD_IMPORTING is initial.
              unassign <L_PARAM_PATTERNS>.
            else.
              assign ME->M_SETUP-PATTERNS_METD_IMPORTING to <L_PARAM_PATTERNS>.
            endif.
            L_CODE = C_CODE-METH_IMP.
          when 'EXPORTING'.
            if ME->M_SETUP-PATTERNS_METD_EXPORTING is initial.
              unassign <L_PARAM_PATTERNS>.
            else.
              assign ME->M_SETUP-PATTERNS_METD_EXPORTING to <L_PARAM_PATTERNS>.
            endif.
            L_CODE = C_CODE-METH_EXP.
          when 'CHANGING'.
            if ME->M_SETUP-PATTERNS_METD_CHANGING is initial.
              unassign <L_PARAM_PATTERNS>.
            else.
              assign ME->M_SETUP-PATTERNS_METD_CHANGING to <L_PARAM_PATTERNS>.
            endif.
            L_CODE = C_CODE-METH_CHG.
          when 'RETURNING'.
            if ME->M_SETUP-PATTERNS_METD_RETURNING is initial.
              unassign <L_PARAM_PATTERNS>.
            else.
              assign ME->M_SETUP-PATTERNS_METD_RETURNING to <L_PARAM_PATTERNS>.
            endif.
            L_CODE = C_CODE-METH_RET.
          when 'EXCEPTIONS' or 'RAISING'.
            return.
          when 'FOR'.
            return.
        endcase.
      endif.

      if TOKEN_WA-TYPE = SANA_TOK_FIELD_DEF and <L_PARAM_PATTERNS> is assigned.
        if TOKEN_WA-LEN3 > 0 and TOKEN_WA-LEN1 > 0 and
           ( TOKEN_WA-STR(TOKEN_WA-LEN1) = 'VALUE' or
             TOKEN_WA-STR(TOKEN_WA-LEN1) = 'REFERENCE' ).
          L_NAME   = TOKEN_WA-STR+TOKEN_WA-OFF3(TOKEN_WA-LEN3).
          L_COLUMN = TOKEN_WA-COL + TOKEN_WA-OFF3.
        else.
          L_NAME = TOKEN_WA-STR.
          L_COLUMN  = TOKEN_WA-COL.
        endif.

        L_HAS_MATCH =  CHECK_NAME(
          P_TAG = 'DA'
          P_NAME = L_NAME
          P_IDS = <L_PARAM_PATTERNS>
          P_PREFIX = L_PREFIX  ).
        if L_HAS_MATCH = ABAP_FALSE.
          L_INCLUDE = GET_INCLUDE( ).

          RAISE_MESSAGE( P_PREFIX  = L_PREFIX
                         P_INCLUDE = L_INCLUDE
                         P_LINE    = TOKEN_WA-ROW
                         P_COLUMN  = L_COLUMN
                         P_CODE    = L_CODE
                         P_PARAM_1 = L_NAME ).
        endif.
      endif.
    endloop.

  endmethod.


  method CHECK_MODULE_LOCAL.
    data:
      L_TOKEN type STRING.

    do.
      read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to STATEMENT_INDEX.
      check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
*  -- avoid native SQL
      check STATEMENT_WA-TYPE na 'EMPSDRG'.

      case KEYWORD( ).
        when 'TYPES'.
          CHECK_DATA_TYPE(
            P_IDS =   ME->M_SETUP-PATTERNS_PROG_TYPE_LOCAL
            P_CODE =  C_CODE-LOCL_TYPE
            P_PREFIX = P_PREFIX
            P_TAG = 'TY' ).

        when 'CONSTANTS'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_PROG_CONSTANT_LOCAL
            P_CODE =   C_CODE-LOCL_CONSTANT
            P_PREFIX = P_PREFIX
            P_TAG =    'DA' ).

        when 'DATA' or 'RANGES'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_PROG_DATA_LOCAL
            P_CODE =   C_CODE-LOCL_DATA
            P_PREFIX = P_PREFIX
            P_TAG =    'DA' ).

        when 'STATICS'.
          CHECK_DATA_TYPE(
            P_IDS =    ME->M_SETUP-PATTERNS_PROG_DATA_STATIC
            P_CODE =   C_CODE-LOCL_DATA_STATIC
            P_PREFIX = P_PREFIX
            P_TAG =    'DA' ).

        when 'FIELD-SYMBOLS'.
          CHECK_DATA(
            P_IDS =    ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_LOC
            P_CODE =   C_CODE-LOCL_FS
            P_PREFIX = P_PREFIX ).

        when 'DEFINE'.
          CHECK_DEFINE( P_PREFIX ).

        when 'ENDFORM' or 'ENDFUNCTION' or 'ENDMETHOD'.
          return.

        when 'FORM'
          or 'FUNCTION'
          or 'MODULE'
          or 'START-OF-SELECTION'
          or 'END-OF-SELECTION'
          or 'TOP-OF-PAGE'
          or 'END-OF-PAGE'
          or 'LOAD-OF-PROGRAM'
          or 'INITIALIZATION'.
          subtract 1 from STATEMENT_INDEX.
          return.

        when 'CLASS'.
          if GET_TOKEN_REL( 3 ) = 'DEFINITION'
            and ( GET_TOKEN_REL( 4 ) = 'LOAD'
               or GET_TOKEN_REL( 4 ) = 'DEFERRED' ).
          else.
            subtract 1 from STATEMENT_INDEX.
            return.
          endif.

        when 'INTERFACE'.
          if    GET_TOKEN_REL( 3 ) = 'LOAD'
             or GET_TOKEN_REL( 3 ) = 'DEFERRED'.
          else.
            subtract 1 from STATEMENT_INDEX.
            return.
          endif.


        when 'AT'.
          L_TOKEN = GET_TOKEN_REL( 2 ).
          if STRLEN( L_TOKEN ) > 2 and L_TOKEN(2) = 'PF'.
            subtract 1 from STATEMENT_INDEX.
            exit.
          else.
            case L_TOKEN.
              when 'USER-COMMAND'.
                subtract 1 from STATEMENT_INDEX.
                return.
              when 'LINE-SELECTION'.
                subtract 1 from STATEMENT_INDEX.
                return.
              when 'SELECTION-SCREEN'.
                subtract 1 from STATEMENT_INDEX.
                return.
            endcase.
          endif.

        when 'SET'.
          CHECK_SET( ).

        when others.
          CHECK_ANY( P_PREFIX    = P_PREFIX
                     P_DATA_IDS  = ME->M_SETUP-PATTERNS_PROG_DATA_LOCAL
                     P_DATA_CODE = C_CODE-LOCL_DATA
                     P_FS_IDS    = ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_LOC
                     P_FS_CODE   = C_CODE-LOCL_FS ).
      endcase.
    enddo.


  endmethod.


  method CHECK_NAME.
    data:
      L_TYPE_SYMBOL         type ref to CL_ABAP_COMP_TYPE,
      L_TYPE_SYMBOL_REF     type ref to CL_ABAP_COMP_REF_TYPE,
      L_TYPE_SYMBOL_CLASS   type ref to CL_ABAP_COMP_CLASS,
      L_TYPE_ALIAS_SYMBOL   type ref to CL_ABAP_COMP_ALIAS_TYPE,
      L_TABLE_SYMBOL        type ref to CL_ABAP_COMP_TABLE_TYPE,
      L_DATA_SYMBOL_GENERIC type ref to CL_ABAP_COMP_DATA_GENERIC,
      L_DATA_SYMBOL_SIMPLE  type ref to CL_ABAP_COMP_DATA,
      L_DATA_SYMBOL_TAB     type ref to CL_ABAP_COMP_TABLE_WITH_HEAD,
      L_TYPE                type STRING,
      L_TYPE2               type STRING,
      L_INCLUDE             type PROGRAM,
      L_LINE                type TOKEN_ROW,
      L_COLUMN              type TOKEN_COL,
      L_FULL_NAME           type STRING,
      L_FULL_NAME_2         type STRING,
      L_PATTERN             type STRING,
      L_OFFSET              type I,
      L_COUNT               type I.

    field-symbols:
      <L_PATTERN>            type SCI_E_REGULAR_EXPRESSION.

    if IGNORE = ABAP_TRUE or P_IDS is initial.
      P_RESULT = ABAP_TRUE.
      return.
    endif.

    concatenate P_PREFIX '\' P_TAG ':' P_NAME into L_FULL_NAME.
    if P_PREFIX_2 is supplied.
      concatenate P_PREFIX_2 '\' P_TAG ':' P_NAME into L_FULL_NAME_2.
    endif.

    if P_TAG = 'TY'.
      L_TYPE_SYMBOL ?= COMP->GET_SYMBOL_ENTRY( L_FULL_NAME ).
    elseif P_TAG = 'DA'.
      L_DATA_SYMBOL_GENERIC ?= COMP->GET_SYMBOL_ENTRY( L_FULL_NAME ).
      if L_DATA_SYMBOL_GENERIC is initial and L_FULL_NAME_2 is not initial.
        L_DATA_SYMBOL_GENERIC ?= COMP->GET_SYMBOL_ENTRY( L_FULL_NAME_2 ).
      endif.
      if L_DATA_SYMBOL_GENERIC is initial.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 1 ).
        L_COLUMN   = GET_COLUMN_REL( 1 ).

        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = C_CODE-SYMBOL
                       P_PARAM_1 = L_FULL_NAME ).
        P_RESULT = 'X'.
        return.

      endif.

      if  L_DATA_SYMBOL_GENERIC->NODE_KIND = CL_ABAP_COMP_DATA_GENERIC=>DATA_NODE_KIND_TABLE_WITH_HEAD.
        L_DATA_SYMBOL_TAB    ?= L_DATA_SYMBOL_GENERIC.
        L_DATA_SYMBOL_SIMPLE = L_DATA_SYMBOL_TAB->TABLE.
      else.
        L_DATA_SYMBOL_SIMPLE ?= L_DATA_SYMBOL_GENERIC.
      endif.


      L_TYPE_SYMBOL = L_DATA_SYMBOL_SIMPLE->TYPE.
    endif.


    if L_TYPE_SYMBOL is initial.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 1 ).
      L_COLUMN   = GET_COLUMN_REL( 1 ).
      RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                     P_INCLUDE = L_INCLUDE
                     P_LINE    = L_LINE
                     P_COLUMN  = L_COLUMN
                     P_CODE    = C_CODE-SYMBOL
                     P_PARAM_1 = L_FULL_NAME ).
      P_RESULT = 'X'.
      return.
    endif.

    while L_TYPE_SYMBOL->TYPE_KIND = CL_ABAP_COMP_TYPE=>TYPE_KIND_ALIAS.
      L_TYPE_ALIAS_SYMBOL ?= L_TYPE_SYMBOL.
      L_TYPE_SYMBOL        = L_TYPE_ALIAS_SYMBOL->ALIAS_TYPE.
    endwhile.

    case L_TYPE_SYMBOL->TYPE_KIND.

      when CL_ABAP_COMP_TYPE=>TYPE_KIND_ELEMENTARY.
        case L_TYPE_SYMBOL->FULL_NAME.
          when '\PT:ANY' or '\PT:DATA'.
            L_TYPE = ME->M_SETUP-PREFIX_GENERIC.
          when '\PT:EMPTYTYPE'.
            " handeled differently for field-symbols and data declaration
            L_TYPE =  ME->M_SETUP-PREFIX_GENERIC.
            L_TYPE2 = ME->M_SETUP-PREFIX_ELEMENTARY.

          when others.
            L_TYPE = ME->M_SETUP-PREFIX_ELEMENTARY.
        endcase.

      when CL_ABAP_COMP_TYPE=>TYPE_KIND_STRUCTURE
      or   CL_ABAP_COMP_TYPE=>TYPE_KIND_DDIC_DBTAB.
        L_TYPE = ME->M_SETUP-PREFIX_STRUCTURE.

      when CL_ABAP_COMP_TYPE=>TYPE_KIND_TABLE.
        L_TABLE_SYMBOL ?= L_TYPE_SYMBOL.
        case L_TABLE_SYMBOL->INDEX_KIND.
          when CL_ABAP_COMP_TABLE_TYPE=>INDEX_KIND_HASHED.
            L_TYPE = ME->M_SETUP-PREFIX_TABLE_HASHED.
          when CL_ABAP_COMP_TABLE_TYPE=>INDEX_KIND_SORTED.
            L_TYPE = ME->M_SETUP-PREFIX_TABLE_SORTED.
          when CL_ABAP_COMP_TABLE_TYPE=>INDEX_KIND_INDEX.
            L_TYPE = ME->M_SETUP-PREFIX_TABLE_INDEX.
          when CL_ABAP_COMP_TABLE_TYPE=>INDEX_KIND_STANDARD.
            L_TYPE = ME->M_SETUP-PREFIX_TABLE_STANDARD.
          when CL_ABAP_COMP_TABLE_TYPE=>INDEX_KIND_ANY.
            L_TYPE = ME->M_SETUP-PREFIX_TABLE_ANY.
        endcase.

      when CL_ABAP_COMP_TYPE=>TYPE_KIND_REFERENCE.
        L_TYPE_SYMBOL_REF ?= L_TYPE_SYMBOL.

        case L_TYPE_SYMBOL_REF->REF_TYPE->TYPE_KIND.
          when CL_ABAP_COMP_TABLE_TYPE=>TYPE_KIND_INTERFACE.
            L_TYPE = ME->M_SETUP-PREFIX_REF_INTERFACE.
          when CL_ABAP_COMP_TABLE_TYPE=>TYPE_KIND_CLASS.

            L_TYPE_SYMBOL_CLASS ?= L_TYPE_SYMBOL_REF->REF_TYPE.

            case GET_SUPER_CLASS( L_TYPE_SYMBOL_CLASS ).
              when '\TY:CX_ROOT'.
                L_TYPE = ME->M_SETUP-PREFIX_REF_CLASS_EXCEPTION.
              when '\TY:CL_BADI_BASE'.
                L_TYPE = ME->M_SETUP-PREFIX_REF_CLASS_BADI.
              when others.
                L_TYPE = ME->M_SETUP-PREFIX_REF_CLASS.
            endcase.

          when others.
            L_TYPE = ME->M_SETUP-PREFIX_REF_DATA.

        endcase.

        " when CL_ABAP_COMP_TYPE=>TYPE_KIND_INTERFACE.
        "  L_TYPE = ME->M_SETUP-PREFIX_INTERFACE.

        " when CL_ABAP_COMP_TYPE=>TYPE_KIND_CLASS.
        "   L_TYPE = ME->M_SETUP-PREFIX_CLASS.

      when others.
        return. "

    endcase.

    concatenate '(' L_TYPE ')' into L_TYPE.
    loop at P_IDS assigning <L_PATTERN>.

      L_PATTERN = <L_PATTERN>.
      replace all occurrences of '[:type:]' in L_PATTERN with L_TYPE.

      find regex L_PATTERN in P_NAME match offset L_OFFSET ignoring case.
      if SY-SUBRC = 0 and L_OFFSET = 0.
        P_RESULT = ABAP_TRUE.
        return.
      endif.

    endloop.

    if L_TYPE2 is not initial and L_TYPE2 <> L_TYPE.
      concatenate '(' L_TYPE2 ')' into L_TYPE2.
      loop at P_IDS assigning <L_PATTERN>.

        L_PATTERN = <L_PATTERN>.
        replace all occurrences of '[:type:]' in L_PATTERN with L_TYPE2.

        find regex L_PATTERN in P_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          P_RESULT = ABAP_TRUE.
          return.
        endif.

      endloop.
    endif.

    P_RESULT = ABAP_FALSE.

  endmethod.


  method CHECK_PROGRAM.
    data:
      L_LINE      type I,
      L_INCLUDE   type PROGRAM,
      L_COLUMN    type TOKEN_COL,
      L_NAME      type STRING,
      L_HAS_MATCH type ABAP_BOOL,
      L_OFFSET    type I.

    field-symbols:
      <L_PATTERN>        type SCI_E_REGULAR_EXPRESSION.


    " Check Mismatch between TADIR and local Name
    L_NAME = GET_TOKEN_REL( 2 ).
    if L_NAME <> PROGRAM_NAME and IGNORE = ABAP_FALSE.
      L_INCLUDE  = GET_INCLUDE( ).
      L_LINE     = GET_LINE_REL( 2 ).
      L_COLUMN   = GET_COLUMN_REL( 2 ).
      RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                     P_INCLUDE = L_INCLUDE
                     P_LINE    = L_LINE
                     P_COLUMN  = L_COLUMN
                     P_CODE    = C_CODE-REPT_NAME_C
                     P_PARAM_1 = TOKEN_WA-STR ).
    endif.

    " Check if name satisfies pattern
    if ME->M_SETUP-PATTERNS_REPT_NAME is not initial.
      loop at ME->M_SETUP-PATTERNS_REPT_NAME assigning <L_PATTERN>.
        find regex <L_PATTERN> in L_NAME match offset L_OFFSET ignoring case.
        if SY-SUBRC = 0 and L_OFFSET = 0.
          L_HAS_MATCH = ABAP_TRUE.
          exit. "loop.
        endif.
      endloop.
      if L_HAS_MATCH = ABAP_FALSE.
        L_INCLUDE  = GET_INCLUDE( ).
        L_LINE     = GET_LINE_REL( 2 ).
        L_COLUMN   = GET_COLUMN_REL( 2 ).
        RAISE_MESSAGE( P_PREFIX  = P_PREFIX
                       P_INCLUDE = L_INCLUDE
                       P_LINE    = L_LINE
                       P_COLUMN  = L_COLUMN
                       P_CODE    = C_CODE-REPT_NAME
                       P_PARAM_1 = L_NAME ).
      endif.
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

    CL_ABAP_COMPILER=>CLEAR_CACHE( ).

  endmethod.


  method CONSTRUCTOR.

    data:
      L_PATTERN type SCI_E_REGULAR_EXPRESSION.

    SUPER->CONSTRUCTOR( ).

    DESCRIPTION       = '1.- Valida Nombre de Programa'(000).
    CATEGORY          = 'ZCL_CI_CATEGORY_CYT'.
      POSITION           = '001'.
    ADD_OBJ_TYPE( C_TYPE_PROGRAM ).
    HAS_DOCUMENTATION = 'X'.
    USES_CHECKSUM     = 'X'.

    define FILL_PATTERN.
      L_PATTERN   = &1 ##NO_TEXT.
      insert L_PATTERN into table ME->M_SETUP-&2.
    end-of-definition.

    FILL_PATTERN:
      'ZZMM*'                    PATTERNS_REPT_NAME,
      'ZZSD*'                    PATTERNS_REPT_NAME,
      'ZZFI*'                    PATTERNS_REPT_NAME.

    define FILL_MESSAGE.
      clear SMSG.
      SMSG-TEST = ME->MYNAME.
      SMSG-CODE = &1.           "code
      SMSG-KIND = C_WARNING.    "priority
      SMSG-TEXT = &2.           "description
      SMSG-PCOM = ''.           "pseudo comment
      SMSG-PCOM_ALT = ''.       "alternative pseudo comment
      insert SMSG into table SCIMESSAGES.
    end-of-definition.

    FILL_MESSAGE C_CODE-DEFINE           'Unerlaubter Name &1 für DEFINE'(013).
    FILL_MESSAGE C_CODE-FLDGRP           'Unerlaubter Name &1 für FIELD-GROUPS'(024).
    FILL_MESSAGE C_CODE-FORM_NAME        'Unerlaubter Name &1 für FORM'(050).
    FILL_MESSAGE C_CODE-FORM_CHG         'Unerlaubter Name &1 für CHANGING-Parameter (FORM)'(052).
    FILL_MESSAGE C_CODE-FORM_TAB         'Unerlaubter Name &1 für TABLES-Parameter (FORM)'(053).
    FILL_MESSAGE C_CODE-FORM_USI         'Unerlaubter Name &1 für USING-Parameter (FORM)'(051).
    FILL_MESSAGE C_CODE-FUNC_NAME        'Unerlaubter Name &1 für FUNCTION'(045).
    FILL_MESSAGE C_CODE-FUNC_CHG         'Unerlaubter Name &1 für CHANGING-Parameter (FUNCTION)'(048).
    FILL_MESSAGE C_CODE-FUNC_EXP         'Unerlaubter Name &1 für EXPORTING-Parameter (FUNCTION)'(047).
    FILL_MESSAGE C_CODE-FUNC_IMP         'Unerlaubter Name &1 für IMPORTING-Parameter (FUNCTION)'(046).
    FILL_MESSAGE C_CODE-FUNC_TAB         'Unerlaubter Name &1 für TABLES-Parameter (FUNCTION)'(049).
    FILL_MESSAGE C_CODE-GLOB_CONSTANT    'Unerlaubter Name &1 für CONSTANTS (global)'(016).
    FILL_MESSAGE C_CODE-GLOB_DATA        'Unerlaubter Name &1 für DATA (global)'(018).
    FILL_MESSAGE C_CODE-GLOB_FS          'Unerlaubter Name &1 für FIELD-SYMBOLS (global)'(020).
    FILL_MESSAGE C_CODE-GLOB_TYPE        'Unerlaubter Name &1 für TYPES (global)'(014).
    FILL_MESSAGE C_CODE-LOCL_CONSTANT    'Unerlaubter Name &1 für CONSTANTS (lokal)'(017).
    FILL_MESSAGE C_CODE-LOCL_DATA        'Unerlaubter Name &1 für DATA/RANGES (lokal)'(019).
    FILL_MESSAGE C_CODE-LOCL_DATA_STATIC 'Unerlaubter Name &1 für STATICS (lokal)'(E20).
    FILL_MESSAGE C_CODE-LOCL_FS          'Unerlaubter Name &1 für FIELD-SYMBOLS (lokal)'(021).
    FILL_MESSAGE C_CODE-LOCL_TYPE        'Unerlaubter Name &1 für TYPES (lokal)'(015).
    FILL_MESSAGE C_CODE-CLAS_GLOB_NAME   'Unerlaubter Name &1 für CLASS (global)'(026).
    FILL_MESSAGE C_CODE-CLAS_LOCL_NAME   'Unerlaubter Name &1 für CLASS (lokal)'(034).
    FILL_MESSAGE C_CODE-CLAS_CONSTANT    'Unerlaubter Name &1 für Konstanten von Klassen'(028).
    FILL_MESSAGE C_CODE-CLAS_DATA        'Unerlaubter Name &1 für Instanzattribute von Klassen'(029).
    FILL_MESSAGE C_CODE-CLAS_CLASSDATA   'Unerlaubter Name &1 für statischen Attributen von Klassen'(030).
    FILL_MESSAGE C_CODE-CLAS_TYPE        'Unerlaubter Name &1 für Typdefinition in Klassen'(027).
    FILL_MESSAGE C_CODE-INTF_GLOB_NAME   'Unerlaubter Name &1 für INTERFACE (global)'(025).
    FILL_MESSAGE C_CODE-INTF_LOCL_NAME   'Unerlaubter Name &1 für INTERFACE (lokal)'(033).
    FILL_MESSAGE C_CODE-INTF_CONSTANT    'Unerlaubter Name &1 für Konstanten von Interfaces'(I28).
    FILL_MESSAGE C_CODE-INTF_DATA        'Unerlaubter Name &1 für Instanzattribute von Interfaces'(I29).
    FILL_MESSAGE C_CODE-INTF_CLASSDATA   'Unerlaubter Name &1 für statischen Attributen von Interfaces'(I30).
    FILL_MESSAGE C_CODE-INTF_TYPE        'Unerlaubter Name &1 für Typdefinition in Interfaces'(I27).
    FILL_MESSAGE C_CODE-METH_NAME        'Unerlaubter Name &1 für METHODS'(032).
    FILL_MESSAGE C_CODE-METH_CHG         'Unerlaubter Name &1 für CHANGING-Parameter  (METHODS)'(043).
    FILL_MESSAGE C_CODE-METH_EXP         'Unerlaubter Name &1 für EXPORTING-Parameter (METHODS)'(042).
    FILL_MESSAGE C_CODE-METH_IMP         'Unerlaubter Name &1 für IMPORTING-Parameter (METHODS)'(041).
    FILL_MESSAGE C_CODE-METH_RET         'Unerlaubter Name &1 für RETURNING-Parameter (METHODS)'(044).
    FILL_MESSAGE C_CODE-EVNT_NAME        'Unerlaubter Name &1 für EVENTS'(031).
    FILL_MESSAGE C_CODE-EVNT_EXP         'Unerlaubter Name &1 für EXPORTING-Parameter (EVENTS)'(058).
    FILL_MESSAGE C_CODE-PARAM            'Unerlaubter Name &1 für PARAMETER(S)'(022).
    FILL_MESSAGE C_CODE-SELOPT           'Unerlaubter Name &1 für SELECT-OPTIONS'(023).
    FILL_MESSAGE C_CODE-FUGR_NAME        'Unerlaubter Name &1 FUNCTION-POOL'(55B).
    FILL_MESSAGE C_CODE-FUGR_NAME_C      'Inkonsistenter Name &1 FUNCTION-POOL'(055).
    FILL_MESSAGE C_CODE-REPT_NAME_C      'Inkonsistenter Name &1 PROGRAM/REPORT'(054).
    FILL_MESSAGE C_CODE-REPT_NAME        'Unerlaubter Name &1 PROGRAM/REPORT'(54B).
    FILL_MESSAGE C_CODE-SYNTAX           'Syntax-Fehler. Die Analyse wurde abgebrochen'(059).
    FILL_MESSAGE C_CODE-SYMBOL           'Symbol &1 konnte nicht aufgelöst werden'(060).

  endmethod.


  method GET_ATTRIBUTES.

    export
      PREFIX_ELEMENTARY  =               ME->M_SETUP-PREFIX_ELEMENTARY
      PREFIX_GENERIC  =                  ME->M_SETUP-PREFIX_GENERIC
      PREFIX_STRUCTURE =                 ME->M_SETUP-PREFIX_STRUCTURE
      PREFIX_TABLE_ANY =                 ME->M_SETUP-PREFIX_TABLE_ANY
      PREFIX_TABLE_HASHED =              ME->M_SETUP-PREFIX_TABLE_HASHED
      PREFIX_TABLE_INDEX =               ME->M_SETUP-PREFIX_TABLE_INDEX
      PREFIX_TABLE_STANDARD =            ME->M_SETUP-PREFIX_TABLE_STANDARD
      PREFIX_TABLE_SORTED =              ME->M_SETUP-PREFIX_TABLE_SORTED
      PREFIX_REF_DATA =                  ME->M_SETUP-PREFIX_REF_DATA
      PREFIX_REF_CLASS =                 ME->M_SETUP-PREFIX_REF_CLASS
      PREFIX_REF_CLASS_BADI =            ME->M_SETUP-PREFIX_REF_CLASS_BADI
      PREFIX_REF_CLASS_EXCEPTION =       ME->M_SETUP-PREFIX_REF_CLASS_EXCEPTION
      PREFIX_REF_INTERFACE =             ME->M_SETUP-PREFIX_REF_INTERFACE
      PATTERNS_CLAS_NAME_GLOBAL =        ME->M_SETUP-PATTERNS_CLAS_NAME_GLOBAL
      PATTERNS_CLAS_NAME_LOCAL =         ME->M_SETUP-PATTERNS_CLAS_NAME_LOCAL
      PATTERNS_CLAS_MEMBER =             ME->M_SETUP-PATTERNS_CLAS_MEMBER
      PATTERNS_CLAS_MEMBER_CLASS =       ME->M_SETUP-PATTERNS_CLAS_MEMBER_CLASS
      PATTERNS_CLAS_CONSTANT =           ME->M_SETUP-PATTERNS_CLAS_CONSTANT
      PATTERNS_CLAS_TYPE =               ME->M_SETUP-PATTERNS_CLAS_TYPE
      PATTERNS_INTF_NAME_GLOBAL =        ME->M_SETUP-PATTERNS_INTF_NAME_GLOBAL
      PATTERNS_INTF_NAME_LOCAL =         ME->M_SETUP-PATTERNS_INTF_NAME_LOCAL
      PATTERNS_INTF_MEMBER =             ME->M_SETUP-PATTERNS_INTF_MEMBER
      PATTERNS_INTF_MEMBER_CLASS =       ME->M_SETUP-PATTERNS_INTF_MEMBER_CLASS
      PATTERNS_INTF_CONSTANT =           ME->M_SETUP-PATTERNS_INTF_CONSTANT
      PATTERNS_INTF_TYPE =               ME->M_SETUP-PATTERNS_INTF_TYPE
      PATTERNS_METD_NAME =               ME->M_SETUP-PATTERNS_METD_NAME
      PATTERNS_METD_IMPORTING =          ME->M_SETUP-PATTERNS_METD_IMPORTING
      PATTERNS_METD_EXPORTING =          ME->M_SETUP-PATTERNS_METD_EXPORTING
      PATTERNS_METD_CHANGING =           ME->M_SETUP-PATTERNS_METD_CHANGING
      PATTERNS_METD_RETURNING =          ME->M_SETUP-PATTERNS_METD_RETURNING
      PATTERNS_EVNT_NAME =               ME->M_SETUP-PATTERNS_EVNT_NAME
      PATTERNS_EVNT_EXPORTING =          ME->M_SETUP-PATTERNS_EVNT_EXPORTING
      PATTERNS_FUGR_NAME =               ME->M_SETUP-PATTERNS_FUGR_NAME
      PATTERNS_FUNC_NAME =               ME->M_SETUP-PATTERNS_FUNC_NAME
      PATTERNS_FUNC_IMPORTING =          ME->M_SETUP-PATTERNS_FUNC_IMPORTING
      PATTERNS_FUNC_EXPORTING =          ME->M_SETUP-PATTERNS_FUNC_EXPORTING
      PATTERNS_FUNC_CHANGING =           ME->M_SETUP-PATTERNS_FUNC_CHANGING
      PATTERNS_FUNC_TABLES =             ME->M_SETUP-PATTERNS_FUNC_TABLES
      PATTERNS_REPT_NAME =               ME->M_SETUP-PATTERNS_REPT_NAME
      PATTERNS_PROG_DATA_LOCAL =         ME->M_SETUP-PATTERNS_PROG_DATA_LOCAL
      PATTERNS_PROG_DATA_GLOBAL =        ME->M_SETUP-PATTERNS_PROG_DATA_GLOBAL
      PATTERNS_PROG_DATA_STATIC =        ME->M_SETUP-PATTERNS_PROG_DATA_STATIC
      PATTERNS_PROG_FIELD_SYMBOL_LOC =   ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_LOC
      PATTERNS_PROG_FIELD_SYMBOL_GLB =   ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_GLB
      PATTERNS_PROG_CONSTANT_LOCAL =     ME->M_SETUP-PATTERNS_PROG_CONSTANT_LOCAL
      PATTERNS_PROG_CONSTANT_GLOBAL =    ME->M_SETUP-PATTERNS_PROG_CONSTANT_GLOBAL
      PATTERNS_PROG_FIELD_GROUP =        ME->M_SETUP-PATTERNS_PROG_FIELD_GROUP
      PATTERNS_PROG_SELSCR_OPTION =      ME->M_SETUP-PATTERNS_PROG_SELSCR_OPTION
      PATTERNS_PROG_SELSCR_PARAMETER =   ME->M_SETUP-PATTERNS_PROG_SELSCR_PARAMETER
      PATTERNS_PROG_TYPE_LOCAL =         ME->M_SETUP-PATTERNS_PROG_TYPE_LOCAL
      PATTERNS_PROG_TYPE_GLOBAL =        ME->M_SETUP-PATTERNS_PROG_TYPE_GLOBAL
      PATTERNS_PROG_DEFINE =             ME->M_SETUP-PATTERNS_PROG_DEFINE
      PATTERNS_FORM_NAME =               ME->M_SETUP-PATTERNS_FORM_NAME
      PATTERNS_FORM_USING =              ME->M_SETUP-PATTERNS_FORM_USING
      PATTERNS_FORM_CHANGING =           ME->M_SETUP-PATTERNS_FORM_CHANGING
      PATTERNS_FORM_TABLE =              ME->M_SETUP-PATTERNS_FORM_TABLE
      OPTION_SCAN_WEB_DYNPRO =           M_OPTION-SCAN_WEB_DYNPRO
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


  method GET_SUPER_CLASS.
    data:
      L_COUNT       type I,
      L_SUPER_CLASS type STRING.

    while P_TYPE_SYMBOL_CLASS->SUPER_CLASS            is bound        and
          P_TYPE_SYMBOL_CLASS->SUPER_CLASS->FULL_NAME <> '\PT:OBJECT' and
          L_COUNT                                     < 64.
      P_TYPE_SYMBOL_CLASS = P_TYPE_SYMBOL_CLASS->SUPER_CLASS.
      add 1 to L_COUNT.
    endwhile.

    P_SUPER_CLASS = P_TYPE_SYMBOL_CLASS->FULL_NAME.

    check P_SUPER_CLASS(4) = '\TY:'.

    L_SUPER_CLASS = P_SUPER_CLASS+4.

    do.
      select single REFCLSNAME from VSEOPARENT into L_SUPER_CLASS
           where CLSNAME = L_SUPER_CLASS
             and VERSION    = '1'
             and STATE      = '1'.           "#EC CI_NOORDER
      if SY-SUBRC <> 0.
        exit.
      endif.
      add 1 to L_COUNT.
      if L_COUNT > 64.
        exit.
      endif.
    enddo.

    concatenate '\TY:'  L_SUPER_CLASS into P_SUPER_CLASS.

  endmethod.


  method IF_CI_TEST~QUERY_ATTRIBUTES.

    call function 'SCI_TEST_ABAP_NAMING_NEW'
      exporting
        IE_READ_ONLY   = P_DISPLAY
      changing
        CS_SETUP_DATA  = ME->M_SETUP
        CS_OPTION_DATA = M_OPTION.

  endmethod.


  method INIT_PREFIXES.

    if ME->M_SETUP-PREFIX_ELEMENTARY is initial.
      ME->M_SETUP-PREFIX_ELEMENTARY =     'V'.   "(E)lementary!
    endif.

    if ME->M_SETUP-PREFIX_GENERIC is initial.
      " backward compatible - else it would be (G)eneric
      ME->M_SETUP-PREFIX_GENERIC =        ME->M_SETUP-PREFIX_ELEMENTARY.
    endif.
    if ME->M_SETUP-PREFIX_STRUCTURE is initial.
      ME->M_SETUP-PREFIX_STRUCTURE =      'S'.    " (S)tructure
    endif.

    if ME->M_SETUP-PREFIX_TABLE_ANY is initial.
      ME->M_SETUP-PREFIX_TABLE_ANY =      'T'.    " (T)able
    endif.

    if ME->M_SETUP-PREFIX_TABLE_INDEX is initial.
      ME->M_SETUP-PREFIX_TABLE_INDEX =     ME->M_SETUP-PREFIX_TABLE_ANY.
    endif.

    if ME->M_SETUP-PREFIX_TABLE_STANDARD is initial.
      ME->M_SETUP-PREFIX_TABLE_STANDARD =  ME->M_SETUP-PREFIX_TABLE_ANY.
    endif.

    if ME->M_SETUP-PREFIX_TABLE_HASHED is initial.
      concatenate
        ME->M_SETUP-PREFIX_TABLE_ANY 'H?'         " (T)able (H)ashed
        into ME->M_SETUP-PREFIX_TABLE_HASHED.
    endif.

    if ME->M_SETUP-PREFIX_TABLE_SORTED is initial.
      concatenate
        ME->M_SETUP-PREFIX_TABLE_ANY 'S?'         " (T)able (H)ashed
        into ME->M_SETUP-PREFIX_TABLE_SORTED.
    endif.

    if ME->M_SETUP-PREFIX_REF_DATA is initial.
      ME->M_SETUP-PREFIX_REF_DATA = 'R'.          " data (R)eference
    endif.

    if ME->M_SETUP-PREFIX_REF_CLASS is initial.
      ME->M_SETUP-PREFIX_REF_CLASS = 'O'.         " (O)bject Reference
    endif.

    if ME->M_SETUP-PREFIX_REF_CLASS_BADI is initial.
      ME->M_SETUP-PREFIX_REF_CLASS_BADI = ME->M_SETUP-PREFIX_REF_CLASS.
    endif.

    if ME->M_SETUP-PREFIX_REF_CLASS_EXCEPTION is initial.
      ME->M_SETUP-PREFIX_REF_CLASS_EXCEPTION = ME->M_SETUP-PREFIX_REF_CLASS.
    endif.

    if ME->M_SETUP-PREFIX_REF_INTERFACE is initial.
      ME->M_SETUP-PREFIX_REF_INTERFACE = ME->M_SETUP-PREFIX_REF_CLASS.
    endif.

  endmethod.


  method PUT_ATTRIBUTES.
    clear ME->M_SETUP.
    clear M_OPTION.
    import
      PREFIX_ELEMENTARY  =               ME->M_SETUP-PREFIX_ELEMENTARY
      PREFIX_GENERIC =                   ME->M_SETUP-PREFIX_GENERIC
      PREFIX_STRUCTURE =                 ME->M_SETUP-PREFIX_STRUCTURE
      PREFIX_TABLE_ANY =                 ME->M_SETUP-PREFIX_TABLE_ANY
      PREFIX_TABLE_HASHED =              ME->M_SETUP-PREFIX_TABLE_HASHED
      PREFIX_TABLE_INDEX =               ME->M_SETUP-PREFIX_TABLE_INDEX
      PREFIX_TABLE_STANDARD =            ME->M_SETUP-PREFIX_TABLE_STANDARD
      PREFIX_TABLE_SORTED =              ME->M_SETUP-PREFIX_TABLE_SORTED
      PREFIX_REF_DATA =                  ME->M_SETUP-PREFIX_REF_DATA
      PREFIX_REF_CLASS =                 ME->M_SETUP-PREFIX_REF_CLASS
      PREFIX_REF_CLASS_BADI =            ME->M_SETUP-PREFIX_REF_CLASS_BADI
      PREFIX_REF_CLASS_EXCEPTION =       ME->M_SETUP-PREFIX_REF_CLASS_EXCEPTION
      PREFIX_REF_INTERFACE =             ME->M_SETUP-PREFIX_REF_INTERFACE
      PATTERNS_CLAS_NAME_GLOBAL =        ME->M_SETUP-PATTERNS_CLAS_NAME_GLOBAL
      PATTERNS_CLAS_NAME_LOCAL =         ME->M_SETUP-PATTERNS_CLAS_NAME_LOCAL
      PATTERNS_CLAS_MEMBER =             ME->M_SETUP-PATTERNS_CLAS_MEMBER
      PATTERNS_CLAS_MEMBER_CLASS =       ME->M_SETUP-PATTERNS_CLAS_MEMBER_CLASS
      PATTERNS_CLAS_CONSTANT =           ME->M_SETUP-PATTERNS_CLAS_CONSTANT
      PATTERNS_CLAS_TYPE =               ME->M_SETUP-PATTERNS_CLAS_TYPE
      PATTERNS_INTF_NAME_GLOBAL =        ME->M_SETUP-PATTERNS_INTF_NAME_GLOBAL
      PATTERNS_INTF_NAME_LOCAL =         ME->M_SETUP-PATTERNS_INTF_NAME_LOCAL
      PATTERNS_INTF_MEMBER =             ME->M_SETUP-PATTERNS_INTF_MEMBER
      PATTERNS_INTF_MEMBER_CLASS =       ME->M_SETUP-PATTERNS_INTF_MEMBER_CLASS
      PATTERNS_INTF_CONSTANT =           ME->M_SETUP-PATTERNS_INTF_CONSTANT
      PATTERNS_INTF_TYPE =               ME->M_SETUP-PATTERNS_INTF_TYPE
      PATTERNS_METD_NAME =               ME->M_SETUP-PATTERNS_METD_NAME
      PATTERNS_METD_IMPORTING =          ME->M_SETUP-PATTERNS_METD_IMPORTING
      PATTERNS_METD_EXPORTING =          ME->M_SETUP-PATTERNS_METD_EXPORTING
      PATTERNS_METD_CHANGING =           ME->M_SETUP-PATTERNS_METD_CHANGING
      PATTERNS_METD_RETURNING =          ME->M_SETUP-PATTERNS_METD_RETURNING
      PATTERNS_EVNT_NAME =               ME->M_SETUP-PATTERNS_EVNT_NAME
      PATTERNS_EVNT_EXPORTING =          ME->M_SETUP-PATTERNS_EVNT_EXPORTING
      PATTERNS_FUGR_NAME =               ME->M_SETUP-PATTERNS_FUGR_NAME
      PATTERNS_FUNC_NAME =               ME->M_SETUP-PATTERNS_FUNC_NAME
      PATTERNS_FUNC_IMPORTING =          ME->M_SETUP-PATTERNS_FUNC_IMPORTING
      PATTERNS_FUNC_EXPORTING =          ME->M_SETUP-PATTERNS_FUNC_EXPORTING
      PATTERNS_FUNC_CHANGING =           ME->M_SETUP-PATTERNS_FUNC_CHANGING
      PATTERNS_FUNC_TABLES =             ME->M_SETUP-PATTERNS_FUNC_TABLES
      PATTERNS_REPT_NAME =               ME->M_SETUP-PATTERNS_REPT_NAME
      PATTERNS_PROG_DATA_LOCAL =         ME->M_SETUP-PATTERNS_PROG_DATA_LOCAL
      PATTERNS_PROG_DATA_GLOBAL =        ME->M_SETUP-PATTERNS_PROG_DATA_GLOBAL
      PATTERNS_PROG_DATA_STATIC =        ME->M_SETUP-PATTERNS_PROG_DATA_STATIC
      PATTERNS_PROG_FIELD_SYMBOL_LOC =   ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_LOC
      PATTERNS_PROG_FIELD_SYMBOL_GLB =   ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_GLB
      PATTERNS_PROG_CONSTANT_LOCAL =     ME->M_SETUP-PATTERNS_PROG_CONSTANT_LOCAL
      PATTERNS_PROG_CONSTANT_GLOBAL =    ME->M_SETUP-PATTERNS_PROG_CONSTANT_GLOBAL
      PATTERNS_PROG_FIELD_GROUP =        ME->M_SETUP-PATTERNS_PROG_FIELD_GROUP
      PATTERNS_PROG_SELSCR_OPTION =      ME->M_SETUP-PATTERNS_PROG_SELSCR_OPTION
      PATTERNS_PROG_SELSCR_PARAMETER =   ME->M_SETUP-PATTERNS_PROG_SELSCR_PARAMETER
      PATTERNS_PROG_TYPE_LOCAL =         ME->M_SETUP-PATTERNS_PROG_TYPE_LOCAL
      PATTERNS_PROG_TYPE_GLOBAL =        ME->M_SETUP-PATTERNS_PROG_TYPE_GLOBAL
      PATTERNS_PROG_DEFINE =             ME->M_SETUP-PATTERNS_PROG_DEFINE
      PATTERNS_FORM_NAME =               ME->M_SETUP-PATTERNS_FORM_NAME
      PATTERNS_FORM_USING =              ME->M_SETUP-PATTERNS_FORM_USING
      PATTERNS_FORM_CHANGING =           ME->M_SETUP-PATTERNS_FORM_CHANGING
      PATTERNS_FORM_TABLE =              ME->M_SETUP-PATTERNS_FORM_TABLE
      OPTION_SCAN_WEB_DYNPRO =           M_OPTION-SCAN_WEB_DYNPRO
    from data buffer P_ATTRIBUTES.

    INIT_PREFIXES( ). " validate / correct input

  endmethod.


  method QUALIFY_STATEMENT.

    clear QUALIFIED_TOKENS.

    insert lines of P_REF_SCAN->TOKENS
           from STATEMENT_WA-FROM
           to   STATEMENT_WA-TO into table QUALIFIED_TOKENS.

    STATEMENT_FROM = 1.
    STATEMENT_TO   = LINES( QUALIFIED_TOKENS ).


    call function 'RS_QUALIFY_ABAP_TOKENS_STR'
      exporting
        STATEMENT_TYPE        = STATEMENT_WA-TYPE
        INDEX_FROM            = STATEMENT_FROM
        INDEX_TO              = STATEMENT_TO
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


  method RAISE_MESSAGE.
    data:
      L_CHECKSUM type SCI_CRC64.

    CL_CI_PROVIDE_CHECKSUM=>GEN_CHKSUM_FROM_STRING( exporting  P_PARAM         = P_PREFIX
                                                    changing   P_CRC_VALUE     = L_CHECKSUM
                                                    exceptions PARAMETER_ERROR = 1 ).

    CL_CI_PROVIDE_CHECKSUM=>GEN_CHKSUM_FROM_CHARS(  exporting  P_PARAM         = P_CODE
                                                    changing   P_CRC_VALUE     = L_CHECKSUM
                                                    exceptions PARAMETER_ERROR = 1 ).

    if P_PARAM_1 is not initial.
      CL_CI_PROVIDE_CHECKSUM=>GEN_CHKSUM_FROM_STRING( exporting  P_PARAM         = P_PARAM_1
                                                      changing   P_CRC_VALUE     = L_CHECKSUM
                                                      exceptions PARAMETER_ERROR = 1 ).
    endif.



    INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
            P_SUB_OBJ_NAME = P_INCLUDE
            P_LINE         = P_LINE
            P_COLUMN       = P_COLUMN
            P_TEST         = MYNAME
            P_CODE         = P_CODE
            P_PARAM_1      = P_PARAM_1
            P_CHECKSUM_1   = L_CHECKSUM-I1 ).
  endmethod.


  method RUN.
    data:
      L_EXCP    type ref to LCX_EXCEPTION,
      L_PREFIX  type STRING,
      L_SUBRC   like SY-SUBRC,
      L_INCLUDE type STRING.

    if OBJECT_TYPE = 'WDYN' and M_OPTION-SCAN_WEB_DYNPRO = ABAP_FALSE.
      return.
    endif.


    IGNORE = ABAP_FALSE.


    try.

        if REF_SCAN is initial.
          check GET( ) = 'X'.
        endif.

        check REF_SCAN->SUBRC = 0.

        concatenate '\PR:' PROGRAM_NAME into L_PREFIX.

        COMP = CL_ABAP_COMPILER=>CREATE( P_NAME = PROGRAM_NAME ).
        COMP->GET_CHECK_INFOS( importing P_SUBRC = L_SUBRC ).

        if L_SUBRC <> 0.
          raise exception type LCX_EXCEPTION
            exporting
              P_CODE    = C_CODE-SYNTAX
              P_INCLUDE = PROGRAM_NAME
              P_LINE    = 0
              P_COLUMN  = 0.
        endif.

        STATEMENT_INDEX = 1.



        do.
          read table REF_SCAN->STATEMENTS index STATEMENT_INDEX into STATEMENT_WA.
          if SY-SUBRC <> 0. exit. endif.
          add 1 to STATEMENT_INDEX.
          check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
          L_INCLUDE  = GET_INCLUDE( ).
          check L_INCLUDE(1) <> '<' and L_INCLUDE(1) <> '>'.
          "  -- avoid native SQL
          check STATEMENT_WA-TYPE na 'EMPSDRG'.

          case KEYWORD( ).
            when 'PROGRAM' or 'REPORT'.
              CHECK_PROGRAM( L_PREFIX ).

            when 'FUNCTION-POOL'.
              CHECK_FUNCTION_POOL( L_PREFIX ).

            when 'TYPES'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'TY'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_TYPE_GLOBAL
                  P_CODE   = C_CODE-GLOB_TYPE ).

            when 'DATA' or 'RANGES'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'DA'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_DATA_GLOBAL
                  P_CODE   = C_CODE-GLOB_DATA ).

            when 'CONSTANTS'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'DA'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_CONSTANT_GLOBAL
                  P_CODE   = C_CODE-GLOB_CONSTANT ).

            when 'FIELD-SYMBOLS'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'DA'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_GLB
                  P_CODE   = C_CODE-GLOB_FS ).

            when 'SELECT-OPTIONS'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'DA'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_SELSCR_OPTION
                  P_CODE   = C_CODE-SELOPT ).

            when 'PARAMETERS' or 'PARAMETER'.
              CHECK_DATA_TYPE(
                  P_PREFIX = L_PREFIX
                  P_TAG    = 'DA'
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_SELSCR_PARAMETER
                  P_CODE   = C_CODE-PARAM ).

            when 'FIELD-GROUPS'.
              CHECK_GENERAL(
                  P_PREFIX = L_PREFIX
                  P_IDS    = ME->M_SETUP-PATTERNS_PROG_FIELD_GROUP
                  P_CODE   = C_CODE-FLDGRP ).

            when 'CLASS'.
              CHECK_CLASS( P_PREFIX = L_PREFIX ).

            when 'INTERFACE'.
              CHECK_INTERFACE( P_PREFIX = L_PREFIX ).

            when 'FORM'.
              CHECK_FORM( P_PREFIX = L_PREFIX ).

            when 'FUNCTION'.
              CHECK_FUNCTION( P_PREFIX = L_PREFIX ).

            when 'TYPE-POOL'.
              CHECK_TYPEPOOL( ).

            when 'DEFINE'.
              CHECK_DEFINE(  L_PREFIX  ).

            when 'AT'.
              case GET_TOKEN_REL( 2 ).
                when 'SELECTION-SCREEN'.
                  CHECK_AT_SELECTION_SCREEN( P_PREFIX = L_PREFIX ).
              endcase.

            when 'SET'.
              CHECK_SET( ).

            when others.
              CHECK_ANY( P_PREFIX    = L_PREFIX
                         P_DATA_IDS  = ME->M_SETUP-PATTERNS_PROG_DATA_GLOBAL
                         P_DATA_CODE = C_CODE-GLOB_DATA
                         P_FS_IDS    = ME->M_SETUP-PATTERNS_PROG_FIELD_SYMBOL_GLB
                         P_FS_CODE   = C_CODE-GLOB_FS ).
          endcase.
        enddo.
      catch LCX_EXCEPTION into L_EXCP.

        RAISE_MESSAGE( P_PREFIX  = L_PREFIX
                       P_INCLUDE = L_EXCP->INCLUDE
                       P_LINE    = L_EXCP->LINE
                       P_COLUMN  = L_EXCP->COLUMN
                       P_CODE    = L_EXCP->CODE
                       P_PARAM_1 = L_EXCP->PARAM ).
    endtry.
  endmethod.
ENDCLASS.
