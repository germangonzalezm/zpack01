class ZCL_CI_004 definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public .

*"* public components of class ZCL_CI_004
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .
  methods AUTHORITY_CHECK
    exporting
      !P_RESULT type N .

  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.
*"* protected components of class ZCL_CI_004
*"* do not include other source files here!!!

  types T_SEARCH_STRING type SYCHAR72 .

  data COMMENT_MODE type FLAG .
  data LITERAL_MODE type FLAG .
  data MSGKIND      type SYCHAR01.
  data SEARCH_STRINGS type SCI_SRCHSTR .
private section.

*"* private components of class ZCL_CI_004
*"* do not include other source files here!!!
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_CI_004' ##NO_TEXT.
  constants MCODE_0001 type SCI_ERRC value '0001' ##NO_TEXT ##NEEDED.
ENDCLASS.



CLASS ZCL_CI_004 IMPLEMENTATION.


  method authority_check.

    data: wa_tokens like line of ref_scan->tokens.
    data: lv_index type sy-tabix.
    data: lv_next type c length 2.
    data: lt_tokens like ref_scan->tokens.


    append lines of ref_scan->tokens from statement_wa-from to statement_wa-to to lt_tokens.

    loop at lt_tokens into wa_tokens.
      lv_index = sy-tabix.

*'00' AUTHORITY-CHECK
*'01' OBJECT
*'02' 'S_CARRID'
*'03' FOR
*'04' USER
*'05' SY-UNAME
*
*'03' ID
*'07' 'CARRID'
*'08' FIELD
*'09' PA_CARR

*'03 ID
*'07 'ACTVT'
*'08 DUMMY

      if lv_index = '1'.
        lv_next = '01'.
      else.
        case lv_next.
          when '01'.
            lv_next = '02'.
          when '02'.

            data w_i type i.
            w_i = strlen( wa_tokens-str ).

            subtract 1 from w_i.

            wa_tokens-str = wa_tokens-str+0(w_i).
            shift wa_tokens-str by 1 places.

            if wa_tokens-str eq 'S_CARRID'.
              p_result = 1.
            else.
              p_result = 0.
            endif.
            lv_next = '03'.
        endcase.
      endif.
    endloop.

  endmethod.


method CONSTRUCTOR .
  SUPER->CONSTRUCTOR( ).
  DESCRIPTION        = '4.- Uso de objeto especifico en Authority-check'(000).
  VERSION            = '001'.
  CATEGORY           = 'ZCL_CI_CATEGORY_CYT'.
  POSITION           = '001'.
  MSGKIND            = 'N'.
*  HAS_ATTRIBUTES     = C_TRUE.
*  ATTRIBUTES_OK      = C_FALSE.
*  REMOTE_ENABLED     = ABAP_TRUE.
*  REMOTE_RFC_ENABLED = ABAP_TRUE.
  USES_CHECKSUM      = ABAP_TRUE.
endmethod.                    "CONSTRUCTOR


method GET_ATTRIBUTES.
  export LITERAL_MODE   = LITERAL_MODE
         COMMENT_MODE   = COMMENT_MODE
         SEARCH_STRINGS = SEARCH_STRINGS
         MSGKIND        = MSGKIND
         to data buffer P_ATTRIBUTES.
endmethod.                    "GET_ATTRIBUTES


method GET_MESSAGE_TEXT .
  data:
    L_CODE type SCI_ERRC.

  if P_TEST <> MYNAME or P_CODE = C_CODE_NOT_REMOTE_ENABLED.
    SUPER->GET_MESSAGE_TEXT(
                exporting P_TEST = P_TEST P_CODE = P_CODE
                importing P_TEXT = P_TEXT ).
    return.
  endif.
  L_CODE = P_CODE.
  shift L_CODE left deleting leading SPACE.
  P_TEXT = 'Suchstring Nummer &N gefunden in: &1'(101).
  replace first occurrence of '&N' in P_TEXT with L_CODE.

endmethod.                    "GET_MESSAGE_TEXT


method IF_CI_TEST~QUERY_ATTRIBUTES .
  data:
    L_ATTRIBUTES     type SCI_ATTTAB,
    L_ATTRIBUTE      like line of L_ATTRIBUTES,
    L_COMMENT_MODE   like COMMENT_MODE,
    L_LITERAL_MODE   like LITERAL_MODE,
    L_MSGKIND        like MSGKIND,
    L_SEARCH_STRINGS like SEARCH_STRINGS,
    L_SEARCH_STRING  like line of SEARCH_STRINGS,
    L_STRLEN         type I,
    L_MESSAGE(72)    type C.

  define FILL_ATT.
    get reference of &1 into L_ATTRIBUTE-REF.
    L_ATTRIBUTE-TEXT = &2.
    L_ATTRIBUTE-KIND = &3.
    append L_ATTRIBUTE to L_ATTRIBUTES.
  end-of-definition.

  L_SEARCH_STRINGS = SEARCH_STRINGS.
  L_MSGKIND        = MSGKIND.
  L_COMMENT_MODE   = COMMENT_MODE.
  L_LITERAL_MODE   = LITERAL_MODE.

  L_STRLEN = 0.

*-- fill attribute table
*FILL_ATT L_CHAR    'Char'(211)       ' '.
*FILL_ATT L_NUM     'Numerisch'(212)  ' '.
*FILL_ATT L_STRUCT  'Struktur'(213)   ' '.

  FILL_ATT L_COMMENT_MODE   'Kommentare'(202)       'C'.
  FILL_ATT L_LITERAL_MODE   'Literale'(203)         'C'.
  FILL_ATT L_MSGKIND        'Art der Meldung'(205)  ' '.
  FILL_ATT L_SEARCH_STRINGS 'Suchstring(s)'(204)    ' '.

*-- only search with 2 letters minimum
  do.
    L_STRLEN = 999999.
    if CL_CI_QUERY_ATTRIBUTES=>GENERIC(
                          P_NAME       = 'ZCL_CI_004'
                          P_TITLE      = 'Meine Selektionen'(005)
                          P_ATTRIBUTES = L_ATTRIBUTES
                          P_MESSAGE    = L_MESSAGE
                          P_DISPLAY    = P_DISPLAY ) = 'X'.
*-- = 'X' --> 'Exit' Button pressed on PopUp
      return.
    endif.
    if L_SEARCH_STRINGS is initial.
      L_MESSAGE = 'Bitte mindestens einen Suchstring angeben'(902).
      L_STRLEN = 0.
    else.
      loop at L_SEARCH_STRINGS into L_SEARCH_STRING.
        translate L_SEARCH_STRING using '* + '.
        condense L_SEARCH_STRING no-gaps.
        if STRLEN( L_SEARCH_STRING ) < L_STRLEN.
          L_STRLEN = STRLEN( L_SEARCH_STRING ).
        endif.
      endloop.

      if L_STRLEN < 2.
        L_MESSAGE = 'Bitte mindestens 2 Buchstaben eingeben'(901).
      else.
        case L_MSGKIND.
          when 'E' or 'W' or 'N'.
            exit.
          when others.
            L_MESSAGE = 'Meldungsart nur E, W oder N'(903).
        endcase.
      endif.
    endif.
  enddo.

  SEARCH_STRINGS = L_SEARCH_STRINGS.
  MSGKIND        = L_MSGKIND.
  LITERAL_MODE   = L_LITERAL_MODE.
  COMMENT_MODE   = L_COMMENT_MODE.
  ATTRIBUTES_OK  = C_TRUE.

endmethod.                    "IF_CI_TEST~QUERY_ATTRIBUTES


method PUT_ATTRIBUTES .

  import LITERAL_MODE   = LITERAL_MODE
         COMMENT_MODE   = COMMENT_MODE
         SEARCH_STRINGS = SEARCH_STRINGS
         MSGKIND        = MSGKIND
         from data buffer P_ATTRIBUTES.

endmethod.                    "PUT_ATTRIBUTES


method RUN .

  data:
    L_INCLUDE       type SOBJ_NAME,
    L_ROW           type TOKEN_ROW,
    L_COLUMN        type TOKEN_COL,
    L_TOKENNR       like STATEMENT_WA-FROM,
    L_CODE          type SCI_ERRC,
    L_SEARCH_STRING like line of SEARCH_STRINGS,
    L_POSITION      type I,
    L_CHECKSUM      type SCI_CRC64.

  append initial line to search_strings.
  search_strings[ 1 ] = 'AUTHORITY-CHECK'.

  if SEARCH_STRINGS is initial.
    return.
  endif.

  if REF_SCAN is initial.
    check GET( ) = 'X'.
  endif.

  check REF_SCAN->SUBRC = 0.

*-- loop at all tokens
  loop at REF_SCAN->STATEMENTS into STATEMENT_WA.
    check STATEMENT_WA-FROM <= STATEMENT_WA-TO.
    L_POSITION = SY-TABIX.
    if STATEMENT_WA-TYPE = 'S' or
       STATEMENT_WA-TYPE = 'P'.
      check COMMENT_MODE = 'X'.
    endif.

    loop at REF_SCAN->TOKENS into TOKEN_WA
           from STATEMENT_WA-FROM to STATEMENT_WA-TO.
      L_TOKENNR = SY-TABIX.
      if TOKEN_WA-TYPE = 'S'.
        check LITERAL_MODE = 'X'.
      endif.

      loop at SEARCH_STRINGS into L_SEARCH_STRING.
*-- does ABAP-string contain search-string ?
        if TOKEN_WA-STR cp L_SEARCH_STRING.
          unpack SY-TABIX to L_CODE(4).

          data: lv_result type n.
          data: lv_tipo_mensaje type c.
          authority_check( importing p_result = lv_result ).
          if lv_result = '1'.
            lv_tipo_mensaje = 'N'.
          else.
            lv_tipo_mensaje = 'E'.
          endif.

          L_INCLUDE = GET_INCLUDE( ).
          L_ROW     = GET_LINE_ABS( L_TOKENNR ).
          L_COLUMN  = GET_COLUMN_ABS( L_TOKENNR ).
          clear L_CHECKSUM.
          CL_CI_PROVIDE_CHECKSUM=>GEN_CHKSUM_FROM_CHARS( exporting P_PARAM = L_SEARCH_STRING changing  P_CRC_VALUE = L_CHECKSUM exceptions PARAMETER_ERROR = 0 ).
          GET_STMT_CHECKSUM( exporting P_POSITION = L_POSITION changing P_CHECKSUM = L_CHECKSUM exceptions ERROR = 1 ).
          INFORM( P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
                  P_SUB_OBJ_NAME = L_INCLUDE
                  P_POSITION     = L_POSITION
                  P_LINE         = L_ROW
                  P_COLUMN       = L_COLUMN
                  P_KIND         = lv_tipo_mensaje
                  P_TEST         = C_MY_NAME
                  P_CODE         = L_CODE
                  P_SUPPRESS     = '"#EC CI_NOFIND '
                  P_PARAM_1      = TOKEN_WA-STR
                  P_CHECKSUM_1   = L_CHECKSUM-I1 ).
          exit.
        endif.     "l_strpos > l_pos
      endloop.
    endloop.
  endloop.
endmethod.                    "RUN
ENDCLASS.
