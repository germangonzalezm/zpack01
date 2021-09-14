class ZCL_CI_008_AUX definition
  public
  inheriting from CL_CI_TEST_ABAP_COMPILER
  final
  create public .

*"* public components of class ZCL_CI_008_AUX
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
  methods RUN_BEGIN
    redefinition .
  protected section.
*"* protected components of class ZCL_CI_008_AUX
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_CI_008_AUX
*"* do not include other source files here!!!
  data PA_STANDARD type SYCHAR01 .
  data PA_SORTED type SYCHAR01 .
  data PA_HASHED type SYCHAR01 .
  data PA_GENERIC type SYCHAR01 .
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_CI_008_AUX' ##NO_TEXT.
  constants MESSAGEGM0 type SCI_ERRC value 'MESSAGEGM0' ##NO_TEXT.
  constants MESSAGEGM1 type SCI_ERRC value 'MESSAGEGM1' ##NO_TEXT.
  constants MESSAGEGM2 type SCI_ERRC value 'MESSAGEGM2' ##NO_TEXT.
  constants MESSAGEGM3 type SCI_ERRC value 'MESSAGEGM3' ##NO_TEXT.
  constants MESSAGEGJG type SCI_ERRC value 'MESSAGEGJG' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CI_008_AUX IMPLEMENTATION.


  method CONSTRUCTOR .
    SUPER->CONSTRUCTOR( ).
    DESCRIPTION = '8.- WHERE en el comando LOOP'(005).
    CATEGORY    = 'ZCL_CI_CATEGORY_CYT'.
    POSITION    = '999'.
    VERSION     = '000'.

    HAS_ATTRIBUTES = 'X'.
    HAS_DOCUMENTATION = 'X'.
    PA_STANDARD =
    PA_SORTED   =
    PA_HASHED   =
    PA_GENERIC  = 'X'.
    ATTRIBUTES_OK = 'X'.

    define FILL_MESSAGE.
      clear SMSG.
      SMSG-TEST = C_MY_NAME.
      SMSG-CODE = &1.  "message code
      SMSG-KIND = &2.  "message priority
      SMSG-TEXT = &3.  "message text
      SMSG-PCOM = &4.  "pseudocomment
      insert SMSG into table SCIMESSAGES.
    end-of-definition.

*-------Sequential Read Access to Standard Table
    FILL_MESSAGE MESSAGEGM0 C_NOTE    text-100 '"#EC CI_STDSEQ'.
*-------Possible Sequential Read Access to Sorted Table
    FILL_MESSAGE MESSAGEGM1 C_WARNING text-101 '"#EC CI_SORTSEQ'.
*-------Possible Sequential Read Access to Hashed Table
    FILL_MESSAGE MESSAGEGM2 C_WARNING text-102 '"#EC CI_HASHSEQ'.
*-------Possible Sequential Read Access to Generic Table
    FILL_MESSAGE MESSAGEGM3 C_WARNING text-103 '"#EC CI_ANYSEQ'.
*-------Possible Sequential Access when Deleting from Standard Table
    FILL_MESSAGE MESSAGEGJG C_WARNING text-104 '"#EC CI_STDSEQ'.

  endmethod.


  method GET_ATTRIBUTES .
    export PA_STANDARD = PA_STANDARD
           PA_SORTED   = PA_SORTED
           PA_HASHED   = PA_HASHED
           PA_GENERIC  = PA_GENERIC to data buffer P_ATTRIBUTES.
  endmethod.


  method IF_CI_TEST~QUERY_ATTRIBUTES .
    class:
      CL_CI_QUERY_ATTRIBUTES definition load.
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
    FILL_ATT PA_STANDARD             'STANDARD-Tabellen'(001)             'C'.
    FILL_ATT PA_SORTED               'SORTED-Tabellen'(002)               'C'.
    FILL_ATT PA_HASHED               'HASHED-Tabellen'(003)               'C'.
    FILL_ATT PA_GENERIC              'Generische-Tabellen'(004)           'C'.

    if CL_CI_QUERY_ATTRIBUTES=>GENERIC(
                       P_NAME       = C_MY_NAME
                       P_TITLE      = 'Angabe der zu prüfenden Tabellenarten'(006)
                       P_ATTRIBUTES = L_ATTRIBUTES
                       P_MESSAGE    = L_MESSAGE
                       P_DISPLAY    = P_DISPLAY ) = 'X'.
*-- = 'X' --> 'Exit' Button pressed on PopUp
      return.
    endif.

    if not ( PA_STANDARD is initial and
             PA_SORTED   is initial and
             PA_HASHED   is initial and
             PA_GENERIC  is initial ).
      ATTRIBUTES_OK = 'X'.
    else.
      clear ATTRIBUTES_OK.
    endif.

  endmethod.


  method PUT_ATTRIBUTES .
    import PA_STANDARD   = PA_STANDARD
             PA_SORTED   = PA_SORTED
             PA_HASHED   = PA_HASHED
             PA_GENERIC  = PA_GENERIC from data buffer P_ATTRIBUTES.
  endmethod.


  method RUN.
    data:
      L_INFOS       type SYNT_ERRORS,
      L_MESSAGE     like line of L_INFOS,
      L_OBJECT_NAME type SOBJ_NAME,
      L_COL         type TOKEN_COL,
      L_CODE        type SCI_ERRC,
      L_SUPPRESS    type SYCHAR20,
      L_LONGTEXTS   type RSLINLTAB,
      L_LONG_TEXT   type STRING,
      L_INDEX       type I,
      L_KIND        type SYCHAR01.

    check GET( ) = 'X'.

    GET_INFO_LONGTEXT( importing P_INFOS    = L_INFOS
                                 P_LONGTEXT = L_LONGTEXTS ).

    loop at L_INFOS into L_MESSAGE where KIND = 'I'.
      L_INDEX = SY-TABIX.

      check PA_STANDARD = 'X' and L_MESSAGE-MSGNUMBER(3) = 'GM0' or
            PA_STANDARD = 'X' and L_MESSAGE-MSGNUMBER(3) = 'GJG' or
            PA_SORTED   = 'X' and L_MESSAGE-MSGNUMBER(3) = 'GM1' or
            PA_HASHED   = 'X' and L_MESSAGE-MSGNUMBER(3) = 'GM2' or
            PA_GENERIC  = 'X' and L_MESSAGE-MSGNUMBER(3) = 'GM3'.

      L_OBJECT_NAME = L_MESSAGE-INCNAME.
      L_COL         = L_MESSAGE-COL.
      L_CODE        = GET_CODE( P_KEYWORD   = L_MESSAGE-KEYWORD
                                P_MSGNUMBER = L_MESSAGE-MSGNUMBER(3) ).

      read table SCIMESSAGES into SMSG
                 with table key TEST = C_MY_NAME
                                CODE = L_CODE.
      if SY-SUBRC = 0.
        L_SUPPRESS = SMSG-PCOM.
        shift L_SUPPRESS left by 5 places.
        L_KIND     = SMSG-KIND.
      else.
        clear: L_SUPPRESS.
        L_KIND = C_NOTE.
      endif.

      L_LONG_TEXT   = GET_LONG_TEXT( P_LONG_TEXTS = L_LONGTEXTS
                                     P_KIND       = 'I'
                                     P_NUMBER     = L_INDEX ).

      INFORM(
        P_SUB_OBJ_TYPE = C_TYPE_INCLUDE
        P_SUB_OBJ_NAME = L_OBJECT_NAME
        P_POSITION     = L_MESSAGE-STMT_CNT
        P_SUPPRESS     = L_SUPPRESS
        P_LINE         = L_MESSAGE-LINE
        P_COLUMN       = L_COL
        P_KIND         = L_KIND
        P_TEST         = C_MY_NAME
        P_CODE         = L_CODE
        P_PARAM_2      = L_MESSAGE-MESSAGE
        P_PARAM_1      = L_LONG_TEXT ).
    endloop.
  endmethod.


  method RUN_BEGIN .
    NEED_INFOS    = 'X'.
    NEED_COMMENTS = 'X'.
    SUPER->RUN_BEGIN( ).
  endmethod.
ENDCLASS.
