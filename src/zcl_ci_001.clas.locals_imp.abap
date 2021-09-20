type-pools
  SANA.

class LCX_EXCEPTION definition final inheriting from CX_NO_CHECK.
  public section.
    data:
      CODE    type SCI_ERRC,
      INCLUDE type SOBJ_NAME,
      LINE    type TOKEN_ROW,
      COLUMN  type TOKEN_COL,
      PARAM   type STRING.
    methods CONSTRUCTOR importing
                          P_CODE    type SCI_ERRC
                          P_INCLUDE type SOBJ_NAME
                          P_LINE    type TOKEN_ROW
                          P_COLUMN  type TOKEN_COL
                          P_PARAM   type STRING optional.
endclass.

class LCX_EXCEPTION implementation.
  method CONSTRUCTOR.
    SUPER->CONSTRUCTOR( ).
    CODE      = P_CODE.
    INCLUDE   = P_INCLUDE.
    LINE      = P_LINE.
    COLUMN    = P_COLUMN.
    PARAM     = P_PARAM.
  endmethod.
endclass.
