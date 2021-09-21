"! FOR ALL ENTRIES: Locate possibly flawed statements (CI)
class ZCL_CI_005 definition
  public
  inheriting from CL_CI_TEST_SCAN
  create public

  global friends CL_CM_CHK_FOR_ALL_ENTRIES .

public section.
  type-pools ABAP .
  type-pools SANA .
  type-pools SCAN .

  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
  private section.
    constants:
      begin of c_Pitfall,
        likely_No_Content_Check             type sci_Errc value 'FAllUnsafe',
        likely_No_Local_Content_Check       type sci_Errc value 'FAllRisky',
      end of c_Pitfall.

    methods:
      report_Unsafe_Query
        importing   query_Ndx               type sytabix
                    table_Var_Name          type string
                    has_External_Content    type abap_Bool.

ENDCLASS.



CLASS ZCL_CI_005 IMPLEMENTATION.


  method CONSTRUCTOR.
    super->constructor( ).
    me->description =        '5.- Uso inseguro de FOR ALL ENTRIES'(000).
    me->category =           'ZCL_CI_CATEGORY_CYT'.
    me->position =           '999'.
    me->version =            '002'.
*    me->has_Attributes =     abap_False.
*    me->attributes_Ok  =     abap_True.
*    me->remote_Enabled =     abap_True.
*    me->remote_rfc_Enabled = abap_True.
    me->uses_Checksum  =     abap_True.

    data:
      begin of pseudo_Comment,
        basic         type c length 20,
        alternative   type c length 20,
      end of pseudo_Comment.
    if ( not cl_Satc_Permission_Control=>is_Sap_System( ) ).
      pseudo_Comment-basic =        'CI_FAE_LINES_ENSURED'.
      pseudo_Comment-alternative =  'CI_FAE_NO_LINES_OK'.
    endif.

    define mac_Add_Rule.
      insert value #(
        test = me->myname
        code = &1
        kind = &2
        text = &3
        pcom =     pseudo_Comment-basic
        pcom_Alt = pseudo_Comment-alternative )
     into table me->scimessages[].
    end-of-definition.

    mac_Add_Rule:
      c_Pitfall-likely_No_Local_Content_Check
      c_Note
      'Possible use of &1 without prior content validation within procedure'(003),

      c_Pitfall-likely_No_Content_Check
      c_Warning
      'Possible use of &1 without prior content validation'(002).

  endmethod.


  method GET_MESSAGE_TEXT.
    case p_Code.
      when c_Pitfall-likely_No_Content_Check.
        p_Text =
          'Possible use of &1 without prior content validation'(002).

      when c_Pitfall-likely_No_Local_Content_Check.
        p_Text =
          'Possible use of &1 without prior content validation within procedure'(003).

      when others.
        super->get_Message_Text(
          exporting p_Test = p_Test p_Code = p_Code
          importing p_Text = p_Text ).
    endcase.
  endmethod.


  method REPORT_UNSAFE_QUERY.
    " reduced scope, favoring stability over possible clashes
    " number of similar queries shall not be too high in the wild
    " anyway do not touch, else exemptions will break
    constants:
      c_Statement_Range_6 type i value 6.
    data:
      finding_Code     type sci_Errc,
      crc_64           type sci_Crc64.

    if ( abap_True eq has_External_Content ).
      finding_Code =     c_Pitfall-likely_No_Local_Content_Check.
    else.
      finding_Code = c_Pitfall-likely_No_Content_Check.
    endif.

    get_Stmt_Checksum(
      exporting p_Position = query_Ndx
                p_Stmt_Range = c_Statement_Range_6
      changing  p_Checksum = crc_64
      exceptions others =    1 ).
    if ( 0 <> sy-subrc ).
      return.
    endif.

    try.
      data(statement) = me->ref_Scan->statements[ query_Ndx ].
      data(token) = me->ref_Scan->tokens[ statement-from ].
      data(include_Name) = get_Include( p_Ref_Scan = me->ref_Scan p_Level = statement-level ).
    catch cx_Sy_Itab_Line_Not_Found.
      clear include_Name.
    endtry.

    if ( include_Name is initial ).
      inform(
        p_Test  =      me->myname
        p_Code  =      finding_Code
        p_Position =   query_Ndx
        p_Checksum_1 = crc_64-i1
        p_Param_1 =    table_Var_Name ).
    else.
      inform(
        p_Test  =        me->myname
        p_Code  =        finding_Code
        p_Sub_Obj_Type = c_Type_Include
        p_Sub_Obj_Name = include_Name
        p_Line =         token-row
        p_Column =       token-col
        p_Position =     query_Ndx
        p_Checksum_1 =   crc_64-i1
        p_Param_1 =      table_Var_Name ).
    endif.
  endmethod.


  method RUN.
    check get( ).

    data(listener) = new listener_For_Ci( me ).
    data(analyzer) = new for_All_Entries_Analysis(  listener ).
    data(scanned_Source) =
      new scanned_Source(
        statements = me->ref_Scan->statements
        levels =     me->ref_Scan->levels
        tokens =     me->ref_Scan->tokens ).
    analyzer->examine( scanned_Source ).
   endmethod.
ENDCLASS.
