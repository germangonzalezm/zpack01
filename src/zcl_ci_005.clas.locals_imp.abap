"
" design notes on things that should have been done better
" - the analyzer should be chopped into smaller methods
" - the populated flag on block level is inferior, populate belongs to tables
"   - but this requires also to keep track of field-symbol and reference relations
"   - uh uh uh lots that can be done
"   - the statement type could have done better
"

class backtracking_With_Variable definition inheriting from cx_Static_Check.

  public section.
    data: table_Variable type string read-only.
    methods:
      constructor
        importing           table_Variable type string.

endclass.


class backtracking_With_Variable implementation.

  method constructor.
    super->constructor(  ).
    me->table_Variable = table_Variable.
  endmethod.

endclass.


class not_Found_Failure definition inheriting from cx_Dynamic_Check.
endclass.


class internal_Error definition inheriting from cx_No_Check.

  public section.
    methods
      constructor
        importing hint type string optional.

    methods if_Message~get_Text redefinition.

  private section.
    data: hint type string.

endclass.


class internal_Error implementation.

  method constructor.
    super->constructor( ).
    me->hint = hint.
  endmethod.


  method if_Message~get_Text.
    if ( hint is initial ).
      result = |INTERNAL ERROR { sy-repid }|.
    else.
      result = hint.
    endif.
  endmethod.

endclass.


class scanned_Source definition.

  public section.
    data:
       tokens     type stokesx_Tab read-only,
       statements type sstmnt_Tab  read-only,
       levels     type slevel_Tab  read-only.

    methods:
      constructor
        importing   tokens          type stokesx_Tab
                    statements      type sstmnt_Tab
                    levels          type slevel_Tab,

      get_Token_Text_By_Index
        importing   index           type numeric
        returning   value(result)   type string.

endclass.


class block definition.

  public section.
    types:
      begin of ty_Condition_State,
        any_Branch_Populates type abap_Bool,
        has_Others_Branch    type abap_Bool,
      end of ty_Condition_State.

    methods:
      get_Condition_State
        returning   value(result)       type ty_Condition_State,
      set_Condition_State
        importing   state               type ty_Condition_State
                    is_Final            type abap_Bool default abap_False,
      set_Is_Ensured
        importing   value               type abap_Bool default abap_True,
      set_Is_Ensured_By_Exit
        importing   value               type abap_Bool default abap_True,
      set_Is_Ensured_By_Exit_Final
        importing   value               type abap_Bool default abap_True,
      set_Has_External_Content
        importing   value               type abap_Bool default abap_True,
      set_Is_Final
        importing   value               type abap_Bool default abap_True,
      set_Has_Been_Checked
        importing   value               type abap_Bool default abap_True,
      set_Has_Been_Populated
        importing   value               type abap_Bool default abap_True,
      set_Type_Condition_On_Table
        importing   table_Var_Name      type string,
      set_Type_Condition_On_Variable
        importing   var_Name            type string,
      set_Type_Condition_Other,
      set_Type_Loop,
      set_Type_Other,
      set_Do_Check_Sys_Field_Next
        importing   value               type abap_Bool default abap_True,
      add_Variable_Name
        importing   variable_Name       type string,
      add_Table_Variable_Name
        importing   variable_Name       type string,
      remove_Variable_Name
        importing   variable_Name       type string,
      remove_Table_Variable_Name
        importing   variable_Name       type string,
      set_Variable_Names
        importing   variable_Names      type string_Hashed_Table,
      set_Table_Variable_Names
        importing   variable_Names      type string_Hashed_Table,
      set_Required_Table_Names
        importing   variable_Names      type string_Hashed_Table,
      clone
        returning   value(result)       type ref to block.

    constants:
      begin of c_Keyword_Type,
        condition_On_Table      type c length 2 value 'CT',
        condition_On_Variable   type c length 2 value 'CV',
        condition_Other         type c length 2 value 'CO',
        loop_Table              type c length 2 value 'LT',
        loop_Other              type c length 2 value 'LO',
        other                   type c length 2 value 'OT',
      end of c_Keyword_Type.

    data:
      is_Likely_Ensured         type abap_Bool read-only,
      is_Likely_Ensured_By_Exit type abap_Bool read-only,
      has_External_Content      type abap_Bool read-only,
      is_Final                  type abap_Bool read-only,
      do_Check_Sys_Field_Next   type abap_Bool,
      has_Been_Checked          type abap_Bool read-only,
      has_Been_Populated        type abap_Bool read-only,
      variable_Names            type string_Hashed_Table read-only,
      table_Variable_Names      type string_Hashed_Table read-only,
      non_Table_Variable_Names  type string_Hashed_Table read-only,
      required_Table_Names      type string_Hashed_Table read-only,
      condition_Var_Names       type string_Hashed_Table read-only,
      keyword_Type              type c length 2 read-only,
      keyword                   type string.

  private section.
    data:
      condition_State          type ty_Condition_State,
      condition_State_Is_Final type abap_Bool value abap_False.

endclass.


class block implementation.


  method set_Condition_State.
    if ( abap_True eq me->condition_State_Is_Final ).
      return.
    endif.
    me->condition_State = state.
    me->condition_State_Is_Final = is_Final.
  endmethod.


  method get_Condition_State.
    result = me->condition_State.
    if ( abap_False eq me->has_been_populated and abap_False eq me->condition_State_Is_Final ).
      result-any_Branch_Populates = abap_False.
    endif.
  endmethod.


  method set_Is_Final.
    me->is_Final = value.
  endmethod.


  method set_Is_Ensured.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    me->is_Likely_Ensured = value.
    me->is_Likely_Ensured_By_Exit = abap_False.
    if ( abap_True eq me->is_Likely_Ensured ).
      me->has_External_Content = abap_False.
    else.
      me->has_Been_Checked = abap_False.
      me->has_Been_Populated = abap_False.
    endif.
  endmethod.


  method set_Is_Ensured_By_Exit.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    set_Is_Ensured( value ).
    me->is_Likely_Ensured_By_Exit = value.
  endmethod.


  method set_Is_Ensured_By_Exit_Final.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    set_Is_Ensured( value ).
    me->is_Likely_Ensured_By_Exit = value.
    me->is_Final = abap_True.
  endmethod.


  method set_Has_External_Content.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    me->has_External_Content = value.
    if ( abap_True eq me->has_External_Content ).
      me->is_Likely_Ensured = abap_False.
      me->is_Likely_Ensured_By_Exit = abap_False.
      me->has_Been_Checked = abap_False.
      me->has_Been_Populated = abap_False.
    endif.
  endmethod.


  method set_Do_Check_Sys_Field_Next.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    me->do_Check_Sys_Field_Next = value.
  endmethod.


  method set_Has_Been_Checked.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    me->has_Been_Checked = value.
   if ( abap_True eq value ).
       me->is_Likely_Ensured = abap_True.
    endif.
  endmethod.


  method set_Has_Been_Populated.
    if ( abap_True eq me->is_Final ).
      return.
    endif.
    me->has_Been_Populated = value.
    if ( abap_True eq value ).
       me->is_Likely_Ensured = abap_True.
    endif.
  endmethod.


  method set_Type_Condition_On_Table.
    me->keyword_Type = c_Keyword_Type-condition_On_Table.
    read table me->table_Variable_Names transporting no fields
      with key table_Line = table_Var_Name.
    if ( 0 eq sy-subrc ).
      set_Has_Been_Checked( ).
      set_Is_Ensured( ).
    endif.
    insert table_Var_Name into table me->condition_Var_Names.
    me->condition_State-any_Branch_Populates = abap_True.
  endmethod.


  method set_Type_Condition_On_Variable.
    me->keyword_Type = c_Keyword_Type-condition_On_Variable.
    read table me->variable_Names transporting no fields with key table_Line = var_Name.
    if ( 0 eq sy-subrc ).
      set_Has_Been_Checked( ).
      set_Is_Ensured( ).
    endif.
    insert var_Name into table me->condition_Var_Names.
    clear me->condition_State.
    me->condition_State-any_Branch_Populates = abap_True.
  endmethod.


  method set_Type_Condition_Other.
    me->keyword_Type = c_Keyword_Type-condition_Other.
    clear me->condition_Var_Names.
    clear me->condition_State.
    me->condition_State-any_Branch_Populates = abap_True.
  endmethod.


  method set_Type_Loop.
    me->keyword_Type = c_Keyword_Type-loop_Other.
    clear me->condition_Var_Names.
  endmethod.


  method set_Type_Other.
    me->keyword_Type = c_Keyword_Type-other.
    clear me->condition_Var_Names.
  endmethod.


  method set_Variable_Names.
    data: variable_Name type string.
    me->non_Table_Variable_Names = me->variable_Names = variable_Names.
    loop at me->table_Variable_Names into variable_Name.
      insert variable_Name into table me->variable_Names.
      delete me->non_Table_Variable_Names where table_Line = variable_Name.
    endloop.
  endmethod.


  method add_Variable_Name.
    insert variable_Name into table me->variable_Names.
    insert variable_Name into table me->non_Table_Variable_Names.
  endmethod.


  method remove_Variable_Name.
    delete me->variable_Names             where table_Line = variable_Name.
    delete me->non_Table_Variable_Names   where table_Line = variable_Name.
  endmethod.


  method set_Table_Variable_Names.
    data: variable_Name type string.
    me->table_Variable_Names = variable_Names.
    loop at me->table_Variable_Names into variable_Name.
      insert variable_Name into table me->variable_Names.
      delete me->non_Table_Variable_Names where table_Line = variable_Name.
    endloop.
  endmethod.


  method set_Required_Table_Names.
    me->required_Table_Names = variable_Names.
  endmethod.


  method add_Table_Variable_Name.
    insert variable_Name into table me->table_Variable_Names.
    insert variable_Name into table me->variable_Names.
    delete me->non_Table_Variable_Names where table_Line = variable_Name.
  endmethod.


  method remove_Table_Variable_Name.
    read table me->required_Table_Names
      transporting no fields
      with key table_Line = variable_Name.
    if ( 0 eq sy-subrc ).
      return.
    endif.
    delete me->table_Variable_Names
      where table_Line = variable_Name.
    if ( 0 eq sy-subrc ).
      delete me->variable_Names               where table_Line = variable_Name.
    endif.
  endmethod.


  method clone.
    create object result.
    result->is_Likely_Ensured = me->is_Likely_Ensured.
    result->is_Likely_Ensured_By_Exit = me->is_Likely_Ensured_By_Exit.
    result->has_External_Content = me->has_External_Content.
    result->variable_Names = me->variable_Names.
    result->table_Variable_Names = me->table_Variable_Names.
    result->non_Table_Variable_Names = me->non_Table_Variable_Names.
    result->required_Table_Names = me->required_Table_Names.
    result->has_Been_Checked = me->has_Been_Checked.
    result->has_Been_Populated = me->has_Been_Populated.
    result->keyword_Type = me->keyword_Type.
    result->condition_Var_Names = me->condition_Var_Names.
    result->do_Check_Sys_Field_Next = me->do_Check_Sys_Field_Next.
    result->condition_State = me->condition_State.
  endmethod.

endclass.


class block_Stack definition.

  public section.
    methods:
      peek_Entry
        importing   index                   type i default 1
        returning   value(result)           type ref to block
        raising     not_Found_Failure,
      push_Entry importing entry type ref to block,
      pop_Entry  returning value(result) type ref to block,
      set_Entries_To_Nsrd_Upon_Exit,
      set_Entries_To_Nsrd_Upon_Rtrn.

  private section.
      types: ty_Entries type standard table of ref to block.
      data:  block_Entries type ty_Entries.

endclass.


interface listener.
    methods
      handle_Unsafe_Query
        importing   query_Ndx               type sytabix
                    table_Var_Name          type string
                    has_External_Content    type abap_Bool.

endinterface.


class listener_For_Ci definition.
  public section.
    interfaces listener.
    methods:
      constructor
        importing   adapter_To_Ci           type ref to ZCL_CI_005.

  private section.
    data:
      adapter_To_Ci    type ref to ZCL_CI_005.

endclass.


class variable_Declaration_Access definition.

  public section.
    types:
      begin of ty_Variable_Declaration,
        name                        type string,
        is_Local                    type abap_Bool,
        with_Header_Line            type abap_Bool,
      end of ty_Variable_Declaration,
      ty_Variable_Declarations type sorted table of ty_Variable_Declaration
        with unique key name.

    methods:
      constructor
        importing   scanned_Source       type ref to scanned_Source
                    statement_From       type syindex
                    statement_To         type syindex,
      get_Declaration
        importing   table_Variable_Name  type string
        returning   value(result)        type ty_Variable_Declaration.

  private section.
    data:
      begin of env,
        scanned_Source       type ref to scanned_Source,
        statement_From       type syindex,
        statement_To         type syindex,
      end of env,
      declarations         type ty_Variable_Declarations.

endclass.


class variable_Declaration_Access implementation.

  method constructor.
    me->env-scanned_Source = scanned_Source.
    me->env-statement_From = statement_From.
    me->env-statement_To = statement_To.
  endmethod.


  method get_Declaration.
    data:
      name_Normalized     type string,
      inline_Declaration  type string,
      postfix             type string,
      token               type stokesx.

    name_Normalized = table_Variable_Name.
    replace all occurrences of `[]` in name_Normalized with ``.
    replace all occurrences of `@` in name_Normalized with ``.

    read table me->declarations with key name = table_Variable_Name into result.
    if ( 0 eq sy-subrc ).
      return.
    endif.
    if ( name_Normalized cs `<` ).
      inline_Declaration = |FIELD-SYMBOL({ name_Normalized })| ##no_Text.
    else.
      inline_Declaration = |DATA({ name_Normalized })| ##no_Text.
    endif.

    result-name = name_Normalized.

    loop at me->env-scanned_Source->statements assigning field-symbol(<statement>)
      from me->env-statement_From
      to me->env-statement_To
      where type = scan_Stmnt_Type-standard.

      read table me->env-scanned_Source->tokens into token
        index <statement>-from.
      case token-str.
        when 'DATA' or 'STATICS' or 'FIELD-SYMBOLS'.
          read table me->env-scanned_Source->tokens into token
            index <statement>-from + 1.
          if ( token-str eq name_Normalized ).
            result-is_Local = abap_True.
            clear postfix.
            loop at me->env-scanned_Source->tokens into token
              from <statement>-from + 2 to <statement>-to.
              postfix = postfix && token-str && '-'.
            endloop.
            if ( postfix cs `WITH-HEADER-LINE` ).
              result-with_Header_Line = abap_True.
            endif.
          endif.
        when 'SELECT-OPTIONS'.
          read table me->env-scanned_Source->tokens into token
            index <statement>-from + 1.
          if ( token-str eq name_Normalized ).
            result-is_Local = abap_True.
            result-with_Header_Line = abap_True.
          endif.

        when others.
          loop at me->env-scanned_Source->tokens into token
            from <statement>-from + 1 to <statement>-to
            where str = inline_Declaration.
              result-is_Local = abap_True.
          endloop.
      endcase.

      if ( abap_True eq result-is_Local ).
        exit.
      endif.
    endloop.

    insert result into table me->declarations.
  endmethod.

endclass.


class for_All_Entries_Analysis definition.

  public section.
    methods:
      constructor
        importing   listener                type ref to listener
                    safe_Procedure_Names    type string_Hashed_Table optional,
      examine
        importing   scanned_Source          type ref to scanned_Source.

  private section.

  types:
    begin of ana_Context,
      statement_Ndx           type syindex,
      keyword                 type string,
      block_Stack             type ref to block_Stack,
      table_Variable_Name     type string,
      statement_From          type syindex,
      statement_To            type syindex,
      recursion_Level         type i,
      variable_Access         type ref to variable_Declaration_Access,
    end of ana_Context.

    data:
      system_Fields        type string_Hashed_Table,
      system_Tfill_Fields  type string_Hashed_Table,
      safe_Procedure_Names type string_Hashed_Table,
      scanned_Source       type ref to scanned_Source,
      listener             type ref to listener.

    methods:
      ana_Select_Statement
        importing   block_From_Ndx          type i
                    query_Ndx               type i
                    procedure_Name          type string
                    select_Statement        type sstmnt,

      ana_Size_Assignment
        importing   statement               type sstmnt
                    table_Var_Names         type string_Hashed_Table
        exporting   related_Var_Names       type string_Hashed_Table,

      ana_Sys_Field_Access
        importing   statement               type sstmnt
                    keyword                 type string
                    block                   type ref to block
        exporting   is_Sys_Field_Access     type abap_Bool,

      get_Keyword_For_Statement
        importing   statement               type sstmnt
        returning   value(result)           type string,

      can_Be_Altered_By_Call
        importing   statement               type sstmnt
                    table_Var_Names         type string_Hashed_Table
        returning   value(result)           type abap_Bool,

      is_Query_Appending_Table_Token
        importing   tokens                  type string_Hashed_Table
                    query_Statement         type sstmnt
        returning   value(result)           type abap_Bool,

      is_Query_Into_Table_Token
        importing   tokens                  type string_Hashed_Table
                    query_Statement         type sstmnt
        returning   value(result)           type abap_Bool,

      is_Safe_Procedure
        importing   procedure_Name          type string
        returning   value(result)           type abap_Bool,

      is_System_Field_Checked
        importing   statement               type sstmnt
        returning   value(result)           type abap_Bool,

      is_Value_Expression_Stmt
        importing   statement               type sstmnt
        returning   value(result)           type abap_Bool,

      is_Value_Functional_Call
        importing   statement               type sstmnt
        returning   value(result)           type abap_Bool,

      add_Var_Set_From_Sys_Field
        importing   statement               type sstmnt
                    block                   type ref to block,

      ana_Is_Cntnt_Ensured
        importing   statement_From          type sytabix
                    statement_To            type sytabix
                    table_Var_Name          type string
        exporting   is_Likely_Ensured       type abap_Bool
                    has_External_Content    type abap_Bool,

      ana_Is_Cntnt_Ensured_Rec
        importing   statement_From          type sytabix
                    statement_To            type sytabix
                    table_Var_Name          type string
                    recursion_Level         type i
        exporting   is_Likely_Ensured       type abap_Bool
                    is_Exit                 type abap_Bool
                    has_External_Content    type abap_Bool,

      ana_Block_Stmnt
        importing   statement               type sstmnt
                    context                 type ana_Context
        changing    block                   type ref to block
        raising     resumable(backtracking_With_Variable),

      ana_Non_Block_Stmnt
        importing   statement                   type sstmnt
                    block                       type ref to block
                    context                     type ana_Context
        raising     resumable(backtracking_With_Variable),

      ana_Is_Cntnt_Ensured_Btr
        exporting   is_Likely_Ensured           type abap_Bool
                    is_Exit                     type abap_Bool
                    has_External_Content        type abap_Bool
        changing    context                     type ana_Context
        raising     resumable(backtracking_With_Variable),

      initialize_Context
        importing   statement_From              type sytabix
                    statement_To                type sytabix
                    required_Table_Variables    type string_Table
                    recursion_Level             type i
                    table_Variable_Name         type string
        returning   value(result)               type ana_Context,

      is_Described
        importing   statement                   type sstmnt
                    table_Var_Names             type string_Hashed_Table
        returning   value(result)               type abap_Bool,

      is_Token_Referred_By_Stmnt
        importing   token_Words                 type string_Hashed_Table
                    statement                   type sstmnt
        returning   value(result)               type abap_Bool,

      is_Itab_Referred_By_Condition
        importing   variable_Names              type string_Hashed_Table
                    statement                   type sstmnt
                    variable_Access             type ref to variable_Declaration_Access
        exporting   is_Referred                 type abap_Bool
                    referenced_Variable         type string,

      get_Token_Referred_By_Stmnt
        importing   token_Words                 type string_Hashed_Table
                    statement                   type sstmnt
        returning   value(result)               type string,

      get_Stmnt_As_Text
        importing   statement                   type sstmnt
        returning   value(result)               type string,

      ana_Block_Stmnt_Condition
        importing   statement                   type sstmnt
                    statement_Ndx               type syindex
                    block                       type ref to block
                    context                     type ana_Context,

      ana_Block_Stmnt_End
        importing    previous_Block             type ref to block
                     block                      type ref to block,


      ana_Block_Stmnt_Elseif
        importing   statement                   type sstmnt
                    statement_Ndx               type syindex
                    previous_Block              type ref to block
                    block                       type ref to block
                    context                     type ana_Context,

      ana_Block_Stmnt_Else
        importing   previous_Block              type ref to block
                    block                       type ref to block,

      ana_Block_Stmnt_When
        importing   statement                   type sstmnt
                    statement_Ndx               type syindex
                    previous_Block              type ref to block
                    block                       type ref to block
                    context                     type ana_Context,

      is_Test_Of_System_Tfill
        importing   statement_Ndx               type i
                    block                       type ref to block
      returning     value(result)               type abap_Bool,

      report_Unsafe_Query
        importing   query_Ndx               type sytabix
                    table_Var_Name          type string
                    has_External_Content    type abap_Bool.

    constants:
      c_Block_Statements      type string
                              value `IF ELSEIF ENDIF CASE WHEN ENDCASE DO ENDDO LOOP ENDLOOP WHILE ENDWHILE`.

endclass.


class for_All_Entries_Analysis implementation.

  method constructor.
    me->listener = listener.
    insert `SY-SUBRC`   into table me->system_Fields.
    insert `SY-DBCNT`   into table me->system_Fields.
    insert `SYST-SUBRC` into table me->system_Fields.
    insert `SYST-DBCNT` into table me->system_Fields.

    insert `SY-TFILL`   into table me->system_Tfill_Fields.
    insert `SYST-TFILL` into table me->system_Tfill_Fields.

    if ( safe_Procedure_Names is not initial ).
      me->safe_Procedure_Names = safe_Procedure_Names.
    else.
      insert `/BOPF/IF_BUF_DATA_ACCESS_2~READ` into table me->safe_Procedure_Names.
      insert `/BOPF/IF_BUF_DATA_ACCESS_2~READ_BY_ASSOCIATION` into table me->safe_Procedure_Names.
    endif.
  endmethod.


  method examine.
    data:
      index            type syindex,
      keyword          type string,
      procedure_Name   type string,
      query_Ndx        type sytabix,
      module_Ndx       type sytabix,
      cursor_Ndx       type sytabix.
    field-symbols:
      <statement>      type sstmnt.

    me->scanned_Source = scanned_Source.
    loop at me->scanned_Source->statements assigning <statement>
      where
        type <>  scan_Stmnt_Type-comment           and
        type <>  scan_Stmnt_Type-macro_Definition  and
        type <>  scan_Stmnt_Type-unknown.
      index = sy-tabix.
      keyword  = get_Keyword_For_Statement( <statement> ).
      case keyword.
        when 'METHOD' or 'FORM' or 'FUNCTION' or 'MODULE'.
          module_Ndx = index.
          procedure_Name =
            me->scanned_Source->get_Token_Text_By_Index( <statement>-from + 1 ).

        when 'ENDMETHOD' or 'ENDFORM' or 'ENDFUNCTION' or 'ENDMODULE'.
          module_Ndx = 0.
        when 'SELECT'.
          ana_Select_Statement(
            block_From_Ndx = module_Ndx
            procedure_Name = procedure_Name
            query_Ndx = index
            select_Statement = <statement> ).
      endcase.
    endloop.
  endmethod.


  method can_Be_Altered_By_Call.
    data:
     last_Index type sytabix,
     highest_Index_With_Reference type sytabix,
     table_Var_Name type string.
    field-symbols:
      <token> type stokesx.

    highest_Index_With_Reference = statement-from.
    loop at table_Var_Names into table_Var_Name.
      loop at me->scanned_Source->tokens
        transporting no fields
        from ( highest_Index_With_Reference + 1 ) to statement-to
        where str = table_Var_Name.
        last_Index = sy-tabix.
        read table me->scanned_Source->tokens
          index ( last_Index - 1 )
          assigning <token>.
        if ( 0 eq sy-subrc and <token>-str = '=' ). " actual parameter
          highest_Index_With_Reference = sy-tabix.
        endif.
      endloop.
    endloop.

    if ( highest_Index_With_Reference eq statement-from ).
      " not referred
      result = abap_False.
      return.
    endif.

    loop at me->scanned_Source->tokens
      assigning <token>
      from statement-from to ( highest_Index_With_Reference - 1 ).
      case <token>-str.
        when 'CHANGING' or 'IMPORTING' or 'RECEIVING' or 'TABLES'.
          result = abap_True.
          return.
      endcase.
    endloop.
    result = abap_False.
  endmethod.


  method ana_Select_Statement.
    data:
      begin of found,
        for type abap_Bool,
        all type abap_Bool,
        entries type abap_Bool,
        in  type abap_Bool,
      end of found,
      ndx type sytabix,
      table_Var_Name type string,
      is_Likely_Ensured type abap_Bool,
      has_External_Content type abap_Bool,
      is_Local_Variable type abap_Bool.
    field-symbols:
      <cur_Token> type stokesx.

    if ( is_Safe_Procedure( procedure_Name ) ).
      return.
    endif.

    loop at me->scanned_Source->tokens
      assigning <cur_Token>
      from select_Statement-from to select_Statement-to.
      ndx = sy-tabix.

      case <cur_Token>-str.
        when 'FOR'.
          clear found.
          found-for = abap_True.
        when 'ALL'.
          if ( abap_True eq found-for ).
            found-all = abap_True.
          else.
            clear found.
          endif.
        when 'ENTRIES'.
          if ( abap_True eq found-all ).
            found-entries = abap_True.
          else.
            clear found.
          endif.
        when 'IN'.
          if ( abap_True eq found-entries ).
            found-in = abap_True.
            table_Var_Name =
              me->scanned_Source->get_Token_Text_By_Index( ndx + 1 ).
            replace all occurrences of '@' in table_Var_Name with space.
            exit.
          else.
            clear found.
          endif.
      endcase.
    endloop.

    if ( abap_False eq found-in or table_Var_Name is initial ).
      return.
    endif.

    ana_Is_Cntnt_Ensured(
      exporting statement_From =      block_From_Ndx
                statement_To =        query_Ndx - 1
                table_Var_Name =      table_Var_Name
      importing is_Likely_Ensured =      is_Likely_Ensured
                has_External_Content =   has_External_Content ).

    if ( abap_True eq is_Likely_Ensured ).
      return.
    endif.

    report_Unsafe_Query(
      query_Ndx =           query_Ndx
      table_Var_Name =      table_Var_Name
      has_External_Content = has_External_Content ).
  endmethod.


  method ana_Is_Cntnt_Ensured.
    ana_Is_Cntnt_Ensured_Rec(
      exporting statement_From =      statement_From
                statement_To =        statement_To
                table_Var_Name =      table_Var_Name
                recursion_Level =     0
      importing has_External_Content =   has_External_Content
                is_Likely_Ensured =      is_Likely_Ensured ).
  endmethod.


  method ana_Is_Cntnt_Ensured_Rec.
    data:
      context               type ana_Context,
      block                 type ref to block,
      required_Table_Names  type string_Table,
      count_Backtracking    type i,
      backtrack_Signal      type ref to backtracking_With_Variable.
    constants:
      c_Max_Recursion       type i value 3,
      c_Max_Backtracking    type i value 3.

    is_Likely_Ensured = abap_False.
    is_Exit = abap_False.
    has_External_Content = abap_False.

    insert table_Var_Name into table required_Table_Names.

    if ( recursion_Level > c_Max_Recursion  ).
      " too much - abort analysis assuming correctness
      is_Likely_Ensured = abap_True.
      return.
    endif.

    do.
      context =
        initialize_Context(
          exporting statement_From = statement_From
                    statement_To = statement_To
                    recursion_Level = recursion_Level
                    table_Variable_Name = table_Var_Name
                    required_Table_Variables = required_Table_Names ).
      try.
        ana_Is_Cntnt_Ensured_Btr(
          importing is_Likely_Ensured =       is_Likely_Ensured
                    is_Exit =                 is_Exit
                    has_External_Content =    has_External_Content
          changing  context =                 context ).
      catch before unwind backtracking_With_Variable into backtrack_Signal .
        if ( count_Backtracking > c_Max_Backtracking ).
          resume.
          assert 1 = 2.
        else.
          add 1 to count_Backtracking.
          insert backtrack_Signal->table_Variable into table required_Table_Names.
          continue. "do
        endif.
      endtry.
      exit. "do
    enddo.
  endmethod.


  method ana_Is_Cntnt_Ensured_Btr.

    is_Likely_Ensured = abap_False.
    is_Exit = abap_False.
    has_External_Content = abap_False.

    data(block) = context-block_Stack->peek_Entry( ).

    loop at me->scanned_Source->statements assigning field-symbol(<statement>)
      from context-statement_From to context-statement_To
      where
        type = scan_Stmnt_Type-standard or
        type = scan_Stmnt_Type-compute_Direct or
        type = scan_Stmnt_Type-method_Direct.
      context-statement_Ndx = sy-tabix.

      if ( block is not bound ).
        is_Likely_Ensured = abap_False.
        has_External_Content = abap_False.
        return.
      endif.

      context-keyword = get_Keyword_For_Statement( <statement> ).

      if ( abap_True eq block->do_Check_Sys_Field_Next ).
        case context-keyword.
          when 'IF' or 'CASE' or 'MOVE' or 'COMPUTE' or 'CHECK' or 'ASSERT'.
            block->set_Do_Check_Sys_Field_Next( abap_False ).
            ana_Sys_Field_Access(
              exporting statement = <statement>
                        keyword = context-keyword
                        block = block ).
          when others.
            if ( c_Block_Statements ns context-keyword ).
              block->set_Do_Check_Sys_Field_Next( abap_False ).
            endif.
        endcase.
      endif.

      " block relevant statements
      if c_Block_Statements cs context-keyword.
        ana_Block_Stmnt(
          exporting statement = <statement>
                    context = context
          changing  block = block ).
        if ( block is not bound ).
          raise exception type internal_Error
            exporting
              hint = get_Stmnt_As_Text( <statement> ).
        endif.

      elseif ( abap_False eq block->is_Final ).
        ana_Non_Block_Stmnt(
          statement = <statement>
          block = block
          context = context ).
      endif.
    endloop.

    is_Likely_Ensured = block->is_Likely_Ensured.
    is_Exit = block->is_Likely_Ensured_By_Exit.
    has_External_Content = block->has_External_Content.

    if ( abap_False eq is_Likely_Ensured and abap_False eq has_External_Content ).
      data(declaration) = context-variable_Access->get_Declaration( context-table_Variable_Name ).
      if ( abap_False eq declaration-is_Local ).
        has_External_Content = abap_True.
      endif.
    endif.
  endmethod.


  method ana_Block_Stmnt.
    data:
      is_Referred         type abap_Bool,
      previous_Block      type ref to block.

    case context-keyword.
      when 'IF' or 'CASE'.
        context-block_Stack->push_Entry( block ).
        ana_Block_Stmnt_Condition(
          statement = statement
          statement_Ndx = context-statement_Ndx
          block = block
          context = context ).
       if ( 'CASE' eq context-keyword ).
         block->set_Condition_State( state = value #( any_Branch_Populates = abap_True )  is_Final = abap_True ).
       endif.

      when 'WHEN'.
        " when carries over the case / or a preceding when scope
        previous_Block = block.
        block = context-block_Stack->pop_Entry( ).

        context-block_Stack->push_Entry( block ).
        ana_Block_Stmnt_When(
          statement = statement
          statement_Ndx = context-statement_Ndx
          previous_Block = previous_Block
          block = block
          context = context ).

      when 'ELSEIF'.
        previous_Block = block.
        block = context-block_Stack->pop_Entry( ).
        if ( abap_True eq previous_Block->do_Check_Sys_Field_Next ).
          block->do_Check_Sys_Field_Next = abap_True.
        endif.
        context-block_Stack->push_Entry( block ).
        ana_Block_Stmnt_Elseif(
          statement = statement
          statement_Ndx = context-statement_Ndx
          previous_Block = previous_Block
          block = block
          context = context ).

      when 'ELSE'.
        previous_Block = block.
        block = context-block_Stack->pop_Entry( ).
        if ( abap_True eq previous_Block->do_Check_Sys_Field_Next ).
          block->do_Check_Sys_Field_Next = abap_True.
        endif.
        context-block_Stack->push_Entry( block ).

        ana_Block_Stmnt_Else(
          previous_Block = previous_Block
          block = block ).

      when 'ENDIF' or 'ENDCASE'.
        previous_Block = block.
        block = context-block_Stack->pop_Entry( ).
        if ( abap_True eq previous_Block->is_Likely_Ensured and abap_False eq block->has_Been_Populated ).
          ana_Block_Stmnt_End(
            previous_Block = previous_Block
            block = block ).
        endif.

      when 'DO' or 'LOOP' or 'WHILE'.
        context-block_Stack->push_Entry( block ).
        block->set_Type_Loop( ).
       if ( is_Token_Referred_By_Stmnt( statement =  statement token_Words = block->variable_Names ) ).
          block->set_Is_Ensured( ).
        endif.

      when 'ENDLOOP' or 'ENDDO' or 'ENDWHILE'.
        " checking for population avoids false positives but
        " also hides true issues as the variable looped at is not
        " checked for content
        previous_Block = block.
        block = context-block_Stack->pop_Entry( ).
        if ( abap_True eq previous_Block->is_Likely_Ensured and abap_False eq block->has_Been_Populated ).
          ana_Block_Stmnt_End(
            previous_Block = previous_Block
            block = block ).
        endif.

      when others.
        assert 1 = 2.
    endcase.
  endmethod.


  method ana_Non_Block_Stmnt.
    data: statement_As_Text type string.
    data:
      do_Check_Sys_Field    type abap_Bool,
      has_Been_Checked      type abap_Bool,
      is_Referred           type abap_Bool,
      is_Locally_Declared   type abap_Bool,
      has_External_Content  type abap_Bool,
      is_Likely_Ensured     type abap_Bool,
      is_Exit               type abap_Bool,
      do_Propagate          type abap_Bool,
      buffer                type string_Hashed_Table,
      token                 type stokesx,
      count                 type i,
      source_Var            type string,
      target_Var            type string,
      initial_Variables     type string_Hashed_Table.

    initial_Variables = block->required_Table_Names.

    case context-keyword.
      when 'MODIFY' or 'INSERT' or 'APPEND' or 'COLLECT' or 'SPLIT'.
        is_Referred = is_Token_Referred_By_Stmnt(
          statement =  statement
          token_Words   =   block->table_Variable_Names ).
        if ( abap_True eq is_Referred ).
          block->set_Has_Been_Populated( ).
          block->set_Is_Ensured( ).
        endif.

      when 'ASSIGN'.
        read table me->scanned_Source->tokens into token
          index statement-to.
        target_Var = token-str.
        if ( target_Var ns `<` ).
          return.
        endif.
        read table me->scanned_Source->tokens into token
          index ( statement-from + 1 ).
        source_Var = token-str.
        read table block->table_Variable_Names
          transporting no fields
          with key table_Line = source_Var.
        if ( 0 eq sy-subrc ).
          block->add_Table_Variable_Name( target_Var ).
        else.
          read table initial_Variables
            transporting no fields
            with key table_Line = target_Var.
          if ( 0 eq sy-subrc ).
            " the query has been done with a field-symbol which gets assigned now
            " consequently the source variable shall be a table of interest.
            " the source variable might have been sanitized already
            " => back to start with no variable set
            raise resumable exception type backtracking_With_Variable
              exporting table_Variable = source_Var.
          else.
            block->remove_Table_Variable_Name( target_Var ).
          endif.
        endif.

      when 'UNASSIGN'.
        read table me->scanned_Source->tokens into token
          index statement-to.
        target_Var = token-str.
        if ( target_Var ns `<` ).
          return.
        endif.
        block->remove_Table_Variable_Name( target_Var ).

      when 'REFRESH' or 'CLEAR'.
        is_Referred = is_Token_Referred_By_Stmnt(
          statement = statement
          token_Words  =   block->table_Variable_Names ).
        if ( abap_True eq is_Referred ).
          block->set_Is_Ensured( abap_False ).
          block->set_Variable_Names( block->table_Variable_Names ).
        else.
          read table me->scanned_Source->tokens into token
            index statement-from + 1.
          target_Var = token-str.
          block->remove_Variable_Name( target_Var ).
        endif.

      when 'DELETE'.
        is_Referred = is_Token_Referred_By_Stmnt(
          statement = statement
          token_Words  =   block->table_Variable_Names ).
        if ( abap_False eq is_Referred ).
          return.
        endif.
        " delete has many syntax variants, making statement analysis with scan
        " a little bit challenging
        " db syntax is not relevant
        " - delete from { dbtab ] ..
        " - delete { dbtab } from ..
        " adjacent duplicates is harmless.
        " - delete adjacent duplicates ..
        " all other syntax variants considered to potentially cause the itab
        " to be empty (similar to clear). There might be false positives, on
        " the other hand not checking the delete statement caused lots of false
        " negatives.
        statement_As_Text = get_Stmnt_As_Text( statement ).
        if ( statement_As_Text cs `ADJACENT DUPLICATES` or
             statement_As_Text cs `DELETE FROM `).
          return.
        endif.
        read table me->scanned_Source->tokens into token
          index statement-from + 1.
        read table block->table_Variable_Names transporting no fields
          with key table_Line = token-str.
        if ( 0 eq sy-subrc ).
          block->set_Variable_Names( block->table_Variable_Names ).
          block->set_Is_Ensured( abap_False ).
        endif.

      when 'SELECT' or 'FETCH'. " into / appending table
        is_Referred = is_Query_Into_Table_Token(
          query_Statement = statement
          tokens  =   block->table_Variable_Names ).
        if ( abap_True eq is_Referred ).
          block->set_Is_Ensured( abap_False ).
          block->set_Do_Check_Sys_Field_Next( ).
        elseif ( abap_False eq block->is_Likely_Ensured ).
          is_Referred = is_Query_Appending_Table_Token(
            query_Statement = statement
            tokens  =   block->table_Variable_Names ).
          if ( abap_True eq is_Referred ).
            block->set_Do_Check_Sys_Field_Next( ).
          endif.
        endif.

      when 'CALL'.
        if ( can_Be_Altered_By_Call( statement = statement table_Var_Names = block->table_Variable_Names ) ).
          block->set_Has_External_Content( ).
        endif.

      when 'PERFORM'.
        if ( is_Token_Referred_By_Stmnt( statement = statement  token_Words = block->table_Variable_Names ) ).
          block->set_Has_External_Content( ).
        endif.

      when 'READ'.
        if ( abap_True = block->is_Likely_Ensured ).
          return.
        endif..
        if ( is_Token_Referred_By_Stmnt( statement = statement  token_Words = block->table_Variable_Names ) ).
          block->set_Do_Check_Sys_Field_Next( ).
        endif.

      when 'DESCRIBE'.
        ana_Size_Assignment(
          exporting     statement = statement
                        table_Var_Names = block->table_Variable_Names
          importing     related_Var_Names = buffer ).
        loop at buffer into target_Var.
          block->add_Variable_Name( target_Var ).
        endloop.

      when 'MOVE'.
        is_Referred = is_Token_Referred_By_Stmnt(
          statement = statement
          token_Words  =   block->table_Variable_Names ).
        if ( abap_False eq is_Referred ).
          return.
        endif.

        read table me->scanned_Source->tokens into token
          index statement-to.
        target_Var = token-str.
        read table me->scanned_Source->tokens into token
          index statement-from + 1.
        source_Var = token-str.
        if ( source_Var eq target_Var ).
          return.
        endif.

        read table block->table_Variable_Names
          transporting no fields
          with table key table_Line =  target_Var.
        if ( 0 ne sy-subrc ).
          " assignment to other table var
          block->add_Variable_Name( target_Var ).

        else.
          ana_Is_Cntnt_Ensured_Rec(
            exporting   table_Var_Name =  source_Var
                        statement_From =  context-statement_From
                        statement_To =    context-statement_Ndx
                        recursion_Level = context-recursion_Level + 1
            importing   has_External_Content = has_External_Content
                        is_Likely_Ensured = is_Likely_Ensured
                        is_Exit = is_Exit  ).
          if ( abap_True eq is_Likely_Ensured ).
            if ( abap_True eq is_Exit ).
              context-block_Stack->set_Entries_To_Nsrd_Upon_Exit( ).
              block->set_Is_Ensured_By_Exit_Final( ).
            else.
              block->set_Is_Ensured( ).
            endif.
          else.
            block->set_Is_Ensured( abap_False ).
            block->set_Has_External_Content( has_External_Content ).
          endif.
        endif.

      when 'COMPUTE'.
        " x = y /
        " x = y + aggregation ( z ) + ..... /
        " x = method( )
        " x = expression
        if ( not is_Token_Referred_By_Stmnt( statement = statement  token_Words  =   block->variable_Names ) ).
          return.
        endif.
        read table me->scanned_Source->tokens into token
          index statement-from.
        target_Var = token-str.
        read table block->variable_Names
          transporting no fields
          with table key table_Line =  target_Var.
        if ( 0 ne sy-subrc ).
          " table information assigned to
          " assignment to other table variable
          block->add_Variable_Name( target_Var ).
          return.
        endif.
        read table block->table_Variable_Names
          transporting no fields
          with table key table_Line =  target_Var.
        if ( 0 ne sy-subrc ).
          " no table variable - actually this could be
          " something like cnt_Lines = sy-dbcnt
          " => not decidable without major rework
          return.
        endif.

        if ( is_Value_Functional_Call( statement ) ).
          block->set_Has_External_Content( abap_True ).
          return.
        endif.

        if ( is_Value_Expression_Stmt( statement ) ).
          block->set_Is_Ensured( ).
          block->set_Has_Been_Populated( ).
          return.
        endif.

        loop at me->scanned_Source->tokens into token from statement-from + 2 where str <> '('.
          exit.
        endloop.
        source_Var = token-str.

        ana_Is_Cntnt_Ensured_Rec(
          exporting     table_Var_Name =  source_Var
                        statement_From =  context-statement_From
                        statement_To =    context-statement_Ndx
                        recursion_Level = context-recursion_Level + 1
          importing     has_External_Content = has_External_Content
                        is_Likely_Ensured = is_Likely_Ensured
                        is_Exit = is_Exit ).
        if ( abap_True eq is_Likely_Ensured ).
          if ( abap_True eq is_Exit ).
            context-block_Stack->set_Entries_To_Nsrd_Upon_Exit( ).
            block->set_Is_Ensured_By_Exit_Final( ).
          else.
            block->set_Is_Ensured( ).
          endif.
        else.
          block->set_Is_Ensured( abap_False ).
          block->set_Has_External_Content( has_External_Content ).
        endif.

      when 'RETURN' or 'RAISE' or 'LEAVE'.
        if ( abap_True = block->is_Likely_Ensured ).
          context-block_Stack->set_Entries_To_Nsrd_Upon_Rtrn( ).
          block->set_Is_Ensured_By_Exit_Final( ).
          block->set_Is_Final( ).
        endif.

      when 'CHECK' or 'ASSERT'.
        is_Referred = is_Token_Referred_By_Stmnt(
          statement =  statement
          token_Words   =   block->variable_Names ).
        if ( abap_True eq is_Referred ).
          context-block_Stack->set_Entries_To_Nsrd_Upon_Exit( ).
          block->set_Is_Ensured_By_Exit( ).
          block->set_Is_Final( ).
        endif.

      when 'MESSAGE'.
        if ( abap_False = block->is_Likely_Ensured ).
          return.
        endif.
        loop at me->scanned_Source->tokens
          transporting no fields
          from statement-from
          to statement-to where str = 'RAISING'.
          context-block_Stack->set_Entries_To_Nsrd_Upon_Rtrn( ).
          block->set_Is_Ensured_By_Exit_Final( ).
          exit. " inner loop
        endloop.

      when 'EXIT' or 'CONTINUE'.
        if ( abap_True = block->is_Likely_Ensured ).
          block->set_Is_Final( ).
          case block->keyword_Type.
            when
              block=>c_Keyword_Type-loop_Table or
              block=>c_Keyword_Type-loop_Other.
            when others.
              context-block_Stack->set_Entries_To_Nsrd_Upon_Exit( ).
          endcase.
        endif.
    endcase.
  endmethod.


  method is_Test_Of_System_Tfill.

    read table me->scanned_Source->statements index statement_Ndx assigning field-symbol(<statement>).
    result = is_Token_Referred_By_Stmnt(
      statement = <statement>
      token_Words  =    me->system_Tfill_Fields ).
    if ( abap_False eq result ).
      return.
    endif.

    " get preceding statement, ignore comment
    data(prev_No_Comment_Statement_Ndx) = statement_Ndx - 1.
    do.
      read table me->scanned_Source->statements
        index prev_No_Comment_Statement_Ndx
        assigning <statement>.
      if ( 0 ne sy-subrc ).
        result = abap_False.
        return.
      endif.
      case <statement>-type.
        when scan_Stmnt_Type-comment or scan_Stmnt_Type-comment_In_Stmnt.
          prev_No_Comment_Statement_Ndx = prev_No_Comment_Statement_Ndx - 1.
          continue.
        when others.
          exit.
      endcase.
    enddo.

    result = is_Described(
      statement = <statement>
      table_Var_Names = block->variable_Names ).
  endmethod.


  method ana_Block_Stmnt_Condition.
    data:
      is_Referred type abap_Bool,
      var_Name type string,
      non_Table_Variables type string_Hashed_Table.

    is_Itab_Referred_By_Condition(
      exporting   statement = statement
                  variable_Names = block->table_Variable_Names
                  variable_Access = context-variable_Access
      importing   is_Referred = is_Referred
                  referenced_Variable = var_Name ).

    if ( abap_True eq is_Referred ).
      block->set_Type_Condition_On_Table( var_Name ).
      return.
    endif.

    is_Referred = is_Token_Referred_By_Stmnt(
      statement =  statement
      token_Words  =    block->non_Table_Variable_Names ).
    if ( abap_True eq is_Referred ).
      var_Name = get_Token_Referred_By_Stmnt(
        statement =  statement
        token_Words  =    block->non_Table_Variable_Names ).
      block->set_Type_Condition_On_Variable( var_Name ).
      return.
    endif.

    is_Referred = is_Test_Of_System_Tfill(
      statement_Ndx = statement_Ndx
      block = block ).
    if ( abap_True eq is_Referred ).
      block->set_Type_Condition_On_Variable( `SY-TFILL` ).
      block->set_Has_Been_Checked( ).
      return.
    endif.

    block->set_Type_Condition_Other( ).
  endmethod.


  method ana_Block_Stmnt_End.
    check abap_True eq previous_Block->has_Been_Populated and abap_False eq block->has_Been_Populated.
    case previous_Block->keyword_Type.
      when block=>c_Keyword_Type-condition_Other.
        " check if branches cover all possible paths and whether each branch populates the internal table
        data(condition_State) = previous_Block->get_Condition_State( ).
        if ( abap_True eq condition_State-any_Branch_Populates and abap_True eq condition_State-has_Others_Branch ).
          block->set_Has_Been_Populated( ).
        endif.

      when block=>c_Keyword_Type-loop_Other.
        " cannot check whether loop body will get processed at all, but lucky guess is the loop is processed
        block->set_Has_Been_Populated( ).

      when block=>c_Keyword_Type-condition_On_Variable or block=>c_Keyword_Type-condition_On_Table.
        block->set_Has_Been_Populated( ).
    endcase.
  endmethod.


  method ana_Block_Stmnt_Elseif.
     loop at previous_Block->condition_Var_Names into data(var_Name).
      exit.
    endloop.
    case previous_Block->keyword_Type.
      when block=>c_Keyword_Type-condition_On_Table.
        block->set_Type_Condition_On_Table( var_Name ).

      when block=>c_Keyword_Type-condition_On_Variable.
        block->set_Type_Condition_On_Variable( var_Name ).

      when others.
        ana_Block_Stmnt_Condition(
          statement = statement
          statement_Ndx = statement_Ndx
          block = block
          context = context ).
    endcase.
    block->set_Condition_State( previous_Block->get_Condition_State( ) ).

  endmethod.


  method ana_Block_Stmnt_Else.
    loop at previous_Block->condition_Var_Names into data(var_Name).
      exit.
    endloop.

    case previous_Block->keyword_Type.
      when block=>c_Keyword_Type-condition_On_Table.
        block->set_Type_Condition_On_Table( var_Name ).

      when block=>c_Keyword_Type-condition_On_Variable.
        block->set_Type_Condition_On_Variable( var_Name ).

      when others.
        block->set_Type_Condition_Other( ).
    endcase.

    data(else_State) = previous_Block->get_Condition_State( ).
    else_State-has_Others_Branch = abap_True.
    block->set_Condition_State( else_State ).
   endmethod.


  method ana_Block_Stmnt_When.
    loop at previous_Block->condition_Var_Names into data(var_Name).
      exit.
    endloop.
    case previous_Block->keyword_Type.
      when block=>c_Keyword_Type-condition_On_Table.
        block->set_Type_Condition_On_Table( var_Name ).

      when block=>c_Keyword_Type-condition_On_Variable.
        block->set_Type_Condition_On_Variable( var_Name ).

      when others.
        ana_Block_Stmnt_Condition(
          statement = statement
          statement_Ndx = statement_Ndx
          block = block
          context = context ).
    endcase.

    data(condition_State) = previous_Block->get_Condition_State( ).
    read table me->scanned_Source->tokens
      into data(token)
      index statement-from + 1.
    if ( 'OTHERS' = token-str ).
      condition_State-has_Others_Branch = abap_True.
    endif.
    block->set_Condition_State( condition_State ).

  endmethod.


  method is_Value_Expression_Stmt.
    " x = VALUE TYPE( ....
    constants:
      c_Char_Plus_Bracket type c length 10 value '..*\('.

    read table me->scanned_Source->tokens
      into data(token)
      index statement-from + 1.
    if ( token-str <> '=' ).
      result = abap_False.
      return.
    endif.

    read table me->scanned_Source->tokens
      into token
      index statement-from + 2.
    if ( token-str <> 'VALUE' ).
      result = abap_False.
      return.
    endif.

    read table me->scanned_Source->tokens
      into token
      index statement-from + 3.
    if ( not contains( val = token-str regex = c_Char_Plus_Bracket ) ).
      result = abap_False.
      return.
    endif.

    result = abap_True.
  endmethod.


  method is_Value_Functional_Call.
    " x = method( ....
    data:
      token               type stokesx.
    constants:
      c_Chars_Plus_Bracket type c length 10 value '..*\('.

    read table me->scanned_Source->tokens
      into token
      index statement-from + 1.
    if ( token-str <> '=' ).
      result = abap_False.
      return.
    endif.

    read table me->scanned_Source->tokens
      into token
      index statement-from + 2.
    if ( not contains( val = token-str regex = c_Chars_Plus_Bracket ) ).
      result = abap_False.
      return.
    endif.

    result = abap_True.
  endmethod.


  method ana_Sys_Field_Access.
    is_Sys_Field_Access =
      is_Token_Referred_By_Stmnt(
        statement =  statement
        token_Words  =    me->system_Fields ).
    if ( abap_False eq is_Sys_Field_Access ).
      return.
    endif.

    case keyword.
      when 'MOVE' or 'COMPUTE'.
        add_Var_Set_From_Sys_Field(
          statement = statement
          block = block ).

      when others.
        is_Sys_Field_Access = is_System_Field_Checked( statement ).
        if ( abap_True eq is_Sys_Field_Access ).
          block->set_Is_Ensured( ).
          block->set_Has_Been_Checked( ).
        endif.
    endcase.
  endmethod.


  method ana_Size_Assignment.
    data:
      ndx               type sytabix,
      table_Var_Name    type string,
      token             type stokesx,
      keyword           type string,
      refers_To_Table   type abap_Bool.

    clear related_Var_Names[].

    loop at table_Var_Names into table_Var_Name.
      loop at me->scanned_Source->tokens
        transporting no fields
        from statement-from to statement-to
        where str = table_Var_Name.
        refers_To_Table = abap_True.
        exit.
      endloop.
      if ( abap_True eq refers_To_Table ).
        exit.
      endif.
    endloop.

    if ( abap_False eq refers_To_Table ).
      return.
    endif.

    read table
      me->scanned_Source->tokens index statement-from
      into token.
    keyword = token-str.
    case keyword.
      when 'DESCRIBE'.
        ndx = statement-from + 1.
        read table
          me->scanned_Source->tokens index ndx
          into token.
        if ( token-str ne 'TABLE' ).
          return.
        endif.

        ndx = statement-from + 2.
        read table
          me->scanned_Source->tokens index ndx
          into token.
        if ( token-str ne table_Var_Name ).
          return.
        endif.

        ndx = statement-from + 3.
        read table
          me->scanned_Source->tokens index ndx
          into token.
        if ( token-str ne 'LINES' ).
          return.
        endif.

        ndx = statement-from + 4.
        read table
          me->scanned_Source->tokens index ndx
          into token.
        if ( 0 eq sy-subrc ).
          insert token-str into table related_Var_Names.
        endif.

    endcase.
  endmethod.


  method get_Keyword_For_Statement.
    data:
      token like line of scanned_Source->tokens.

    case statement-type.
      when scan_Stmnt_Type-compute_Direct.
        result = 'COMPUTE'.

      when scan_Stmnt_Type-method_Direct.
        result = 'CALL'.

      when others.
        read table scanned_Source->tokens index statement-from into token.
        result = token-str.

    endcase.
  endmethod.


  method is_Query_Into_Table_Token.
    data:
      ndx_Into     type sytabix,
      ndx_Table    type sytabix,
      ndx_Tab_Var  type sytabix,
      token        type stokesx,
      normalized   type string.

    loop at me->scanned_Source->tokens
      transporting no fields
      from query_Statement-from to query_Statement-to
      where   str = 'INTO'.
      ndx_Into = sy-tabix.
      exit.
    endloop.
    if ( 0 = ndx_Into ).
      result = abap_False. return.
    endif.

    loop at me->scanned_Source->tokens
      transporting no fields
      from ndx_Into to query_Statement-to
      where   str = 'TABLE'.
      ndx_Table = sy-tabix.
      exit.
    endloop.
    if ( 0 = ndx_Table ).
      result = abap_False. return.
    endif.

    ndx_Tab_Var = ndx_Table + 1.
    read table me->scanned_Source->tokens index ndx_Tab_Var into token.
    if ( 0 ne sy-subrc or token-str is initial ).
      result = abap_False. return.
    endif.

    " normalize host var tagging
    normalized = token-str.
    replace all occurrences of '@' in normalized with space.

    read table tokens
      with key table_Line = normalized
      transporting no fields.
    if ( 0 ne sy-subrc ).
      result = abap_False. return.
    endif.

    result = abap_True.
  endmethod.


  method is_Query_Appending_Table_Token.
    data:
      ndx_Into     type sytabix,
      ndx_Table    type sytabix,
      ndx_Tab_Var  type sytabix,
      token        type stokesx.

    loop at me->scanned_Source->tokens
      transporting no fields
      from query_Statement-from to query_Statement-to
      where   str = 'APPENDING'.
      ndx_Into = sy-tabix.
      exit.
    endloop.
    if ( 0 = ndx_Into ).
      result = abap_False. return.
    endif.

    loop at me->scanned_Source->tokens
      transporting no fields
      from ndx_Into to query_Statement-to
      where   str = 'TABLE'.
      ndx_Table = sy-tabix.
      exit.
    endloop.
    if ( 0 = ndx_Table ).
      result = abap_False. return.
    endif.

    ndx_Tab_Var = ndx_Table + 1.
    read table me->scanned_Source->tokens index ndx_Tab_Var into token.
    if ( 0 ne sy-subrc or token-str is initial ).
      result = abap_False. return.
    endif.

    read table tokens
      with key table_Line = token-str
      transporting no fields.
    if ( 0 ne sy-subrc ).
      result = abap_False. return.
    endif.

    result = abap_True.
  endmethod.


  method is_System_Field_Checked.
    data:
      keyword        type string.
    keyword = get_Keyword_For_Statement( statement ).
    case keyword.
      when 'IF' or 'CASE' or 'CHECK' or 'ASSERT'.
        result = is_Token_Referred_By_Stmnt(
          statement = statement
          token_Words = me->system_Fields ).
        when others.
          result = abap_False.
    endcase.
  endmethod.


  method is_Safe_Procedure.
   read table me->safe_Procedure_Names
     with key table_Line = procedure_Name
     transporting no fields.
   if ( 0 eq sy-subrc ).
     result = abap_True.
   else.
     result = abap_False.
   endif.
  endmethod.


  method add_Var_Set_From_Sys_Field.
    data:
      is_Referred       type abap_Bool,
      target_Var        type string,
      token             type stokesx,
      keyword           type string.

    is_Referred = is_Token_Referred_By_Stmnt(
      statement = statement
      token_Words = me->system_Fields ).
    if ( abap_False eq is_Referred ).
      return.
    endif.

    keyword = get_Keyword_For_Statement( statement ).
    case keyword.
      when 'COMPUTE'.
        " x = sy-subrc
        read table  me->scanned_Source->tokens
          index statement-from
          into token.
        if ( 0 ne sy-subrc ).
          exit.
        endif.
        target_Var = token-str.
        block->add_Variable_Name( target_Var ).
      when 'MOVE'.
        " move sy-subrc to x.
        read table  me->scanned_Source->tokens
          index statement-to
          into token.
        if ( 0 ne sy-subrc ).
          exit.
        endif.
        target_Var = token-str.
        block->add_Variable_Name( target_Var ).
    endcase.
  endmethod.


  method initialize_Context.
    data:
      block type ref to block,
      table_Variable type string,
      table_Variables type string_Hashed_Table.

    block = new block( ).
    loop at required_Table_Variables into table_Variable.
      replace all occurrences of '@' in table_Variable with space.
      insert table_Variable into table table_Variables.
      if ( table_Variable cs '[]' ).
        replace '[' in table_Variable with ''.
        replace ']' in table_Variable with ''.
      else.
        table_Variable = table_Variable && '[]'.
      endif.
      insert table_Variable into table table_Variables.
    endloop.
    block->set_Table_Variable_Names( table_Variables ).
    block->set_Required_Table_Names( table_Variables ).

    result-statement_From = statement_From.
    result-statement_To = statement_To.
    result-recursion_Level = recursion_Level.
    result-table_Variable_Name = table_Variable_Name.
    create object result-block_Stack.
    result-block_Stack->push_Entry( block ).

    create object result-variable_Access
      exporting scanned_Source = me->scanned_Source
                statement_From = result-statement_From
                statement_To = result-statement_To.
  endmethod.


  method is_Described.
    data(keyword) = get_Keyword_For_Statement( statement ).
    if ( 'DESCRIBE' ne keyword ).
      result = abap_False.
      return.
    endif.

    result = is_Token_Referred_By_Stmnt(
      statement =  statement
      token_Words  =    table_Var_Names ).
  endmethod.


  method is_Token_Referred_By_Stmnt.
    if ( statement-type = scan_Stmnt_Type-compute_Direct ).
      data(ndx_From) = statement-from.
    else.
      ndx_From = statement-from + 1.
    endif.
    loop at token_Words into data(token).
      loop at me->scanned_Source->tokens
        transporting no fields
        from ndx_From    to   statement-to
        where     str = token.
        result = abap_True.
        return.
      endloop.
    endloop.

    result = abap_False.
  endmethod.


  method is_Itab_Referred_By_Condition.
    data:
      variable_Name type string,
      postfix type string,
      ndx_Cursor type syindex,
      token type stokesx,
      declaration type variable_Declaration_Access=>ty_Variable_Declaration.

    " table with header lines use the same name for a structure and also an
    " internal table. The condition IS INITIAL requires an exact specification
    " in such an case. Please note the check below requires that the declaration
    " is known (which is not the case for variables not declared outside the
    " current context). In other words this heuristic covers some cases but also
    " misses many others.

    loop at variable_Names into variable_Name.
      loop at me->scanned_Source->tokens transporting no fields
        from      statement-from + 1  to statement-to
        where     str = variable_Name.
        ndx_Cursor = sy-tabix.
        if ( variable_Name cs `[` ).
          referenced_Variable = variable_Name.
          is_Referred = abap_True.
          return.
        endif.
        declaration = variable_Access->get_Declaration( variable_Name ).
        if ( declaration-with_Header_Line = abap_False ).
          referenced_Variable = variable_Name.
          is_Referred = abap_True.
          return.
        endif.

        clear postfix.
        loop at me->scanned_Source->tokens into token
          from      ndx_Cursor + 1  to ndx_Cursor + 3.
          postfix = postfix && token-str && `-`.
          if ( postfix eq ')' ).
            exit.
          endif.
        endloop.
        if ( postfix cs `IS-INITIAL` or postfix cs `IS-NOT-INITIAL`  ).
          " condition checks header line and not internal table => ignore
          continue.
        else.
          referenced_Variable = variable_Name.
          is_Referred = abap_True.
          return.
        endif.
      endloop.
    endloop.
    clear referenced_Variable.
    is_Referred = abap_False.
  endmethod.


  method get_Token_Referred_By_Stmnt.
    data:
      token type string,
      ndx_From type syindex.

    if ( statement-type = scan_Stmnt_Type-compute_Direct ).
      ndx_From = statement-from.
    else.
      ndx_From = statement-from + 1.
    endif.
    loop at token_Words into token.
      loop at me->scanned_Source->tokens
        transporting no fields
        from ndx_From    to   statement-to
        where     str = token.
        result = token.
        return.
      endloop.
    endloop.
  endmethod.


  method get_Stmnt_As_Text.
    data: token type stokesx.
    loop at me->scanned_Source->tokens
      into token
      from statement-from    to   statement-to.
      if ( result is initial ).
        result = token-str.
      else.
        concatenate result ` ` token-str into result ##no_Text.
      endif.
    endloop.
  endmethod.


  method report_Unsafe_Query.
    data: table_Var_Name_Normalized type string.
    table_Var_Name_Normalized = table_Var_Name.
    replace all occurrences of '[]'
      in table_Var_Name_Normalized
      with space.
    me->listener->handle_Unsafe_Query(
       query_Ndx = query_Ndx
       table_Var_Name = table_Var_Name_Normalized
       has_External_Content = has_External_Content ).
  endmethod.

endclass.


class scanned_Source implementation.

   method constructor.
     me->tokens =     tokens.
     me->statements = statements.
     me->levels =     levels.
   endmethod.


   method get_Token_Text_By_Index.
     data: token type stokesx.
     read table me->tokens into token index !index.
     result = token-str.
   endmethod.

endclass.


class ZCL_CI_005 definition
    local friends listener_For_Ci.

class listener_For_Ci implementation.

  method constructor.
    me->adapter_To_Ci = adapter_To_Ci.
  endmethod.


  method listener~handle_Unsafe_Query.
    me->adapter_To_Ci->report_Unsafe_Query(
      query_Ndx =       query_Ndx
      table_Var_Name =  table_Var_Name
      has_External_Content = has_External_Content ).
  endmethod.

endclass.


class block_Stack implementation.

  method peek_Entry.
    read table block_Entries index !index into result.
    if ( 0 ne sy-subrc ).
      raise exception type not_Found_Failure.
    endif.
  endmethod.


  method push_Entry.
    insert entry->clone( ) into block_Entries index 1.
  endmethod.


  method pop_Entry.
    read table block_Entries index 1 into result.
    delete block_Entries index 1.
  endmethod.


  method set_Entries_To_Nsrd_Upon_Exit.
    data: block type ref to block.
    data: condition_Level type i.
    loop at me->block_Entries into block.
      if ( condition_Level < 1 ).
        block->set_Is_Ensured_By_Exit_Final( ).
      else.
        block->set_Is_Ensured_By_Exit( ).
      endif.
      case block->keyword_Type.
        when
          block=>c_Keyword_Type-loop_Table or
          block=>c_Keyword_Type-loop_Other.
          exit.
        when
          block=>c_Keyword_Type-condition_On_Table or
          block=>c_Keyword_Type-condition_On_Variable.
          add 1 to condition_Level.
      endcase.
    endloop.
  endmethod.


  method set_Entries_To_Nsrd_Upon_Rtrn.
  " return does not stop on loops
    data: block type ref to block.
    data: condition_Level type i.
    loop at me->block_Entries into block.
      if ( condition_Level < 1 ).
        block->set_Is_Ensured_By_Exit_Final( ).
      else.
        block->set_Is_Ensured_By_Exit( ).
      endif.
      case block->keyword_Type.
        when
          block=>c_Keyword_Type-condition_On_Table or
          block=>c_Keyword_Type-condition_On_Variable.
          add 1 to condition_Level.
      endcase.
    endloop.
  endmethod.

endclass.
