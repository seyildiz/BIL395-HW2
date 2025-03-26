with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Float_Text_IO;        use Ada.Float_Text_IO;

procedure Calculator is

   -- Token kinds for numbers, identifiers, operators, etc.
   type Token_Kind is (
      tk_Number, tk_Plus, tk_Minus, tk_Times, tk_Divide,
      tk_LParen, tk_RParen, tk_Identifier, tk_Assign,
      tk_End, tk_Invalid
   );

   type Token is record
      Kind    : Token_Kind;
      Value   : Float := 0.0;
      Id_Text : String(1..32) := (others => ' ');
   end record;

   -- Global parsing state
   Input_Line    : String (1 .. 256);
   Len           : Natural;
   Pos           : Natural := 1;
   Current_Token : Token;

   -- Skip whitespace characters
   procedure Skip_Whitespace is
   begin
      while (Pos <= Len) and then
            (Input_Line(Pos) = ' ' or else
             Input_Line(Pos) = ASCII.LF or else
             Input_Line(Pos) = ASCII.CR) loop
         Pos := Pos + 1;
      end loop;
   end Skip_Whitespace;

   -- Tokenizer: returns the next token from the input string.
   function Get_Token return Token is
      Tok      : Token;
      Start_Pos: Natural;
      Id_Len   : Natural;
   begin
      Skip_Whitespace;
      if Pos > Len then
         Tok.Kind := tk_End;
         return Tok;
      end if;
      case Input_Line(Pos) is
         when '+' =>
            Tok.Kind := tk_Plus;
            Pos := Pos + 1;
         when '-' =>
            Tok.Kind := tk_Minus;
            Pos := Pos + 1;
         when '*' =>
            Tok.Kind := tk_Times;
            Pos := Pos + 1;
         when '/' =>
            Tok.Kind := tk_Divide;
            Pos := Pos + 1;
         when '(' =>
            Tok.Kind := tk_LParen;
            Pos := Pos + 1;
         when ')' =>
            Tok.Kind := tk_RParen;
            Pos := Pos + 1;
         when '=' =>
            Tok.Kind := tk_Assign;
            Pos := Pos + 1;
         when others =>
            if Input_Line(Pos) in '0'..'9' then
               Start_Pos := Pos;
               while (Pos <= Len) and then
                     ((Input_Line(Pos) in '0'..'9') or else (Input_Line(Pos) = '.')) loop
                  Pos := Pos + 1;
               end loop;
               Tok.Value := Float'Value(Input_Line(Start_Pos .. Pos - 1));
               Tok.Kind := tk_Number;
            elsif Input_Line(Pos) in 'A'..'Z' or else Input_Line(Pos) in 'a'..'z' then
               Start_Pos := Pos;
               while (Pos <= Len) and then
                     ((Input_Line(Pos) in 'A'..'Z') or else
                      (Input_Line(Pos) in 'a'..'z') or else
                      (Input_Line(Pos) in '0'..'9')) loop
                  Pos := Pos + 1;
               end loop;
               Tok.Kind := tk_Identifier;
               Id_Len := Pos - Start_Pos;
               if Id_Len > 32 then
                  Id_Len := 32;
               end if;
               Tok.Id_Text := (others => ' ');
               Tok.Id_Text(1 .. Id_Len) := Input_Line(Start_Pos .. Start_Pos + Id_Len - 1);
            else
               Tok.Kind := tk_Invalid;
               Pos := Pos + 1;
            end if;
      end case;
      return Tok;
   end Get_Token;

   -- Advance to the next token.
   procedure Advance_Token is
   begin
      Current_Token := Get_Token;
   end Advance_Token;

   ----------------------------------------------------------------
   -- Variable Storage (fixed-size array acting as a dictionary)
   ----------------------------------------------------------------
   type Var_Record is record
      Name  : String(1..32);
      Value : Float;
   end record;

   type Var_Array is array (Positive range <>) of Var_Record;
   Variables : Var_Array(1 .. 100);
   Var_Count : Natural := 0;

   function Get_Variable (Name : in String) return Float is
      I : Natural;
   begin
      for I in 1 .. Var_Count loop
         if Variables(I).Name(1 .. Name'Length) = Name then
            return Variables(I).Value;
         end if;
      end loop;
      Put_Line("Error: Undefined variable " & Name);
      return 0.0;
   end Get_Variable;

   procedure Set_Variable (Name : in String; Val : Float) is
      I     : Natural;
      Found : Boolean := False;
   begin
      for I in 1 .. Var_Count loop
         if Variables(I).Name(1 .. Name'Length) = Name then
            Variables(I).Value := Val;
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         if Var_Count < Variables'Length then
            Var_Count := Var_Count + 1;
            Variables(Var_Count).Name := (others => ' ');
            Variables(Var_Count).Name(1 .. Name'Length) := Name;
            Variables(Var_Count).Value := Val;
         else
            Put_Line("Error: Variable storage full");
         end if;
      end if;
   end Set_Variable;

   ----------------------------------------------------------------
   -- Recursive Descent Parser Functions
   -- Grammar:
   --   Expression  ::= Term { ("+" | "-") Term }
   --   Term        ::= Factor { ("*" | "/") Factor }
   --   Factor      ::= Number | Identifier | "(" Expression ")" | "-" Factor
   ----------------------------------------------------------------
   function Parse_Expression return Float;
   function Parse_Term return Float;
   function Parse_Factor return Float;

   function Parse_Factor return Float is
      Result : Float;
   begin
      if Current_Token.Kind = tk_Number then
         Result := Current_Token.Value;
         Advance_Token;
         return Result;
      elsif Current_Token.Kind = tk_Identifier then
         Result := Get_Variable(Current_Token.Id_Text);
         Advance_Token;
         return Result;
      elsif Current_Token.Kind = tk_LParen then
         Advance_Token;
         Result := Parse_Expression;
         if Current_Token.Kind /= tk_RParen then
            Put_Line("Error: Missing closing parenthesis");
         else
            Advance_Token;
         end if;
         return Result;
      elsif Current_Token.Kind = tk_Minus then
         Advance_Token;
         return -Parse_Factor;
      else
         Put_Line("Error: Unexpected token in expression");
         Advance_Token;
         return 0.0;
      end if;
   end Parse_Factor;

   function Parse_Term return Float is
      Result  : Float := Parse_Factor;
      Divisor : Float;
   begin
      while (Current_Token.Kind = tk_Times) or else (Current_Token.Kind = tk_Divide) loop
         if Current_Token.Kind = tk_Times then
            Advance_Token;
            Result := Result * Parse_Factor;
         elsif Current_Token.Kind = tk_Divide then
            Advance_Token;
            Divisor := Parse_Factor;
            if Divisor = 0.0 then
               Put_Line("Error: Division by zero");
               return 0.0;
            else
               Result := Result / Divisor;
            end if;
         end if;
      end loop;
      return Result;
   end Parse_Term;

   function Parse_Expression return Float is
      Result : Float := Parse_Term;
   begin
      while (Current_Token.Kind = tk_Plus) or else (Current_Token.Kind = tk_Minus) loop
         if Current_Token.Kind = tk_Plus then
            Advance_Token;
            Result := Result + Parse_Term;
         elsif Current_Token.Kind = tk_Minus then
            Advance_Token;
            Result := Result - Parse_Term;
         end if;
      end loop;
      return Result;
   end Parse_Expression;

   -- Temporary variables for the interactive loop.
   Temp       : Token;
   Expr_Value : Float;

begin
   Put_Line("Simple Calculator in ADA. Enter an expression or assignment (e.g., X = 3+4).");
   Put_Line("Type an empty line to exit.");
   loop
      Put("> ");
      Get_Line(Input_Line, Len);
      if Len = 0 then
         exit;
      end if;
      Pos := 1;
      Advance_Token;
      if Current_Token.Kind = tk_Identifier then
         Temp := Current_Token;
         Advance_Token;
         if Current_Token.Kind = tk_Assign then
            Advance_Token;
            Expr_Value := Parse_Expression;
            Set_Variable(Temp.Id_Text, Expr_Value);
            Put(Temp.Id_Text & " = ");
            Ada.Float_Text_IO.Put(Item => Expr_Value, Fore => 0, Aft => 5, Exp => 0);
            New_Line;
         else
            -- Not an assignment; treat as an expression.
            Pos := 1;
            Advance_Token;
            Expr_Value := Parse_Expression;
            Put("Result: ");
            Ada.Float_Text_IO.Put(Item => Expr_Value, Fore => 0, Aft => 5, Exp => 0);
            New_Line;
         end if;
      else
         Expr_Value := Parse_Expression;
         Put("Result: ");
         Ada.Float_Text_IO.Put(Item => Expr_Value, Fore => 0, Aft => 5, Exp => 0);
         New_Line;
      end if;
   end loop;
   Put_Line("Goodbye.");
end Calculator;
