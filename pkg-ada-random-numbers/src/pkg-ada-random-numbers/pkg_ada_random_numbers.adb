-- File: pkg_ada_random_numbers.adb

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

-- ========================================================
package body pkg_ada_random_numbers
-- ======================================================== 
--   with SPARK_Mode => on  -- CANNOT BE TURNED ON WHY?
is
 
    package ATIO   renames Ada.Text_IO;
 
   -- =====================================================
   function get_random_integer (low_end, high_end : in Integer) return Integer is
   -- =====================================================
      
      type RandomIntRange is new Integer range low_end .. high_end;
      package RandomInt is new Ada.Numerics.Discrete_Random(RandomIntRange);
      use RandomInt;

      GenInt  : Generator;
      RandInt : RandomIntRange;
      
   begin
    
      Reset(GenInt);
      RandInt := Random(GenInt);
            
      return (Integer(RandInt));
   end get_random_integer;
   
   -- =====================================================   
   function get_random_float (low_end, high_end : in Float  ) return Float is
   -- =====================================================
   
      GenFloat  : Ada.Numerics.Float_Random.Generator;
      RndFloat  : Float;
      
      the_range      : Float;
      the_rand_float : Float;
      
   begin
   
      Ada.Numerics.Float_Random.Reset(GenFloat);
      -- NOTE: RndFloat is in the range 0.0 .. 1.0,
      RndFloat := Ada.Numerics.Float_Random.Random (GenFloat);
      
      the_range      := (high_end - low_end);
      the_rand_float := (RndFloat * the_range) + low_end;  
   
      return (the_rand_float);
    end get_random_float;
   
    -- ==================================================== 
    procedure generate_10_random_integers 	is
    -- ====================================================
      
    type RandomIntRange is new Integer range 1 .. 100;
    package RandomInt is new Ada.Numerics.Discrete_Random(RandomIntRange);
    use RandomInt;

    GenInt  : Generator;
    RandInt : RandomIntRange;
      
   begin
      
     for idx in 1 .. 10 loop
       Reset(GenInt);
       RandInt := Random(GenInt);
       ATIO.Put_line("Random integer (1 .. 100) = " & RandomIntRange'Image(RandInt));
     end loop;
      
    end generate_10_random_integers;
   
    -- ==================================================== 
    procedure generate_10_random_floats is
    -- ====================================================
      	   
    GenFloat  : Ada.Numerics.Float_Random.Generator;
    RndFloat  : Float;
      
    begin
      for idx in 1 .. 10 loop
         -- RESET Generator
         Ada.Numerics.Float_Random.Reset(GenFloat);
      
         -- NOTE: RndFloat is in the range 0.0 .. 1.0, ONLY
         RndFloat := Ada.Numerics.Float_Random.Random (GenFloat);
	   
         ATIO.Put_line("Random float (0.0 .. 1.0) = " & Float'Image(RndFloat));
      end loop;   
   
   end generate_10_random_floats;
   
   -- =====================================================
   procedure test_randomness_random_integers (low_end, high_end : in Integer) is 
   -- =====================================================
   -- Run one million random numbers, divide into 10 bins,
   -- Tabulate counts for each bin. Observe uniformity of counts.
   begin
      null;
   
   end test_randomness_random_integers;   
      
   -- =====================================================
   procedure test_randomness_random_floats (low_end, high_end : in Float) is  
   -- =====================================================
   -- Run one million random numbers, divide into 10 bins,
   -- Tabulate counts for each bin. Observe uniformity of counts.
   begin
      null;
   
   end test_randomness_random_floats; 
   
-- ========================================================
-- CLOSE THIS PACKAGE      
-- ========================================================
begin
    null;
-- ========================================================    
end pkg_ada_random_numbers; 
-- ========================================================
