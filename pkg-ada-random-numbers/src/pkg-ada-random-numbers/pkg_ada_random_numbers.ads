
-- ========================================================
package pkg_ada_random_numbers
-- ======================================================== 
    with SPARK_Mode => on
is
   
         
   function get_random_integer (low_end, high_end : in Integer) return Integer; 
   function get_random_float   (low_end, high_end : in Float  ) return Float; 
   
   procedure generate_10_random_integers; 
   procedure generate_10_random_floats; 
   
   -- TO DO
   procedure test_randomness_random_integers (low_end, high_end : in Integer); 
   procedure test_randomness_random_floats   (low_end, high_end : in Float  );  
 
   -- TO DO CRAZY THING (Why not? Usage?)
   -- function get_random_integer_in_random_range (LRange, HRange : in Integer); 
   -- The algorithm
   -- (1) ret_num1 = get random integer in range  (LRange, HRange); 
   -- (2) ret_num2 = get random integer in range  (LRange, HRange);
   -- (3) if (ret_num1 < return_num2) then low_end = ret_num1 and high_end = ret_num2
   -- else reverse (swap)
   -- (4) Finally, result = get random integer in range (low_end, high_end)
   -- (5) Ha ha ha. Alhamdulillah 3 times. But why do this?
   -- (6) How about test uniformity of randomness here? Ha ha ha.
   
   
   
-- ======================================================== 
end pkg_ada_random_numbers;
-- ========================================================
