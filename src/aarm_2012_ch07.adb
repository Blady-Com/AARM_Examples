procedure AARM_2012_CH07 is
   -- 7.1 Package Specifications and Declarations
      package Rational_Numbers is

         type Rational is
              record
                 Numerator   : Integer;
                 Denominator : Positive;
              end record;

         function "="(X,Y : Rational) return Boolean;

         function "/"  (X,Y : Integer)  return Rational;  --  to construct a rational number

         function "+"  (X,Y : Rational) return Rational;
           function "-"  (X,Y : Rational) return Rational;
           function "*"  (X,Y : Rational) return Rational;
           function "/"  (X,Y : Rational) return Rational;
        end Rational_Numbers;


begin
null;
end;
