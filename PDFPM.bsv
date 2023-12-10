////////////////////////////////////////////////////////////////////////////////
//  Filename      : FloatingPoint.bsv
//  Description   : General purpose floating point library
////////////////////////////////////////////////////////////////////////////////
//package FloatingPoint;

// Notes :
// increasing exp = right shift sfd
// decreasing exp = left shift sfd

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Real              ::*;
import Vector            ::*;
import BUtils            ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;

////////////////////////////////////////////////////////////////////////////////
/// Exports
////////////////////////////////////////////////////////////////////////////////
export Disorder(..);
export Double;
export DoubleExtended;
export Exception(..);
export Float;
export FloatingPoint(..);
export Half;
export RoundMode(..);
export compareFP;
export convert;
export fract;
export infinity;
export isInfinity;
export isNaN;
export isNegativeZero;
export isOne;
export isQNaN;
export isSNaN;
export isSubNormal;
export isZero;
export mkFloatingPointMultiplier;
export nanQuiet;
export one;
export qnan;
export snan;
export zero;

////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////

typedef struct {
   Bool        sign;
   Bit#(e)     exp;
   Bit#(m)     sfd;
} FloatingPoint#(numeric type e, numeric type m) deriving (Bits);

instance DefaultValue#( FloatingPoint#(e,m) );
   defaultValue = FloatingPoint {
      sign:       False,
      exp:        0,
      sfd:        0
      };
endinstance

typedef enum {
   LT,
   EQ,
   GT,
   UO
   } Disorder deriving (Bits, Eq, Bounded);

////////////////////////////////////////////////////////////////////////////////
/// Eq
////////////////////////////////////////////////////////////////////////////////
instance Eq#(FloatingPoint#(e,m));
   function Bool \== ( FloatingPoint#(e,m) a, FloatingPoint#(e,m) b );
      let c = compareFP(a, b);
      return (c == EQ);
   endfunction

   function Bool \/= ( FloatingPoint#(e,m) a, FloatingPoint#(e,m) b );
      let c = compareFP(a, b);
      return (c != EQ);
   endfunction
endinstance

instance FShow#( FloatingPoint#(e,m) );
   function Fmt fshow( FloatingPoint#(e,m) value );
      return $format("<Float %s%x.%x>", value.sign ? "-" : "+", value.exp, value.sfd);
   endfunction
endinstance

instance Bounded#(FloatingPoint#(e,m));
   minBound = FloatingPoint {
      sign: True,
      exp: ('1 - 1),
      sfd: '1
      };
   maxBound = FloatingPoint {
      sign: False,
      exp: ('1 - 1),
      sfd: '1
      };
endinstance

typedef enum {
     Rnd_Nearest_Even
   , Rnd_Nearest_Away_Zero
   , Rnd_Plus_Inf
   , Rnd_Minus_Inf
   , Rnd_Zero
} RoundMode deriving (Bits, Eq);

instance DefaultValue#(RoundMode);
   defaultValue = Rnd_Nearest_Even;
endinstance

instance FShow#( RoundMode );
   function Fmt fshow( RoundMode value );
      case(value)
	 Rnd_Nearest_Even:      return $format("<Round Mode: Nearest Even>");
	 Rnd_Nearest_Away_Zero: return $format("<Round Mode: Nearest Away From Zero>");
	 Rnd_Plus_Inf:          return $format("<Round Mode: +Infinity>");
	 Rnd_Minus_Inf:         return $format("<Round Mode: -Infinity>");
	 Rnd_Zero:              return $format("<Round Mode: Zero>");
      endcase
   endfunction
endinstance

typedef struct {
   Bool invalid_op;
   Bool divide_0;
   Bool overflow;
   Bool underflow;
   Bool inexact;
} Exception deriving (Bits, Eq);

instance DefaultValue#(Exception);
   defaultValue = unpack(0);
endinstance

instance Bitwise#(Exception);
   function Exception \& (Exception x1, Exception x2);
      return unpack(pack(x1) & pack(x2));
   endfunction
   function Exception \| (Exception x1, Exception x2);
      return unpack(pack(x1) | pack(x2));
   endfunction
   function Exception \^ (Exception x1, Exception x2);
      return unpack(pack(x1) ^ pack(x2));
   endfunction
   function Exception \~^ (Exception x1, Exception x2);
      return unpack(pack(x1) ~^ pack(x2));
   endfunction
   function Exception \^~ (Exception x1, Exception x2);
      return unpack(pack(x1) ^~ pack(x2));
   endfunction
   function Exception invert (Exception x1);
      return unpack(~pack(x1));
   endfunction
   function Exception \<< (Exception x1, ix x2)
     provisos(PrimShiftIndex#(ix,dx));
      return error("Bitwise left shift not supported for type " + quote("Exception"));
   endfunction
   function Exception \>> (Exception x1, ix x2)
     provisos(PrimShiftIndex#(ix,dx));
      return error("Bitwise right shift not supported for type " + quote("Exception"));
   endfunction
   function Bit#(1) msb (Exception x);
      return error("Bitwise msb() not supported for type " + quote("Exception"));
   endfunction
   function Bit#(1) lsb (Exception x);
      return error("Bitwise lsb() not supported for type " + quote("Exception"));
   endfunction
endinstance

instance FShow#( Exception );
   function Fmt fshow( Exception value );
      Fmt f = $format("<Exception: ");
      if (value.invalid_op)
	 f = f + $format("Invalid_Op ");
      if (value.divide_0)
	 f = f + $format("Divide_0 ");
      if (value.overflow)
	 f = f + $format("Overflow ");
      if (value.underflow)
	 f = f + $format("Underflow ");
      if (value.inexact)
	 f = f + $format("Inexact ");
      f = f + $format(">");
      return f;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Float Formats
////////////////////////////////////////////////////////////////////////////////
typedef FloatingPoint#(5,10)  Half;
typedef FloatingPoint#(8,23)  Float;
typedef FloatingPoint#(11,32) SingleExtended;
typedef FloatingPoint#(11,52) Double;
typedef FloatingPoint#(15,64) DoubleExtended;

function Tuple2#(FloatingPoint#(e2,m2),Exception) convert(FloatingPoint#(e,m) in, RoundMode rmode, Bool preservenan)
   provisos(
      Max#(e, e2, emax),
      Max#(m, m2, mmax),
      Add#(emax, 1, expbits),
      Add#(mmax, 5, sfdbits),
      // per request of bsc
      Add#(a__, e, TAdd#(TMax#(e, e2), 1)),
      Add#(b__, e2, TAdd#(TMax#(e, e2), 1)),
      Add#(c__, TLog#(TAdd#(1, m)), expbits),
      Add#(d__, e2, expbits),
      Add#(m2, e__, sfdbits),
      Add#(f__, e, expbits),
      Add#(2, g__, sfdbits),
      Add#(1, TAdd#(1, TAdd#(m2, TAdd#(2, h__))), sfdbits),
      Add#(4, h__, e__),
      Add#(i__, TLog#(TAdd#(1, sfdbits)), TAdd#(e2, 1))
      );

   FloatingPoint#(e2,m2) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaN(in)) begin
      if (isSNaN(in) && !preservenan) begin
	 in = nanQuiet(in);
	 exc.invalid_op = True;
      end

      out.sign = in.sign;
      out.exp = '1;
      Bit#(sfdbits) sfd = zExtendLSB(in.sfd);
      out.sfd = truncateLSB(sfd);
      if (out.sfd == 0)
	 out.sfd = zExtendLSB(2'b01);
   end
   else if (isInfinity(in))
      out = infinity(in.sign);
   else if (isZero(in))
      out = zero(in.sign);
   else if (isSubNormal(in)) begin
      Int#(expbits) exp = fromInteger(minexp(in));
      Bit#(sfdbits) sfd = zExtendLSB({1'b0, getHiddenBit(in),in.sfd});
      Int#(expbits) subexp = unpack(pack(extend(countZerosMSB(in.sfd))));

      if ((exp - subexp) > fromInteger(maxexp(out))) begin
	 out.sign = in.sign;
	 out.exp = maxBound - 1;
	 out.sfd = maxBound;

	 exc.overflow = True;
	 exc.inexact = True;

	 let y = round(rmode, out, '1);
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else if ((exp - subexp) < fromInteger(minexp(out))) begin
	 out.sign = in.sign;
	 out.exp = 0;  // subnormal

	 let shift = fromInteger(abs(minexp(in)) - abs(minexp(out)));
	 if (shift < 0) begin
	    sfd = sfd << (-shift);
	 end
	 else if (shift < fromInteger(valueOf(sfdbits))) begin
	    Bit#(1) guard = |(sfd << (fromInteger(valueOf(sfdbits)) - shift));

	    sfd = sfd >> shift;
	    sfd[0] = sfd[0] | guard;
	 end
	 else if (|sfd == 1) begin
	    sfd = 1;
	 end

	 let x = normalize(out, sfd);
	 out = tpl_1(x);
	 exc = exc | tpl_3(x);

	 if (isZero(out)) begin
	    exc.underflow = True;
	 end

	 let y = round(rmode, out, tpl_2(x));
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else begin
	 out.sign = in.sign;
	 out.exp = pack(truncate(exp + fromInteger(bias(out))));

	 let x = normalize(out, sfd);
	 out = tpl_1(x);
	 exc = exc | tpl_3(x);

	 let y = round(rmode, out, tpl_2(x));
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
   end
   else begin
      Int#(expbits) exp = signExtend(unpack(unbias(in)));
      Bit#(sfdbits) sfd = zExtendLSB({1'b0, getHiddenBit(in),in.sfd});

      if (exp > fromInteger(maxexp(out))) begin
	 out.sign = in.sign;
	 out.exp = maxBound - 1;
	 out.sfd = maxBound;

	 exc.overflow = True;
	 exc.inexact = True;

	 let y = round(rmode, out, '1);
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else if (exp < fromInteger(minexp(out))) begin
	 out.sign = in.sign;
	 out.exp = 0;  // subnormal

	 let shift = (fromInteger(minexp(out)) - exp);
	 if (shift < fromInteger(valueOf(sfdbits))) begin
	    Bit#(1) guard = |(sfd << (fromInteger(valueOf(sfdbits)) - shift));

	    sfd = sfd >> shift;
	    sfd[0] = sfd[0] | guard;
	 end
	 else if (|sfd == 1) begin
	    sfd = 1;
	 end

	 let x = normalize(out, sfd);
	 out = tpl_1(x);
	 exc = exc | tpl_3(x);

	 if (isZero(out)) begin
	    exc.underflow = True;
	 end

	 let y = round(rmode, out, tpl_2(x));
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else begin
	 out.sign = in.sign;
	 out.exp = pack(truncate(exp + fromInteger(bias(out))));

	 let x = normalize(out, sfd);
	 out = tpl_1(x);
	 exc = exc | tpl_3(x);

	 let y = round(rmode, out, tpl_2(x));
	 out = tpl_1(y);
	 exc = exc | tpl_2(y);
      end
   end

   return tuple2(out, exc);
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Functions
////////////////////////////////////////////////////////////////////////////////
// Zero extend a quantity by padding on the LSB side.
function Bit#(m) zExtendLSB(Bit#(n) value)
   provisos( Add#(n,m,k) );
   Bit#(k) out = { value, 0 };
   return out[valueof(k)-1:valueof(n)];
endfunction

// Zero extend and change type by padding on the LSB side (of bits instance)
function a cExtendLSB(b value)
   provisos( Bits#(a,sa), Bits#(b,sb) );
   let out = unpack(zExtendLSB(pack(value)));
   return out;
endfunction

// Returns the 1-based index (or 0 if not found) of the first 1
// from the MSB down.
function Integer findIndexOneMSB_( Bit#(s) din );
   Vector#(s, Bit#(1)) v = unpack(din);
   Integer result = 0;
   for(Integer i = 0; i < valueof(s); i = i + 1) begin
      if (v[i] == 1) result = i + 1;
   end
   return result;
endfunction

function UInt#(l) findIndexOneMSB( Bit#(s) din )
   provisos( Add#(_, 1, s), Log#(s, logs), Add#(logs,1,l));
   Vector#(s, Bit#(1)) v = unpack(reverseBits(din));
   if (findElem(1'b1, v) matches tagged Valid .ridx) begin
      return fromInteger(valueOf(s)) - cExtend(ridx);
   end
   else begin
      return 0;
   end
endfunction

// Returns the 1-based index (or 0 if not found) of the first 1
// from the LSB up.
function Integer findIndexOneLSB_( Bit#(s) din );
   Vector#(s, Bit#(1)) v = unpack(din);
   Integer result = 0;
   Bool done = False;
   for(Integer i = 0; i < valueof(s); i = i + 1) begin
      if (v[i] == 1)  done = True;
      else if (!done) result = i + 1;
   end
   return (done) ? result : 0;
endfunction

function UInt#(l) findIndexOneLSB( Bit#(s) din )
   provisos( Add#(_, 1, s), Log#(s, logs), Add#(logs,1,l));
   Vector#(s, Bit#(1)) v = unpack(din);
   if (findElem(1'b1, v) matches tagged Valid .ridx) begin
      return cExtend(ridx);
   end
   else begin
      return 0;
   end
endfunction


function FloatingPoint#(e,m) infinity(Bool sign);
   return FloatingPoint {
      sign:     sign,
      exp:      maxBound,
      sfd:      0
      };
endfunction

function Bool isInfinity( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (din.sfd == 0));
endfunction

function FloatingPoint#(e,m) qnan();
   return FloatingPoint {
      sign:     False,
      exp:      '1,
      sfd:      reverseBits('b1)
      };
endfunction

function Bool isQNaN( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (msb(din.sfd) == 1));
endfunction

function FloatingPoint#(e,m) snan();
   return FloatingPoint {
      sign:     False,
      exp:      '1,
      sfd:      reverseBits('b10)
      };
endfunction

function Bool isSNaN( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (|din.sfd == 1) && (msb(din.sfd) == 0));
endfunction

function FloatingPoint#(e,m) nanQuiet(FloatingPoint#(e,m) din);
   din.sfd = din.sfd | reverseBits('b1);
   return din;
endfunction

function FloatingPoint#(e,m) zero(Bool sign);
   return FloatingPoint {
      sign:     sign,
      exp:      0,
      sfd:      0
      };
endfunction

function FloatingPoint#(e,m) one(Bool sign);
   FloatingPoint#(e,m) dummy = ?;
   return FloatingPoint {
      sign:     sign,
      exp:      fromInteger(bias(dummy)),
      sfd:      0
      };
endfunction

function Bool isZero( FloatingPoint#(e,m) din );
   return isSubNormal(din) && (din.sfd == 0);
endfunction

function Bool isOne( FloatingPoint#(e,m) din );
   return (din.sfd == 0) && (din.exp == fromInteger(bias(din)));
endfunction

function Bool isNegativeZero( FloatingPoint#(e,m) din );
   return isZero(din) && (din.sign);
endfunction

function Bool isNaN( FloatingPoint#(e,m) din );
   return isNaNOrInfinity(din) && !isInfinity(din);
endfunction

function Bool isNaNOrInfinity( FloatingPoint#(e,m) din );
   return (din.exp == '1);
endfunction

function Bool isSubNormal( FloatingPoint#(e,m) din );
   return (din.exp == 0);
endfunction

function Integer bias( FloatingPoint#(e,m) din );
   return (2 ** (valueof(e)-1)) - 1;
endfunction

function Bit#(e) unbias( FloatingPoint#(e,m) din );
   return (din.exp - fromInteger(bias(din)));
endfunction

function Bit#(1) getHiddenBit( FloatingPoint#(e,m) din );
   return (isSubNormal(din)) ? 0 : 1;
endfunction

function Integer minexp( FloatingPoint#(e,m) din );
  return 1-bias(din);
endfunction

function Integer minexp_subnormal( FloatingPoint#(e,m) din );
   return minexp(din)-valueof(m);
endfunction

function Integer maxexp( FloatingPoint#(e,m) din );
   return bias(din);
endfunction

function FloatingPoint#(e,m) rightshift( FloatingPoint#(e,m) din, Bit#(e) amt )
   provisos(  Add#(m, 4, m4)
	    , Add#(m4, m, x)
	    );
   Bit#(x) sfd = cExtendLSB({ getHiddenBit(din), din.sfd });
   Bit#(1) hidden;
   Bit#(m) s;
   Bit#(m) rest;
   { hidden, s, rest } = cExtendLSB(sfd >> amt);
   din.sfd    = s;
   return din;
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) round( RoundMode rmode, FloatingPoint#(e,m) din, Bit#(2) guard )
   provisos(  Add#(m, 1, m1)
	    , Add#(m, 2, m2)
	    );

   FloatingPoint#(e,m) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaNOrInfinity(din)) begin
      out = din;
   end
   else begin
      let din_inc = din;

      Bit#(TAdd#(m,2)) sfd = unpack({1'b0, getHiddenBit(din), din.sfd}) + 1;

      if (msb(sfd) == 1) begin
	 if (din.exp == fromInteger(maxexp(din) + bias(out))) begin
	    din_inc = infinity(din_inc.sign);
	 end
	 else begin
	    din_inc.exp = din_inc.exp + 1;
	    din_inc.sfd = truncate(sfd >> 1);
	 end
      end
      else if ((din.exp == 0) && (truncateLSB(sfd) == 2'b01)) begin
	 din_inc.exp = 1;
	 din_inc.sfd = truncate(sfd);
      end
      else begin
	 din_inc.sfd = truncate(sfd);
      end

      if (guard != 0) begin
	 exc.inexact = True;
      end

      case(rmode)
	 Rnd_Nearest_Even:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din;
	       'b10: out = (lsb(din.sfd) == 1) ? din_inc : din;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Nearest_Away_Zero:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din_inc;
	       'b10: out = din_inc;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Plus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din;
	    else
	       out = din_inc;
	 end

	 Rnd_Minus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din_inc;
	    else
	       out = din;
	 end

	 Rnd_Zero:
	 begin
	    out = din;
	 end
      endcase
   end

   if (isInfinity(out)) begin
      exc.overflow = True;
   end

   return tuple2(out,exc);
endfunction

function Tuple3#(FloatingPoint#(e,m),Bit#(2),Exception) normalize( FloatingPoint#(e,m) din, Bit#(x) sfdin )
   provisos(
      Add#(1, a__, x),
      Add#(m, b__, x),
      // per request of bsc
      Add#(c__, TLog#(TAdd#(1, x)), TAdd#(e, 1))
      );

   FloatingPoint#(e,m) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;

   Int#(TAdd#(e,1)) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   let zeros = countZerosMSB(sfdin);

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
	 // carry, no sfd adjust necessary

	 if (out.exp == 0)
	    out.exp = 2;
	 else
	    out.exp = out.exp + 1;

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
	 // already normalized

	 if (out.exp == 0)
	    out.exp = 1;

	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(x))) begin
	 // exactly zero
	 out.exp = 0;
      end
      else begin
	 // try to normalize
	 Int#(TAdd#(e,1)) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(TAdd#(e,1)) maxshift = exp - fromInteger(minexp(out));

	 if (shift > maxshift) begin
	    // result will be subnormal

	    sfdin = sfdin << maxshift;
	    out.exp = 0;
	 end
	 else begin
	    // result will be normal

	    sfdin = sfdin << shift;
	    out.exp = out.exp - truncate(pack(shift));
	 end

 	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end

      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(valueOf(m));

      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;

      guard[0] = |sfdin;
   end

   if ((out.exp == 0) && (guard != 0))
      exc.underflow = True;

   return tuple3(out,guard,exc);
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) fract(FloatingPoint#(e,m) din)
   provisos(
      // per request of bsc
      Add#(a__, TLog#(TAdd#(1, TAdd#(1, TAdd#(TAdd#(m, 1), 1)))), TAdd#(e, 1))
      );
   FloatingPoint#(e,m) res = din;
   Exception exc = defaultValue;

   // this routine extracts the fractional portion of a floating point number, i.e.
   //  123.456 would return 0.456.

   // if the value is not a number, provide not a number result
   if (isNaN(din))
      res = din;
   else if (isInfinity(din)) // if the number is infinity, signal NaN
      res = qnan();
   else if (din.exp < fromInteger(bias(din))) // 0 <= quantity < 1
      res = din;
   else begin // all other cases
      Bit#(TAdd#(m,1)) m = { 1'b1, din.sfd };
      m = m << (din.exp - fromInteger(bias(din)) + 1);
      res.exp = fromInteger(bias(din)) - 1;
      let x = normalize(res, { 1'b0, m, 1'b0 });
      res = tpl_1(x);
      exc = tpl_3(x);
      let y = round(defaultValue, res, tpl_2(x));
      res = tpl_1(y);
      exc = tpl_2(y);
   end
   return tuple2(res,exc);
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Real type conversion
////////////////////////////////////////////////////////////////////////////////
instance RealLiteral#( FloatingPoint#(e,m) );
   function FloatingPoint#(e,m) fromReal( Real n );
      FloatingPoint#(e,m) out = defaultValue;
      Bit#(m) sfdm = 0; Bit#(2) rnd = 0; Bit#(53) rest = 0;

      let {s,ma,ex} = decodeReal(n);

      Bit#(53) sfd = s ? fromInteger(ma) : fromInteger(-ma);
      let msbindex = findIndexOneMSB_(sfd);
      let exp      = ex + msbindex - 1;

      if (msbindex == 0) begin
	 out.sign   = !s;
	 out.exp    = 0;
	 out.sfd    = 0;
      end
      else if (exp > maxexp(out)) begin
      	 out = error("Specified Real '" + realToString(n) + "' caused overflow and cannot be represented by the given type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")'.", out);
      end
      else if (exp < minexp_subnormal(out)) begin
      	 out = error("Specified Real '" + realToString(n) + "' caused underflow and cannot be represented by the given type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")'.", out);
      end
      else if (exp < minexp(out)) begin
	 out.sign       = !s;
	 out.exp        = 0;
	 { sfdm, rnd, rest } = unpack(cExtendLSB(sfd) >> (minexp(out) - exp - 1));
	 out.sfd        = sfdm;
      end
      else begin
      	 out.sign       = !s;
      	 Bit#(e) x      = fromInteger(exp) + fromInteger(bias(out));
      	 out.exp        = unpack(x);
	 { sfdm, rnd, rest } = unpack(cExtendLSB(sfd) << (53 - (msbindex - 1)));
	 out.sfd        = sfdm;
      end

      return out;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Literal
////////////////////////////////////////////////////////////////////////////////
instance Literal#( FloatingPoint#(e,m) );
   function FloatingPoint#(e,m) fromInteger( Integer n );
      FloatingPoint#(e,m) out = defaultValue;
      Bool issue_warning = False;
      String warning_msg = "";

      let maxsfd = 2 ** (valueof(m)+1);

      out.sign = n < 0;
      Integer x = (out.sign) ? -n : n;
      Integer exp = 0;

      if (n != 0) begin
	 // determine the initial exponent
	 while(mod(x,2) == 0 ) begin
	    exp = exp + 1;
	    x   = x / 2;
	 end

	// Check if the number of bits to represent exceeds a certain threshold.
	// If so, truncate to maintain manageability and avoid overflow.
	// Display a warning about potential loss of precision due to truncation.

	 if (x > maxsfd) begin
	    while(x > maxsfd) begin
	       exp = exp + 1;
	       x   = x / 2;
	    end

	    Integer s = x * (2 ** exp);
	    s = (out.sign) ? -s : s;

	    warning_msg = "Converting from Literal '" + integerToString(n) + "' to type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")' exceeds the precision offered.  Replacing with " + integerToString(s) + ".";
	    issue_warning = True;
	 end

	 // move the significand into a field with hidden bit explicit.
	 Bit#(TAdd#(m,1)) sx = fromInteger(x);

	 // If the hidden bit is indeed set, we are done.  Convert to float.
	 if (msb(sx) == 1) begin
	    out.exp  = fromInteger(exp + bias(out) + valueof(m));
	    out.sfd  = cExtend(sx);
	 end
	 else begin
	    Bit#(m) mval = cExtend(sx);
	    let msbindex = findIndexOneMSB_(mval);
	    out.exp      = fromInteger(exp + bias(out) + msbindex - 1);
	    out.sfd      = mval << (valueof(m) - (msbindex - 1));
	 end
      end

      return (issue_warning) ? warning(warning_msg,out) : out;
   endfunction

   function Bool inLiteralRange(FloatingPoint#(e,m) a, Integer n);
      return False;
   endfunction
endinstance


////////////////////////////////////////////////////////////////////////////////
/// Ord (sort of)
/// values are not totally ordered so some comparisons return unordered
////////////////////////////////////////////////////////////////////////////////
function Disorder compareFP( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
   if (isNaN(x) || isNaN(y))
      return UO;
   else if (isZero(x) && isZero(y))
      return EQ;
   else begin
      let expLT  = x.exp < y.exp;
      let expEQ  = x.exp == y.exp;
      let expGT  = x.exp > y.exp;
      let sfdLT  = x.sfd < y.sfd;
      let sfdGT  = x.sfd > y.sfd;
      let sfdEQ  = x.sfd == y.sfd;

      if (x.sign && !y.sign) begin
	 return LT;
      end
      else if (!x.sign && y.sign) begin
	 return GT;
      end
      else if (!x.sign) begin
	 // both positive
	 if (expLT || (expEQ && sfdLT)) begin
	    return LT;
	 end
	 else if (expGT || (expEQ && sfdGT)) begin
	    return GT;
	 end
	 else begin
	    return EQ;
	 end
      end
      else begin
	 // both negative
	 if (expGT || (expEQ && sfdGT)) begin
	    return LT;
	 end
	 else if (expLT || (expEQ && sfdLT)) begin
	    return GT;
	 end
	 else begin
	    return EQ;
	 end
      end
   end
endfunction


instance Ord#(FloatingPoint#(e,m));
   function Bool \< ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == LT);
   endfunction

   function Bool \<= ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == LT) || (c == EQ);
   endfunction

   function Bool \> ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == GT);
   endfunction

   function Bool \>= ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == GT) || (c == EQ);
   endfunction

   function Ordering compare( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
      let c = compareFP(x, y);
      case (c)
	 UO: return error("Unordered comparison of type " + quote("FloatingPoint") + ".");
	 EQ: return EQ;
	 LT: return LT;
	 GT: return GT;
      endcase
   endfunction

   function FloatingPoint#(e,m) min( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
      if (isSNaN(x)) return nanQuiet(x);
      else if (isSNaN(y)) return nanQuiet(y);
      else if (isQNaN(x)) return x;
      else if (isQNaN(y)) return y;
      else begin
	 let signLT = (x.sign && !y.sign);
	 let signEQ = x.sign == y.sign;
	 let expLT  = x.exp < y.exp;
	 let expEQ  = x.exp == y.exp;
	 let manLT  = x.sfd < y.sfd;

	 if (signLT || (signEQ && expLT) || (signEQ && expEQ && manLT)) return x;
	 else return y;
      end
   endfunction

   function FloatingPoint#(e,m) max( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
      if (isSNaN(x)) return nanQuiet(x);
      else if (isSNaN(y)) return nanQuiet(y);
      else if (isQNaN(x)) return x;
      else if (isQNaN(y)) return y;
      else begin
	 let signEQ = x.sign == y.sign;
	 let signGT = (!x.sign && y.sign);
	 let expEQ  = x.exp == y.exp;
	 let expGT  = x.exp > y.exp;
	 let manGT  = x.sfd > y.sfd;

	 if (signGT || (signEQ && expGT) || (signEQ && expEQ && manGT)) return x;
	 else return y;
      end
   endfunction
endinstance



typedef struct {
   Maybe#(FloatingPoint#(e,m)) res;
   Exception exc;
   RoundMode rmode;
   } CommonState#(numeric type e, numeric type m) deriving (Bits, Eq);



////////////////////////////////////////////////////////////////////////////////
/// Pipelined Floating Point Multiplier
////////////////////////////////////////////////////////////////////////////////
module mkFloatingPointMultiplier(Server#(Tuple3#(FloatingPoint#(e,m), FloatingPoint#(e,m), RoundMode), Tuple2#(FloatingPoint#(e,m),Exception)))
   provisos(
      // per request of bsc
      Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1))
      );

   
   // S0
  
   FIFO#(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode))                 fOperands_S0        <- mkLFIFO;

   /// S1 - calculate the new exponent/sign

   FIFO#(Tuple5#(CommonState#(e,m),
		 Bit#(TAdd#(m,1)),
		 Bit#(TAdd#(m,1)),
		 Int#(TAdd#(e,2)),
		 Bool)) fState_S1 <- mkLFIFO;

   rule s1_stage;
      match { .opA, .opB, .rmode } <- toGet(fOperands_S0).get;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Int#(TAdd#(e,2)) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(TAdd#(e,2)) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));
      Int#(TAdd#(e,2)) newexp = expA + expB;

      Bool sign = (opA.sign != opB.sign);

      Bit#(TAdd#(m,1)) opAsfd = { getHiddenBit(opA), opA.sfd };
      Bit#(TAdd#(m,1)) opBsfd = { getHiddenBit(opB), opB.sfd };

      if (isSNaN(opA)) begin
	 s.res = tagged Valid nanQuiet(opA);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opB)) begin
	 s.res = tagged Valid nanQuiet(opB);
	 s.exc.invalid_op = True;
      end
      else if (isQNaN(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isQNaN(opB)) begin
	 s.res = tagged Valid opB;
      end
      else if ((isInfinity(opA) && isZero(opB)) || (isZero(opA) && isInfinity(opB))) begin
	 s.res = tagged Valid qnan();
	 s.exc.invalid_op = True;
      end
      else if (isInfinity(opA) || isInfinity(opB)) begin
	 s.res = tagged Valid infinity(opA.sign != opB.sign);
      end
      else if (isZero(opA) || isZero(opB)) begin
	 s.res = tagged Valid zero(opA.sign != opB.sign);
      end
      else if (newexp > fromInteger(maxexp(opA))) begin
	 FloatingPoint#(e,m) out;
	 out.sign = (opA.sign != opB.sign);
	 out.exp = maxBound - 1;
	 out.sfd = maxBound;

	 s.exc.overflow = True;
	 s.exc.inexact = True;

	 let y = round(rmode, out, '1);
	 s.res = tagged Valid tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end
      else if (newexp < (fromInteger(minexp_subnormal(opA))-2)) begin
	 FloatingPoint#(e,m) out;
	 out.sign = (opA.sign != opB.sign);
	 out.exp = 0;
	 out.sfd = 0;

	 s.exc.underflow = True;
	 s.exc.inexact = True;

	 let y = round(rmode, out, 'b01);
	 s.res = tagged Valid tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      fState_S1.enq(tuple5(s,
			   opAsfd,
			   opBsfd,
			   newexp,
			   sign));
   endrule

   
   /// S2
   
   FIFO#(Tuple4#(CommonState#(e,m),
		 Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
		 Int#(TAdd#(e,2)),
		 Bool)) fState_S2 <- mkLFIFO;

   rule s2_stage;
      match {.s, .opAsfd, .opBsfd, .exp, .sign} <- toGet(fState_S1).get;

      Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))) sfdres = primMul(opAsfd, opBsfd);

      fState_S2.enq(tuple4(s,
			   sfdres,
			   exp,
			   sign));
   endrule

   /// S3
   FIFO#(Tuple4#(CommonState#(e,m),
		 Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
		 Int#(TAdd#(e,2)),
		 Bool)) fState_S3 <- mkLFIFO;

   rule s3_stage;
      let x <- toGet(fState_S2).get;
      fState_S3.enq(x);
   endrule

   
   /// S4
   
   FIFO#(Tuple3#(CommonState#(e,m),
		 FloatingPoint#(e,m),
		 Bit#(2))) fState_S4 <- mkLFIFO;

   rule s4_stage;
      match {.s, .sfdres, .exp, .sign} <- toGet(fState_S3).get;

      FloatingPoint#(e,m) result = defaultValue;
      Bit#(2) guard = ?;

      if (s.res matches tagged Invalid) begin
	 //$display("sfdres = 'h%x", sfdres);

	 let shift = fromInteger(minexp(result)) - exp;
	 if (shift > 0) begin
	    // subnormal
	    Bit#(1) sfdlsb = |(sfdres << (fromInteger(valueOf(TAdd#(TAdd#(m,1),TAdd#(m,1)))) - shift));

	    //$display("sfdlsb = |'h%x = 'b%b", (sfdres << (fromInteger(valueOf(TAdd#(TAdd#(m,1),TAdd#(m,1)))) - shift)), sfdlsb);

            sfdres = sfdres >> shift;
            sfdres[0] = sfdres[0] | sfdlsb;

	    result.exp = 0;
	 end
	 else begin
	    result.exp = cExtend(exp + fromInteger(bias(result)));
	 end

	 // $display("shift = %d", shift);
	 // $display("sfdres = 'h%x", sfdres);
	 // $display("result = ", fshow(result));
	 // $display("exc = 'b%b", pack(exc));
	 // $display("zeros = %d", countZerosMSB(sfdres));

	 result.sign = sign;
	 let y = normalize(result, sfdres);
	 result = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);

	 // $display("result = ", fshow(result));
	 // $display("exc = 'b%b", pack(exc));
      end

      fState_S4.enq(tuple3(s,
			   result,
			   guard));
   endrule

   
   /// S5
   
   FIFO#(Tuple2#(FloatingPoint#(e,m),Exception)) fResult_S5          <- mkLFIFO;

   rule s5_stage;
      match {.s, .rnd, .guard} <- toGet(fState_S4).get;

      FloatingPoint#(e,m) out = rnd;

      if (s.res matches tagged Valid .x)
	 out = x;
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      fResult_S5.enq(tuple2(out,s.exc));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Interface Connections / Methods
   ////////////////////////////////////////////////////////////////////////////////
   interface request = toPut(fOperands_S0);
   interface response = toGet(fResult_S5);

endmodule: mkFloatingPointMultiplier
