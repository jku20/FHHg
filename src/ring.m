%----------------------------------------------------------------------------%
% A ring typeclass and some common instantiations.
%----------------------------------------------------------------------------%

:- module ring.
:- interface.

:- use_module uint32.
:- use_module int32.

%----------------------------------------------------------------------------%

% An mathematical ring.
:- typeclass ring(T) where [
  % Returns the sum of two elements. This should be commutative.
  func T + T = T,

  % Returns the product of two elements. This should be distributive over sum.
  func T * T = T,

  % Addative identity.
  func zero = T,

  % Multiplicative identity.
  func one = T
].

:- instance ring(uint32).
:- instance ring(int32).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- instance ring(uint32) where [
  func((+)/2) is uint32.(+),
  func((*)/2) is uint32.(*),
  zero = 0u32,
  one = 1u32
].

:- instance ring(int32) where [
  func((+)/2) is int32.(+),
  func((*)/2) is int32.(*),
  zero = 0i32,
  one = 1i32
].
