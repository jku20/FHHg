%----------------------------------------------------------------------------%
% An implementation of a NAND gate in the TFHE scheme.
%----------------------------------------------------------------------------%

:- module tfhe.
:- interface.

:- import_module int32.
:- import_module random.

%----------------------------------------------------------------------------%

:- type key.
:- type plaintext ---> plaintext(int32).
:- type ciphertext.

:- pred mk_key(key::out, R::in, R::out) is det <= random(R).
:- pred encrypt(plaintext::in, key::in, ciphertext::out, R::in, R::out) is det
<= random(R).
:- pred decrypt(ciphertext::in, key::in, plaintext::out) is det.

:- func nand(ciphertext, ciphertext) = ciphertext.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module vector.

%----------------------------------------------------------------------------%

:- type key_size ---> size.
:- instance dimension(key_size) where [
  dim(size) = 10u64
].
:- type ciphertext ---> ciphertext(vector(int32, key_size), int32).
:- type key ---> key(vector(int32, key_size)).

:- func deviation = float.
:- pred generate_noise(int32::out, R::in, R::out) is det <= random(R).

%----------------------------------------------------------------------------%

deviation = 1e-20.

generate_noise(O, !R) :-
  normal_floats(0.0, deviation, F, _, !R),
  % Max int as a float.
  O = int32.det_from_int(round_to_int(F * 4294967295.0)).

mk_key(K, !R) :- 
  mk_random_vec(size, K0, !R),
  K = key(K0).

encrypt(plaintext(P), key(K), C, !R) :- 
  mk_random_vec(size, A, !R),
  generate_noise(E, !R),
  C = ciphertext(A, K * A + P + E).

decrypt(C, K, P) :-
  P = plaintext(0i32).

nand(C1, C2) = ciphertext(V, 0i32) :- 
  mk_zero_vec(size, V).
