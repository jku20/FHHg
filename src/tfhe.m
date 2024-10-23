%----------------------------------------------------------------------------%
% An implementation of a NAND gate in the TFHE scheme.
%----------------------------------------------------------------------------%

:- module tfhe.
:- interface.

:- import_module random.

%----------------------------------------------------------------------------%

:- type key.
:- type plaintext ---> plaintext(int8).
:- type ciphertext.

% Encryption/Decryption.
:- pred mk_key(key::out, R::in, R::out) is det <= random(R).
:- pred encrypt(plaintext::in, key::in, ciphertext::out, R::in, R::out) is det
<= random(R).
:- pred decrypt(ciphertext::in, key::in, plaintext::out) is det.

% Common Operations.
:- func ciphertext + ciphertext = ciphertext.
:- func int32 * ciphertext = ciphertext.
:- func nand(ciphertext, ciphertext) = ciphertext.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module vector.
:- import_module int32.

%----------------------------------------------------------------------------%

:- type key_size ---> size.
:- instance dimension(key_size) where [
  dim(size) = 10u64
].
:- type ciphertext ---> ciphertext(vector(int32, key_size), int32).
:- type key ---> key(vector(int32, key_size)).

:- func deviation = float.
:- pred generate_noise(int32::out, R::in, R::out) is det <= random(R).

:- func encryption_offset = int.

%----------------------------------------------------------------------------%

% Encryption/Decryption.

deviation = 1e-24.
encryption_offset = 24.

generate_noise(O, !R) :-
  normal_floats(0.0, deviation, F, _, !R),
  % Max int as a float.
  O = int32.det_from_int(round_to_int(F * 4294967295.0)).

mk_key(K, !R) :- 
  mk_random_vec(size, K0, !R),
  K = key(K0).

encrypt(plaintext(P0), key(K), C, !R) :-
  P = cast_from_int8(P0) << encryption_offset,
  mk_random_vec(size, A, !R),
  generate_noise(E, !R),
  C = ciphertext(A, K * A + P + E).

decrypt(ciphertext(A, B), key(K), P) :-
  P = plaintext(cast_to_int8((B - K * A) >> encryption_offset)).

% Common Operations.

ciphertext(A1, B1) + ciphertext(A2, B2) = ciphertext(A1 + A2, B1 + B2).
C * ciphertext(A, B) = ciphertext(scale(C, A), C * B).

nand(C1, C2) = ciphertext(V, 0i32) :- 
  mk_zero_vec(size, V).
