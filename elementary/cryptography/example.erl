-module(example).
-export([generate_key/0]).


geenerate_key() ->
    crypto:generate_key(dh, [3,9]). % => {PUK, PrivateKeyOut} 

pub_encrypt() ->
    crypto:public_encrypt(rsa, list_to_binary("64"), [3,19], rsa_pkcs1_oaep_padding).
