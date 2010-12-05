// -------------------------------------------------------------------
//
// erlz: Erlang bindings for the fastlz compression library
//
// Copyright (c) 2010 Hypothetical Labs, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#include "erl_nif_compat.h"
#include "fastlz.h"

// NIF functions
ERL_NIF_TERM erlz_nifs_compress(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM erlz_nifs_decompress(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc erlz_nif_funcs[] =
{
    {"compress", 1, erlz_nifs_compress},
    {"decompress", 1, erlz_nifs_decompress}
};

ERL_NIF_INIT(erlz_nifs, erlz_nif_funcs, NULL, NULL, NULL, NULL);

int erlz_compress(ErlNifEnv *env, ErlNifBinary *source, ErlNifBinary *target) {
    int retval = 0;
    int bufsize;
    double expansion_factor = 1.1;
    int result;
    while(expansion_factor < 2.5) {
        bufsize = (int) source->size * expansion_factor;
        bufsize = bufsize < 66 ? 66 : bufsize;
        enif_alloc_binary_compat(env, bufsize, target);
        result = fastlz_compress_level(2, source->data, source->size, target->data);
        if (result) {
            enif_realloc_binary_compat(env, target, result);
            retval = 1;
            break;
        }
        else {
            enif_release_binary_compat(env, target);
        }
        expansion_factor += 0.1;
    }
    return retval;
}

int erlz_decompress(ErlNifEnv *env, ErlNifBinary *source, ErlNifBinary *target) {
    int retval = 0;
    int bufsize;
    double expansion_factor = 1.1;
    int result;
    while(expansion_factor < 2.5) {
        bufsize = (int) source->size * expansion_factor;
        bufsize = bufsize < 66 ? 66 : bufsize;
        enif_alloc_binary_compat(env, bufsize, target);
        result = fastlz_decompress(source->data, source->size, target->data, target->size);
        if (result) {
            enif_realloc_binary_compat(env, target, result);
            retval = 1;
            break;
        }
        else {
            enif_release_binary_compat(env, target);
        }
        expansion_factor += 0.1;
    }
    return retval;
}

ERL_NIF_TERM erlz_nifs_compress(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    ErlNifBinary source;
    ErlNifBinary target;

    if (argc != 1 || !enif_inspect_binary(env, argv[0], &source)) {
        return enif_make_badarg(env);
    }
    if (erlz_compress(env, &source, &target)) {
        retval = enif_make_binary(env, &target);
    }
    else {
        retval = enif_make_badarg(env);
    }
    return retval;
}

ERL_NIF_TERM erlz_nifs_decompress(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    ErlNifBinary source;
    ErlNifBinary target;
    if (argc != 1 || !enif_inspect_binary(env, argv[0], &source)) {
        return enif_make_badarg(env);
    }
    if (erlz_decompress(env, &source, &target)) {
        retval = enif_make_binary(env, &target);
    }
    else {
        retval = enif_make_badarg(env);
    }
    return retval;
}
