/**
 * zfec -- fast forward error correction library with Python interface
 */

#include "fec.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "tables.c"



/*
 * To speed up computations, we have tables for logarithm, exponent and
 * inverse of a number.  We use a table for multiplication as well (it takes
 * 64K, no big deal even on a PDA, especially because it can be
 * pre-initialized an put into a ROM!), otherwhise we use a table of
 * logarithms. In any case the macro gf_mul(x,y) takes care of
 * multiplications.
 */

static gf gf_exp[510];  /* index->poly form conversion table    */
static gf inverse[256]; /* inverse of field elem.               */
                                /* inv[\alpha**i]=\alpha**(GF_SIZE-i-1) */

/*
 * modnn(x) computes x % GF_SIZE, where GF_SIZE is 2**GF_BITS - 1,
 * without a slow divide.
 */
static gf
modnn(int x) {
    while (x >= 255) {
        x -= 255;
        x = (x >> 8) + (x & 255);
    }
    return x;
}

#define SWAP(a,b,t) {t tmp; tmp=a; a=b; b=tmp;}

/*
 * gf_mul(x,y) multiplies two numbers.  It is much faster to use a
 * multiplication table.
 *
 * USE_GF_MULC, GF_MULC0(c) and GF_ADDMULC(x) can be used when multiplying
 * many numbers by the same constant. In this case the first call sets the
 * constant, and others perform the multiplications.  A value related to the
 * multiplication is held in a local variable declared with USE_GF_MULC . See
 * usage in _addmul1().
 */
static gf gf_mul_table[256][256];

#define gf_mul(x,y) gf_mul_table[x][y]

#define USE_GF_MULC register gf * __gf_mulc_

#define GF_MULC0(c) __gf_mulc_ = gf_mul_table[c]
#define GF_ADDMULC(dst, x) dst ^= __gf_mulc_[x]

#define NEW_GF_MATRIX(rows, cols) \
    (gf*)malloc(rows * cols)


/*
 * Various linear algebra operations that i use often.
 */

/*
 * addmul() computes dst[] = dst[] + c * src[]
 * This is used often, so better optimize it! Currently the loop is
 * unrolled 16 times, a good value for 486 and pentium-class machines.
 * The case c=0 is also optimized, whereas c=1 is not. These
 * calls are unfrequent in my typical apps so I did not bother.
 */
#define addmul(dst, src, c, sz)                 \
    if (c != 0) _addmul1(dst, src, c, sz)

#define UNROLL 16               /* 1, 4, 8, 16 */
static void
_addmul1(register gf*restrict dst, const register gf*restrict src, gf c, size_t sz) {
    USE_GF_MULC;
    const gf* lim = &dst[sz - UNROLL + 1];

    GF_MULC0 (c);

#if (UNROLL > 1)                /* unrolling by 8/16 is quite effective on the pentium */
    for (; dst < lim; dst += UNROLL, src += UNROLL) {
        GF_ADDMULC (dst[0], src[0]);
        GF_ADDMULC (dst[1], src[1]);
        GF_ADDMULC (dst[2], src[2]);
        GF_ADDMULC (dst[3], src[3]);
#if (UNROLL > 4)
        GF_ADDMULC (dst[4], src[4]);
        GF_ADDMULC (dst[5], src[5]);
        GF_ADDMULC (dst[6], src[6]);
        GF_ADDMULC (dst[7], src[7]);
#endif
#if (UNROLL > 8)
        GF_ADDMULC (dst[8], src[8]);
        GF_ADDMULC (dst[9], src[9]);
        GF_ADDMULC (dst[10], src[10]);
        GF_ADDMULC (dst[11], src[11]);
        GF_ADDMULC (dst[12], src[12]);
        GF_ADDMULC (dst[13], src[13]);
        GF_ADDMULC (dst[14], src[14]);
        GF_ADDMULC (dst[15], src[15]);
#endif
    }
#endif
    lim += UNROLL - 1;
    for (; dst < lim; dst++, src++)       /* final components */
        GF_ADDMULC (*dst, *src);
}

/*
 * computes C = AB where A is n*k, B is k*m, C is n*m
 */
static void
_matmul(gf * a, gf * b, gf * c, unsigned n, unsigned k, unsigned m) {
    unsigned row, col, i;

    for (row = 0; row < n; row++) {
        for (col = 0; col < m; col++) {
            gf *pa = &a[row * k];
            gf *pb = &b[col];
            gf acc = 0;
            for (i = 0; i < k; i++, pa++, pb += m)
                acc ^= gf_mul (*pa, *pb);
            c[row * m + col] = acc;
        }
    }
}

/*
 * _invert_mat() takes a matrix and produces its inverse
 * k is the size of the matrix.
 * (Gauss-Jordan, adapted from Numerical Recipes in C)
 * Return non-zero if singular.
 */
static void
_invert_mat(gf* src, size_t k) {
    gf c;
    size_t irow = 0;
    size_t icol = 0;
    size_t row, col, i, ix;

    unsigned* indxc = (unsigned*) malloc (k * sizeof(unsigned));
    unsigned* indxr = (unsigned*) malloc (k * sizeof(unsigned));
    unsigned* ipiv = (unsigned*) malloc (k * sizeof(unsigned));
    gf *id_row = NEW_GF_MATRIX (1, k);

    memset (id_row, '\0', k * sizeof (gf));
    /*
     * ipiv marks elements already used as pivots.
     */
    for (i = 0; i < k; i++)
        ipiv[i] = 0;

    for (col = 0; col < k; col++) {
        gf *pivot_row;
        /*
         * Zeroing column 'col', look for a non-zero element.
         * First try on the diagonal, if it fails, look elsewhere.
         */
        if (ipiv[col] != 1 && src[col * k + col] != 0) {
            irow = col;
            icol = col;
            goto found_piv;
        }
        for (row = 0; row < k; row++) {
            if (ipiv[row] != 1) {
                for (ix = 0; ix < k; ix++) {
                    if (ipiv[ix] == 0) {
                        if (src[row * k + ix] != 0) {
                            irow = row;
                            icol = ix;
                            goto found_piv;
                        }
                    } else
                        assert (ipiv[ix] <= 1);
                }
            }
        }
      found_piv:
        ++(ipiv[icol]);
        /*
         * swap rows irow and icol, so afterwards the diagonal
         * element will be correct. Rarely done, not worth
         * optimizing.
         */
        if (irow != icol)
            for (ix = 0; ix < k; ix++)
                SWAP (src[irow * k + ix], src[icol * k + ix], gf);
        indxr[col] = irow;
        indxc[col] = icol;
        pivot_row = &src[icol * k];
        c = pivot_row[icol];
        assert (c != 0);
        if (c != 1) {                       /* otherwhise this is a NOP */
            /*
             * this is done often , but optimizing is not so
             * fruitful, at least in the obvious ways (unrolling)
             */
            c = inverse[c];
            pivot_row[icol] = 1;
            for (ix = 0; ix < k; ix++)
                pivot_row[ix] = gf_mul (c, pivot_row[ix]);
        }
        /*
         * from all rows, remove multiples of the selected row
         * to zero the relevant entry (in fact, the entry is not zero
         * because we know it must be zero).
         * (Here, if we know that the pivot_row is the identity,
         * we can optimize the addmul).
         */
        id_row[icol] = 1;
        if (memcmp (pivot_row, id_row, k * sizeof (gf)) != 0) {
            gf *p = src;
            for (ix = 0; ix < k; ix++, p += k) {
                if (ix != icol) {
                    c = p[icol];
                    p[icol] = 0;
                    addmul (p, pivot_row, c, k);
                }
            }
        }
        id_row[icol] = 0;
    }                           /* done all columns */
    for (col = k; col > 0; col--)
        if (indxr[col-1] != indxc[col-1])
            for (row = 0; row < k; row++)
                SWAP (src[row * k + indxr[col-1]], src[row * k + indxc[col-1]], gf);
    free(indxc);
    free(indxr);
    free(ipiv);
    free(id_row);
}

/*
 * fast code for inverting a vandermonde matrix.
 *
 * NOTE: It assumes that the matrix is not singular and _IS_ a vandermonde
 * matrix. Only uses the second column of the matrix, containing the p_i's.
 *
 * Algorithm borrowed from "Numerical recipes in C" -- sec.2.8, but largely
 * revised for my purposes.
 * p = coefficients of the matrix (p_i)
 * q = values of the polynomial (known)
 */
void
_invert_vdm (gf* src, unsigned k) {
    unsigned i, j, row, col;
    gf *b, *c, *p;
    gf t, xx;

    if (k == 1)                   /* degenerate case, matrix must be p^0 = 1 */
        return;
    /*
     * c holds the coefficient of P(x) = Prod (x - p_i), i=0..k-1
     * b holds the coefficient for the matrix inversion
     */
    c = NEW_GF_MATRIX (1, k);
    b = NEW_GF_MATRIX (1, k);

    p = NEW_GF_MATRIX (1, k);

    for (j = 1, i = 0; i < k; i++, j += k) {
        c[i] = 0;
        p[i] = src[j];            /* p[i] */
    }
    /*
     * construct coeffs. recursively. We know c[k] = 1 (implicit)
     * and start P_0 = x - p_0, then at each stage multiply by
     * x - p_i generating P_i = x P_{i-1} - p_i P_{i-1}
     * After k steps we are done.
     */
    c[k - 1] = p[0];              /* really -p(0), but x = -x in GF(2^m) */
    for (i = 1; i < k; i++) {
        gf p_i = p[i];            /* see above comment */
        for (j = k - 1 - (i - 1); j < k - 1; j++)
            c[j] ^= gf_mul (p_i, c[j + 1]);
        c[k - 1] ^= p_i;
    }

    for (row = 0; row < k; row++) {
        /*
         * synthetic division etc.
         */
        xx = p[row];
        t = 1;
        b[k - 1] = 1;             /* this is in fact c[k] */
        for (i = k - 1; i > 0; i--) {
            b[i-1] = c[i] ^ gf_mul (xx, b[i]);
            t = gf_mul (xx, t) ^ b[i-1];
        }
        for (col = 0; col < k; col++)
            src[col * k + row] = gf_mul (inverse[t], b[col]);
    }
    free (c);
    free (b);
    free (p);
    return;
}


/*
 * This section contains the proper FEC encoding/decoding routines.
 * The encoding matrix is computed starting with a Vandermonde matrix,
 * and then transforming it into a systematic matrix.
 */

#define FEC_MAGIC	0xFECC0DEC

void
fec_free (fec_t *p) {
    assert (p != NULL && p->magic == (((FEC_MAGIC ^ p->k) ^ p->n) ^ (unsigned long) (p->enc_matrix)));
    free (p->enc_matrix);
    free (p);
}

fec_t *
fec_new(unsigned short k, unsigned short n) {
    unsigned row, col;
    gf *p, *tmp_m;

    fec_t *retval;

    assert(k >= 1);
    assert(n >= 1);
    assert(n <= 256);
    assert(k <= n);

    retval = (fec_t *) malloc (sizeof (fec_t));
    retval->k = k;
    retval->n = n;
    retval->enc_matrix = NEW_GF_MATRIX (n, k);
    retval->magic = ((FEC_MAGIC ^ k) ^ n) ^ (unsigned long) (retval->enc_matrix);
    tmp_m = NEW_GF_MATRIX (n, k);
    /*
     * fill the matrix with powers of field elements, starting from 0.
     * The first row is special, cannot be computed with exp. table.
     */
    tmp_m[0] = 1;
    for (col = 1; col < k; col++)
        tmp_m[col] = 0;
    for (p = tmp_m + k, row = 0; row + 1 < n; row++, p += k)
        for (col = 0; col < k; col++)
            p[col] = gf_exp[modnn (row * col)];

    /*
     * quick code to build systematic matrix: invert the top
     * k*k vandermonde matrix, multiply right the bottom n-k rows
     * by the inverse, and construct the identity matrix at the top.
     */
    _invert_vdm (tmp_m, k);        /* much faster than _invert_mat */
    _matmul(tmp_m + k * k, tmp_m, retval->enc_matrix + k * k, n - k, k, k);
    /*
     * the upper matrix is I so do not bother with a slow multiply
     */
    memset (retval->enc_matrix, '\0', k * k * sizeof (gf));
    for (p = retval->enc_matrix, col = 0; col < k; col++, p += k + 1)
        *p = 1;
    free (tmp_m);

    return retval;
}

/* To make sure that we stay within cache in the inner loops of fec_encode().  (It would
   probably help to also do this for fec_decode(). */
#ifndef STRIDE
#define STRIDE 8192
#endif

void
fec_encode(const fec_t* code, const gf*restrict const*restrict const src, gf*restrict const*restrict const fecs, const unsigned*restrict const block_nums, size_t num_block_nums, size_t sz) {
    unsigned char i, j;
    size_t k;
    unsigned fecnum;
    const gf* p;

    for (k = 0; k < sz; k += STRIDE) {
        size_t stride = ((sz-k) < STRIDE)?(sz-k):STRIDE;
        for (i=0; i<num_block_nums; i++) {
            fecnum=block_nums[i];
            assert (fecnum >= code->k);
            memset(fecs[i]+k, 0, stride);
            p = &(code->enc_matrix[fecnum * code->k]);
            for (j = 0; j < code->k; j++)
                addmul(fecs[i]+k, src[j]+k, p[j], stride);
        }
    }
}

/**
 * Build decode matrix into some memory space.
 *
 * @param matrix a space allocated for a k by k matrix
 */
void
build_decode_matrix_into_space(const fec_t*restrict const code, const unsigned*const restrict index, const unsigned k, gf*restrict const matrix) {
    unsigned short i;
    gf* p;
    for (i=0, p=matrix; i < k; i++, p += k) {
        if (index[i] < k) {
            memset(p, 0, k);
            p[i] = 1;
        } else {
            memcpy(p, &(code->enc_matrix[index[i] * code->k]), k);
        }
    }
    _invert_mat (matrix, k);
}

void
fec_decode(const fec_t* code, const gf*restrict const*restrict const inpkts, gf*restrict const*restrict const outpkts, const unsigned*restrict const index, size_t sz) {
    gf* m_dec = (gf*)alloca(code->k * code->k);

    /* char is large enough for outix - it counts the number of primary blocks
       we are decoding for return.  the most primary blocks we might have to
       decode is for k == 128, m == 256.  in this case we might be given 128
       secondary blocks and have to decode 128 primary blocks.  if k decreases
       then the number of total blocks we might have to return decreases.  if
       k increases then the number of secondary blocks that exist decreases so
       we will be passed some primary blocks and the number of primary blocks
       we have to decode decreases. */
    unsigned char outix=0;

    /* row and col are compared directly to k, which could be 256, so make
       them large enough to represent 256.
     */
    unsigned short row=0;
    unsigned short col=0;
    build_decode_matrix_into_space(code, index, code->k, m_dec);

    for (row=0; row<code->k; row++) {
        assert ((index[row] >= code->k) || (index[row] == row)); /* If the block whose number is i is present, then it is required to be in the i'th element. */
        if (index[row] >= code->k) {
            memset(outpkts[outix], 0, sz);
            for (col=0; col < code->k; col++)
                addmul(outpkts[outix], inpkts[col], m_dec[row * code->k + col], sz);
            outix++;
        }
    }
}

/**
 * zfec -- fast forward error correction library with Python interface
 *
 * Copyright (C) 2007-2010 Zooko Wilcox-O'Hearn
 * Author: Zooko Wilcox-O'Hearn
 *
 * This file is part of zfec.
 *
 * See README.rst for licensing information.
 */

/*
 * This work is derived from the "fec" software by Luigi Rizzo, et al., the
 * copyright notice and licence terms of which are included below for reference.
 * fec.c -- forward error correction based on Vandermonde matrices 980624 (C)
 * 1997-98 Luigi Rizzo (luigi@iet.unipi.it)
 *
 * Portions derived from code by Phil Karn (karn@ka9q.ampr.org),
 * Robert Morelos-Zaragoza (robert@spectra.eng.hawaii.edu) and Hari
 * Thirumoorthy (harit@spectra.eng.hawaii.edu), Aug 1995
 *
 * Modifications by Dan Rubenstein (see Modifications.txt for
 * their description.
 * Modifications (C) 1998 Dan Rubenstein (drubenst@cs.umass.edu)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above
 *    copyright notice, this list of conditions and the following
 *    disclaimer in the documentation and/or other materials
 *    provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 */
