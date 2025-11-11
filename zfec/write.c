// This is not part of the main build but is included for reference onby. 
// It's used to generate tables.c, but that's been done and the result committed to source control.

#include "fec.c"

/*
 * Primitive polynomials - see Lin & Costello, Appendix A,
 * and  Lee & Messerschmitt, p. 453.
 */
static const char*const Pp="101110001";
static int gf_log[256]; /* Poly->index form conversion table    */

/*
 * Generate GF(2**m) from the irreducible polynomial p(X) in p[0]..p[m]
 * Lookup tables:
 *     index->polynomial form		gf_exp[] contains j= \alpha^i;
 *     polynomial form -> index form	gf_log[ j = \alpha^i ] = i
 * \alpha=x is the primitive element of GF(2^m)
 *
 * For efficiency, gf_exp[] has size 2*GF_SIZE, so that a simple
 * multiplication of two numbers can be resolved without calling modnn
 */
static void
_init_mul_table(void) {
  int i, j;
  for (i = 0; i < 256; i++)
      for (j = 0; j < 256; j++)
          gf_mul_table[i][j] = gf_exp[modnn (gf_log[i] + gf_log[j])];

  for (j = 0; j < 256; j++)
      gf_mul_table[0][j] = gf_mul_table[j][0] = 0;
}

/*
 * initialize the data structures used for computations in GF.
 */
static void
generate_gf (void) {
    int i;
    gf mask;

    mask = 1;                     /* x ** 0 = 1 */
    gf_exp[8] = 0;          /* will be updated at the end of the 1st loop */
    /*
     * first, generate the (polynomial representation of) powers of \alpha,
     * which are stored in gf_exp[i] = \alpha ** i .
     * At the same time build gf_log[gf_exp[i]] = i .
     * The first 8 powers are simply bits shifted to the left.
     */
    for (i = 0; i < 8; i++, mask <<= 1) {
        gf_exp[i] = mask;
        gf_log[gf_exp[i]] = i;
        /*
         * If Pp[i] == 1 then \alpha ** i occurs in poly-repr
         * gf_exp[8] = \alpha ** 8
         */
        if (Pp[i] == '1')
            gf_exp[8] ^= mask;
    }
    /*
     * now gf_exp[8] = \alpha ** 8 is complete, so can also
     * compute its inverse.
     */
    gf_log[gf_exp[8]] = 8;
    /*
     * Poly-repr of \alpha ** (i+1) is given by poly-repr of
     * \alpha ** i shifted left one-bit and accounting for any
     * \alpha ** 8 term that may occur when poly-repr of
     * \alpha ** i is shifted.
     */
    mask = 1 << 7;
    for (i = 9; i < 255; i++) {
        if (gf_exp[i - 1] >= mask)
            gf_exp[i] = gf_exp[8] ^ ((gf_exp[i - 1] ^ mask) << 1);
        else
            gf_exp[i] = gf_exp[i - 1] << 1;
        gf_log[gf_exp[i]] = i;
    }
    /*
     * log(0) is not defined, so use a special value
     */
    gf_log[0] = 255;
    /* set the extended gf_exp values for fast multiply */
    for (i = 0; i < 255; i++)
        gf_exp[i + 255] = gf_exp[i];

    /*
     * again special cases. 0 has no inverse. This used to
     * be initialized to 255, but it should make no difference
     * since noone is supposed to read from here.
     */
    inverse[0] = 0;
    inverse[1] = 1;
    for (i = 2; i <= 255; i++)
        inverse[i] = gf_exp[255 - gf_log[i]];
}


/* This is the logic for setting up gf_exp, gf_log, gf_inverse and gf_mul_table.
 * It is not used at run time but has been invoked by write.c to generate
 * tables.c in which the contents of these tables are initialised verbatim.
 */
void
fec_init_internal (void) {
  generate_gf();
  _init_mul_table();
}

/* The above comprises all code used purely for table generation.
 * Now we just print the results
 */

void print_table_int(const char * name, const int data[], int len) {
  printf("static int %s[%d]={", name, len);
  printf("%d", data[0]); 
  for (int i=1; i<len; i++) printf(",%d", data[i]);
  printf("};\n\n");
}

void print_gfs(const gf data[], int len)  {
  printf("{%d", data[0]);
  for (int i=1; i<len; i++) printf(",%d", data[i]);
  printf("}");
}

void print_table_gf(const char * name, const gf data[], int len) {
  printf("static gf %s[%d]=", name, len);
  print_gfs(data, len);
  printf(";\n\n");
}

void print_table_gf_256_256(const char * name, const gf data[256][256]) {
  printf("static gf %s[256][256]={", name);
  print_gfs(data[0], 256); 
  for (int i=1; i<256; i++) {
    printf(",\n");
    print_gfs(data[i], 256);
  }
  printf("};\n");
}

int main()
{
  fec_init_internal();
  print_table_gf("gf_exp", gf_exp, 510);
  print_table_int("gf_log", gf_log, 256);
  print_table_gf("inverse", inverse, 256);
  print_table_gf_256_256("gf_mul_table", gf_mul_table);
}
 
