// This is not part of the main build but is included for reference onby. 
// It's used to generate tables.c, but that's been done and the result committed to source control.

#include "fec.c"

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
 
