#include <stdio.h>

extern char *greeting(void);

int main(void)
{
  printf("%s\n", greeting());
  return 0;
}
