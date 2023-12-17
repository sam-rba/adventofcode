#include "header.h"

unsigned char
hash(const unsigned char str[])
{
	unsigned char v;

	v = 0;
	for (; *str != '\0'; str++) {
		v += *str;
		v *= 17;
		v %= 256;
	}
	return v;
}

