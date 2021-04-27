#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char ** argv)
{
    int i, pgsz, *ip;
    unsigned char *cp;
    void *region;

    pgsz = getpagesize();
    printf("PGSZ %d\n",pgsz);

    region = mmap(NULL, pgsz, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
    if (region == ((void *) -1)) 
    {
        perror("mmap");
        return 1;
    }
    printf("region %p\n",region);

    memset(region,'Z',pgsz);

    cp = (unsigned char *)region;
    *(cp+1000) = 'a';

    ip = (int *)region;
    *(ip+251) = 65;  // region+1004

    cp = (unsigned char *)region;
    for (i=999; i < 1009; i++)  // Z a Z Z Z A 0 0 0 Z (all in hex)
    {
        printf("%02x ", *(cp+i));
    }
    printf("\n");

    return 0;
}
