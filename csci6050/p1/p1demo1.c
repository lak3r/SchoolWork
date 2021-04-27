#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char** argv)
{
    int rc, fd;
    char buff[100];
    double d1 = 32.0, d2 = 32.5, d3, d4, da[2];

    fd = open("tempa", O_WRONLY|O_CREAT,0644);
    if (fd < 0)
    {
        printf("**** failed to open tempa\n");
        exit(-1);
    }
    sprintf(buff,"%lf %lf",d1,d2);
    rc = write(fd,buff,strlen(buff)+1);
    close(fd);

    fd = open("tempb", O_WRONLY|O_CREAT,0644);
    if (fd < 0)
    {
        printf("**** failed to open tempb\n");
        exit(-1);
    }
    rc = write(fd,&d1,sizeof(double));
    rc = write(fd,&d2,sizeof(double));
    close(fd);

    fd = open("tempa", O_RDONLY);
    if (fd < 0)
    {
        printf("**** failed to open tempb\n");
        exit(-1);
    }
    buff[0] = '\0';
    rc = read(fd,buff,100);
    printf("buff: %s\n",buff);
    close(fd);

    fd = open("tempb", O_RDONLY);
    if (fd < 0)
    {
        printf("**** failed to open tempb\n");
        exit(-1);
    }
    rc = read(fd,&d3,sizeof(double));
    printf("d3: %lf\n",d3);
    rc = read(fd,&d4,sizeof(double));
    printf("d4: %lf\n",d4);
    close(fd);

    fd = open("tempb", O_RDONLY);
    if (fd < 0)
    {
        printf("**** failed to open tempb\n");
        exit(-1);
    }
    rc = read(fd,da,2*sizeof(double));
    printf("da: %lf %lf\n",da[0],da[1]);
    close(fd);

    return 0;
}
