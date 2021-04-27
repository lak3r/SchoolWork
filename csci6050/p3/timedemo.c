#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
    
double time1()
{
    struct timeval tv;

    gettimeofday( &tv, ( struct timezone * ) 0 );
    return ( (double) (tv.tv_sec + (tv.tv_usec / 1000000.0)) );
}

double time2()
{
    return ( (double) time(NULL) );
}

void main(int argc,char *argv[])
{
    double t1, t2, t3, t4;

    t1 = time1();
    t2 = time2();
    printf("%lf %lf\n",t1,t2);
    sleep(2);
    t3 = time1();
    t4 = time2();
    printf("%lf %lf\n",t3,t4);

    printf("%lf %lf\n",t3-t1,t4-t2);
}
