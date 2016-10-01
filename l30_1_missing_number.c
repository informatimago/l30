/*
Read more: http://javarevisited.blogspot.com/2015/06/top-20-array-interview-questions-and-answers.html
*/


/*
1. How to find missing number in integer vector of 1 to 100? 

You have given an integer vector which contains numbers from 1 to 100
but one number is missing, you need to write a Lisp program to find
that missing number in vector.

One trick to solve this problem is calculate sum of all numbers in
vector and compare with expected sum, the difference would be the
missing number.
*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int sum_vector(int vector[],int size){
    int s=0;
    for(int i=0;i<size;++i){
        s+=vector[i];
    }
    return s;
}


int sum_1_to_n(int n){
    return n*(n+1)/2;
}


int fill_vector(int vector[],const int N,const int missing){
    int size=0;
    for(int i=1;i<=N;++i){
        if(i!=missing){
            vector[size++]=i;
        }
    }
    return size;
}


void l30_1(){
    const int N=100;
    int vector[N];
    int actual_missing=1+rand()%N;
    int size=fill_vector(vector,N,actual_missing);
    int expected_sum=sum_1_to_n(N);
    int actual_sum=sum_vector(vector,size);
    int computed_missing=expected_sum-actual_sum;
    printf("  actual missing = %3d\n",actual_missing);
    printf("computed missing = %3d\n",computed_missing);
    if(computed_missing==actual_missing){
        printf("yay!\n");
    }else{
        printf("bummer!\n");
    }
}


int main(){
    srand(clock());
    l30_1();
    return 0;
}

/* THE END */
