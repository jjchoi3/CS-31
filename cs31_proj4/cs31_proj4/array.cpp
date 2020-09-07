//
//  array.cpp
//  Project 4.1
//
//  Created by Junho Choi on 5/9/20.
//  Copyright © 2020 Junho Choi. All rights reserved.
//

#include <iostream>
#include <string>
#include <cassert>
using namespace std;

int firstDifference(const string a1[], int n1, const string a2[], int n2)
{
  // This function considers only the first n1 elements of the array a1 and the
  // first n2 elements of the array a2.  It returns the position of the first
  // corresponding elements of a1 and a2 that are not equal.  If the arrays are
  // equal up to the point where one or both of them runs out, then return
  // whichever value of n1 or n2 is less than or equal to the other.  If n1 is
  // 0, that means a1 runs out immediately; similarly, for n2 and a2.  If n1 or
  // n2 is negative, act as if it were 0.
    
    if(n1<0)
        n1=0;
    if(n2<0)
        n2=0;
    for(int i=0; i<n1 && i < n2; i++){
        if(a1[i]!=a2[i])
            return i;
    }
    
    if(n1>n2)
        return n2;
    else
        return n1;
}
bool isContainedIn(const string a1[], int n1, const string a2[], int n2)
{
  // This function considers only the first n1 elements of the array a1 and the
  // first n2 elements of the array a2.  If all n1 elements of a1 appear as
  // elements in a2, in the same order (though not necessarily consecutively),
  // then return true.  Return false if a2 does not so contain a1.  Of course,
  // a sequence of 0 elements is contained in every sequence, even a sequence
  // of 0 elements.  If n1 or n2 is negative, act as if it were 0.

    
    int i =0; //tracks a1
    int j = 0; //tracks a2

    //if n1 is negative or 0, return true
    if(n1<=0)
        return true;
    
    if(n1<=n2){
        while(i<n1 && j<n2){ //look for first occurence of matching item
            if(a1[i] == a2[j]){ //if there is a match, move to next index in a1
                i++;
                j++;
            }else
                j++;
        }
        if(i!=n1) //return false if didn't make it to end of a1
            return false;
        else //return true iff a1 index reached the end
            return true;
    }else //if n2 is greater, then a1 cannot be contained within n2
        return false;
    
}


int main()
{
  // Here are some tests that demonstrate what these functions do.  You may add
  // more if you wish.
    string colors[6] = { "pink", "blue", "white", "red", "black", "blue" };
    string hues[5] = { "pink", "blue", "white", "black", "green" };
    string tint[5] = { "yellow", "blue", "white", "black", "green" };
    assert(firstDifference(colors, 6, hues, 5) == 3);
    assert(firstDifference(colors, 4, hues, 5) == 3);
    assert(firstDifference(colors, 2, hues, 5) == 2);
    assert(firstDifference(colors, 2, hues, 2) == 2);
    assert(firstDifference(colors, 5, hues, 1) == 1);
    assert(firstDifference(colors, 5, hues, 0) == 0);
    assert(firstDifference(colors, 5, tint, 0) == 0);
    string little1[4] = { "blue", "white", "black", "blue" };
    assert(isContainedIn(little1, 4, colors, 6));
    string little2[2] = { "red", "white" };
    assert(!isContainedIn(little2, 2, colors, 6));
    string little3[3] = { "blue", "blue", "red" };
    assert(!isContainedIn(little3, 3, colors, 6));
    string little4[3] = { "yellow","white", "black" };
    assert(!isContainedIn(little4, 3, tint, 3));
    assert(isContainedIn(hues, 3, colors, 3));
    assert(!isContainedIn(hues, 3, colors, 2));
    assert(isContainedIn(hues, 0, tint, 5));
    assert(isContainedIn(hues, 0, tint, 0));
    cout << "All tests succeeded" << endl;
}
