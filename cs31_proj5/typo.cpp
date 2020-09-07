//
//  typo.cpp
//  Project 5
//
//  Created by Junho Choi on 5/17/20.
//  Copyright © 2020 Junho Choi. All rights reserved.
//

#include <iostream>
#include <string>
#include <cassert>
using namespace std;

int scoreTypo(const string dictionary[], int n, string word){
    
    string dictionaryEntry; //holds entry of dictionary
    int typoCount=0; //counts # of typos
    int leastTypoCount = 1000;
    int skipIndex = 0; //if extra letter or missing letter in word, tracks how many indexes must be skipped in dictionary
    
    if(n<=0) return -1;
    
    for(int dictIndex=0; dictIndex<n; dictIndex++){ //goes through dictionary
        dictionaryEntry = dictionary[dictIndex];
        skipIndex = 0;
        typoCount = 0; //reset typoCount to 0 every iteration
        
        for(int wordPos=0; (wordPos+skipIndex)<dictionaryEntry.length() && wordPos < word.length(); wordPos++){ //goes through entry in dictionary
            
            if(word[wordPos]!=dictionaryEntry[wordPos+skipIndex]){
                typoCount++;
                /*switched character in word*/
                if(word[wordPos+1] == dictionaryEntry[wordPos] && word[wordPos]==dictionaryEntry[wordPos+1] ){
                    wordPos++; //skip those letters
                /*extra character in word*/
                }else if(word[wordPos]==dictionaryEntry[wordPos+1]){
                    skipIndex++;
                /*missing character in word*/
                }else if(word[wordPos+1]==dictionaryEntry[wordPos]){
                    skipIndex--;
                }
                
            }
        }
        
        if(word.length() == dictionaryEntry.length()-skipIndex+1) //if word is longer than dictionary entry, account for that
            typoCount++;
        else if(word.length() > dictionaryEntry.length()-skipIndex+1)
            typoCount+=2;
        if(dictIndex==0 || typoCount < leastTypoCount)
            leastTypoCount = typoCount;
    }
    
    if(leastTypoCount <=2)
        return leastTypoCount;
    else
        return 2;
        
}

int main()
{
      // Here are some tests.  You may add more if you wish.
    string dict1[6] = { "february", "pointer", "country", "forty", "conversation", "minuscule" };
    assert(scoreTypo(dict1, 0, "forty") == -1);
    assert(scoreTypo(dict1, 6, "forty") == 0);
    assert(scoreTypo(dict1, 6, "fourty") == 1);
    assert(scoreTypo(dict1, 6, "febuary") == 1);
    assert(scoreTypo(dict1, 6, "miniscule") == 1);
    assert(scoreTypo(dict1, 6, "poitner") == 1);
    assert(scoreTypo(dict1, 6, "conservation") == 2);
    
    string dict2[6] = { "banana", "apple", "pumpkin", "apricot", "peach", "pear" };
    assert(scoreTypo(dict2, -2, "apple") == -1);
    assert(scoreTypo(dict2, 3, "aplpe") == 1);
    assert(scoreTypo(dict2, 6, "each") == 1);
    assert(scoreTypo(dict2, 6, "par") == 1);
    assert(scoreTypo(dict2, 6, "bnaana") == 1);
    assert(scoreTypo(dict2, 6, "peaah") == 1);
    assert(scoreTypo(dict2, 2, "pear") == 2);
    assert(scoreTypo(dict2, 6, "cucumber") == 2);
    
    string dict3[4] = { "brown", "red", "pink", "blue"};
    assert(scoreTypo(dict3, 0, "grapes") == -1);
    assert(scoreTypo(dict3, 2, "rde") == 1);
    assert(scoreTypo(dict3, 3, "read") == 1);
    assert(scoreTypo(dict3, 4, "bule") == 1);
    assert(scoreTypo(dict3, 2, "borwn") == 1);
    assert(scoreTypo(dict3, 3, "pnk") == 1);
    assert(scoreTypo(dict3, 1, "pink") == 2);
    assert(scoreTypo(dict3, 4, "cucumber") == 2);
    
    string dict4[5] = { "library", "school", "dorm", "ucla", "study"};
    assert(scoreTypo(dict4, 2, "scohol") == 1);
    assert(scoreTypo(dict4, 3, "dormm") == 1);
    assert(scoreTypo(dict4, 4, "lucla") == 1);
    assert(scoreTypo(dict4, 5, "studs") == 1);
    assert(scoreTypo(dict4, 4, "ulca") == 1);
    assert(scoreTypo(dict4, 2, "libarry") == 1);
    assert(scoreTypo(dict4, 2, "lbrayr") == 2);
    cout << "All tests succeeded" << endl;
}
