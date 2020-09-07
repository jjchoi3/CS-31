//
//  Orders.cpp
//  Project 3
//
//  Created by Junho Choi on 4/30/20.
//  Copyright © 2020 Junho Choi. All rights reserved.
//

#include <iostream>
#include <cctype>
using namespace std;

//required functions
bool hasValidSyntax(string orders);
int countCases(string orders, char status, int& caseCount);

//helper functions
bool isValidStateCode(string code, int i);
int isValidTotal(string code, int i);
char isValidSign(string code, int i);
bool isZeroOrders(string code);
bool isValidUppercaseStateCode(string stateCode);

int stringToInt(string n);

int main(){
    /*string code;
    string stateCodeTest;
    int cases;
    
    getline(cin, code);
    switch (countCases(code, '-', cases)) {
        case 0:
            cout<< "Success" << endl;
            cout << "Cases : " << cases <<endl;
            break;
        case 1:
            cout<< "Invalid Code" << endl;
            cout << "Cases : " << cases <<endl;
            break;
        case 2:
            cout<< "Company ordered 0" << endl;
            break;
        case 3:
            cout << "Invalid Status" << endl;
            break;
        default:
            cout << "Error" <<endl;
            break;
    }*/
}


// Return true if the argument is a two-uppercase-letter state code, or
// false otherwise.
bool isValidUppercaseStateCode(string stateCode){
    const string codes =
    "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.GU.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MP.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.PR.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2  &&
        stateCode.find('.') == string::npos  &&  // no '.' in stateCode
        codes.find(stateCode) != string::npos);  // match found
}

//checks if valid syntax
bool hasValidSyntax(string orders){
    
    int index=0;//index, counts index throughout
    
    if(orders == "")
        return true;
    
    for(;;){
        if(isValidStateCode(orders, index)){
            index+=2; //if valid state code, should take first two indexes
            
            if(isValidTotal(orders, index)!=-1){
                index+=isValidTotal(orders,index);
                
                if(isValidSign(orders, index)=='+' || isValidSign(orders, index)=='-'){
                    if(index==orders.length()-1) //if end of string, must be true
                        return true;
                    index++;
                }else{
                    return false;
                }
            }else
                return false;
        }else{
            return false;
        }
    }
    
    
    return true;
}

//counts the number of cases corresponding to the status
//returns 1 is not valid syntax, 2 if there are zero orders for a state, 3 if incorrect status, and 0 if no errors
int countCases(string orders, char status, int& countCases){
    
    int index =0;
    int totalOrders = 0; // counts total orders for matching status
    string numberString;
    int numEndLocation;
    
    if(!hasValidSyntax(orders)){ //return 1 if not valid syntax
        return 1;
    }else if(!(status=='+') && !(status=='-')){//return 3 if not proper status
        return 3;
    }else if(isZeroOrders(orders)){
        return 2;
    }
    
    while(index!=orders.length()){
        index+=2; //first two indexes must be state code
        numEndLocation = isValidTotal(orders, index); // location of the end of the order number
        index += numEndLocation;
        if(isValidSign(orders,index)==status){
            for(int i = index-numEndLocation;i<index;i++){ //start from beginning of number to end
                numberString+=orders.at(i);
            }
            index++;
            totalOrders += stringToInt(numberString);
        }else{
            index+=isValidTotal(orders,index)+1; // skip orders "block" if not correct sign
        }
        numberString=""; //reinitialize
    }
    
    //return 0 if all conditions pass
    countCases = totalOrders;
    return 0;
    
    
}


//returns bool whether it is valid state code
//returns false if error
bool isValidStateCode(string code, int i){
    string stateInitial="";
    int index = i;
    
    for(;index<code.length();index++){
        if(isalpha(code.at(index))){ //if alphabet count letter
            stateInitial+=toupper(code.at(index)); //isValidUppercaseStateCode only accepts uppercase codes
        }else{
            return false;
        }
        if(index==i+1){ //when index has checked 2 letters
            break;
        }
    }
    if((index+1)<code.length() && isdigit(code.at(index+1))){ //next index must be number
        if(isValidUppercaseStateCode(stateInitial)){
            stateInitial="";
            return true;
        }else
            return false;
            
    }
    return false;
}

//returns number of indexes the count takes up
//returns -1 if error
int isValidTotal(string code, int i){
    int total=0;
    
    for(;i!=code.length();i++){
        if(isdigit(code.at(i))){
            total++;
        }else{
            return total;//-1?
        }
    }
    return -1; //"false"
    
}

//returns character
//returns empty if error
char isValidSign(string code, int i){
    if(code.at(i)=='+')
        return '+';
    if(code.at(i)=='-')
        return '-';
    else return ' '; //"false"
}

//changes string to int
int stringToInt(string stringNum){
    int intNum =0;
    int multiplier =1;
    for(int i=stringNum.length()-1; i>=0; i--){ //going backwards beginning at the end of the number
        intNum += multiplier*(stringNum.at(i) - '0');
        multiplier*=10; //multiply by factor of ten each iteration
    }
    return intNum;
}

//returns whether there are any orders of 0
bool isZeroOrders(string code){
    int index =0;
    string numberString;
    int numEndLocation;
    
    while(index!=code.length()){
        index+=2; //first two indexes must be state code
        
        numEndLocation = isValidTotal(code, index);
        index += numEndLocation;
        
        for(int i = index-numEndLocation;i<index;i++){ //start from beginning of number to end
            numberString+=code.at(i);
        }
        index++;
        
        if(stringToInt(numberString)==0){
            return true;
        }
        numberString=""; //reinitialize
    }
    return false;
}
