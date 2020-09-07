//
//  tax.cpp
//  Project 2
//
//  Created by Junho Choi on 4/10/20.
//  Copyright © 2020 Junho Choi. All rights reserved.
//
#include <iostream>
#include <string>
using namespace std;

int main(){
    int error = 0; //Tracks user errors
    
    // Collect user information
    cout << "Name: ";
    string personsName;
    getline(cin, personsName);
    if(personsName == "")
        error = 1;
    
    cout << "Taxable income: ";
    double income;
    cin >> income;
    cin.ignore(10000, '\n');
    if(income<0 && personsName !="")
        error = 2;
    
    cout << "Occupation: ";
    string occupation;
    getline(cin, occupation);
    if(occupation == "" && income>0 && personsName !="")
        error = 3;
    
    cout << "Number of children: ";
    int childCount;
    cin >> childCount;
    if(childCount<0 && occupation != "" && income>0 && personsName !="")
        error = 4;
    
    
    //Determine if occupation tax discount is available
    double occupationTax = 0.06;
    if(occupation == "engineer" || occupation == "scientist") //if engineer or scientist, special tax discount
        occupationTax =0.05;
    
    
    //Calculate tax rate without adjusting for number of children
    double initTaxedIncome = 0.0; //income before taking into account childCount
    if (income <= 50000)
        initTaxedIncome = (income*0.04);
    else if(income <= 120000)
        initTaxedIncome = (50000*0.04)+((income-50000)*occupationTax);
    else
        initTaxedIncome = (50000*0.04)+(70000*occupationTax)+((income-120000)*0.09);
    
    
    //Calculate tax rate taking into account number of children
    double adjustedTaxedIncome; //Adjusted tax rate taking into account number of children
    if(income<120000) //If they make less than 120000, tax reduced depending on childCount
        adjustedTaxedIncome = initTaxedIncome-200*childCount;
    else
        adjustedTaxedIncome = initTaxedIncome;

    if(adjustedTaxedIncome<0) //If adjusted tax is below 0, set to 0.
        adjustedTaxedIncome = 0;
    
    //Dashed line
    cout << "---" << endl;
    
    //Print message
    cout.setf(ios::fixed); //Set to 2 decimal places
    cout.precision(2);
    if(error ==1)
        cout<<"You must enter a name" << endl;
    else if(error ==2)
        cout<<"The taxable income must be nonnegative" << endl;
    else if(error ==3)
        cout<<"You must enter an occupation" << endl;
    else if(error ==4)
        cout<<"The number of children must be nonnegative" << endl;
    else //If no error, then print
        cout << personsName << " would pay $" << adjustedTaxedIncome << endl;
}
