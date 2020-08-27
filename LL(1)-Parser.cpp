#include "iostream"
#include "fstream"
#include "string"
#include "vector"
#include "map"
#include "unordered_map"
#include "iomanip"
#include "set"

using namespace std;

#define STACK_MAX 250

template <class T>
class Stack{
    T *data;
    int p;
    const int SIZE;
public:
    Stack(int s): SIZE(s),p(0){
        data=new T[SIZE];
    }
    void push(T d){
        if (p==SIZE)    throw(0);
        data[p++]=d;
    }
    T pop() {
        if (p==0)    throw(1);
        return data[--p];
    }
    T seeTop() const{
        if (p==0)    throw(1);
        return data[p-1];
    }
    bool isFull() const{
        return p==SIZE;
    }
    bool isEmpty() const{
        return p==0;
    }
	void clear(){
		p=0;
	}
	void print(){
		for(int i=0;i<p;i++)
			cout<<data[i]<<'\t';
		cout<<endl;
	}
    ~Stack(){   delete []data;}
};
class setElement{
public:
	string element,production;
	bool operator<(const setElement &r) const {
		return element<r.element;
	}
};
struct CFG{
	unordered_map<string,bool> terminals,nonTerminals;
	unordered_map<string,vector<string>> productions;
	unordered_map<string,set<string>> follow;
	unordered_map<string,set<setElement>> first;
	string startingTerminal;
};
string clean(string production){
	if(production.length()>4 && production[0]=='\\' && production[1]=='\"' && production[production.length()-2]=='\\' &&production[production.length()-1]=='\"')
		return production.substr(2,production.length()-4);
	else 
		return production;
}
bool mergeSets(set<string> &set1,set<setElement> &set2){
	bool containEpsilon = false;
	for(setElement s:set2)
		if(s.element != "\"\""){
			set1.insert(s.element);
		}
		else containEpsilon = true;
		return containEpsilon;
}
bool mergeSets(set<string> &set1,set<string> &set2){
	bool containEpsilon = false;
	for(string s:set2)
		if(s != "\"\""){
			set1.insert(s);
		}
		else containEpsilon = true;	
		return containEpsilon;
}
bool mergeSets(set<setElement> &set1,set<setElement> set2,string production){
	bool containEpsilon = false;
	for(setElement s:set2)
		if(s.element != "\"\""){
			s.production=production;
			set1.insert(s);
		}
		else
			containEpsilon = true;
	return containEpsilon;
}
map<string,string> breakRecursion;
set<setElement> getFirstSet(CFG &g,string nonTerminal,int index){
	set<setElement> firstSet1;
	string temp="";
	int epsilonCount=0;
	bool isComplete=true;
	for(unsigned int i=0;i<g.productions[nonTerminal].size();++i){
		for(unsigned int j=0;j<g.productions[nonTerminal][i].length();++j){
			temp+=g.productions[nonTerminal][i][j];
			if(temp=="\"" || temp=="\\")		continue;
			if(temp == "\\\""){
				temp="";
				isComplete=false;
			}
			if(temp.length()>=2 && !isComplete && temp[temp.length()-2] == '\\' && temp[temp.length()-1] == '\"'){
				temp=temp.substr(0,temp.length()-2);
				isComplete=true;
			}
			if(isComplete){
				if(g.terminals.find(temp) != g.terminals.end()){
					//firstSet.insert(temp);
					setElement SE;
					SE.element=temp;
					SE.production=g.productions[nonTerminal][i];
					firstSet1.insert(SE);
					temp="";
					break;
				}else{
					bool result=false;
					if(g.first.find(temp) == g.first.end()){
						set<setElement> s = getFirstSet(g,temp,0);

						result = mergeSets(firstSet1,s,g.productions[nonTerminal][i]);

						g.first[temp] = s;

					}else{
						result=mergeSets(firstSet1,g.first[temp],g.productions[nonTerminal][i]);
						temp="";
					}
					if(result)
						epsilonCount++;
					else
						break;
					temp="";
				}
			}
		}
		if(epsilonCount == g.productions[nonTerminal][i].length()){
				setElement SE;
				SE.element="\"\"";
				SE.production=g.productions[nonTerminal][i];
				firstSet1.insert(SE);
		}
		epsilonCount = 0;
	}
	cout<<"FIRST("<<nonTerminal<<")={";
	for(setElement x: firstSet1)
		//<<"\t{ "<<clean(x.production)<<" }"
		cout<<' '<<x.element<<", ";
	cout<<"\b\b"<<'}'<<endl;
	return firstSet1;
}
void splitProduction(vector<string> &s,string production){
	string temp="";
	bool isComplete=true;
	for(unsigned int i=0;i<production.length();i++){
		temp+=production[i];
		if(production[i]=='\\' && isComplete)
			continue;
		else if(temp=="\\\""){
			isComplete=false;
			temp="";
		}else if(temp=="\"")
			continue;
		if(temp.length()>=2 && !isComplete && temp[temp.length()-2] == '\\' && temp[temp.length()-1] == '\"'){
			temp=temp.substr(0,temp.length()-2);
			isComplete=true;
		}
		if(isComplete){
			s.push_back(temp);
			temp="";
		}
	}
}
bool canEmptyStack(Stack<string> &check,vector<vector<string>> &parseTable,CFG &g,map<string,int> &tableMap){
	while (!check.isEmpty()){
		string temp=check.pop();
		if(g.terminals.find(temp) != g.terminals.end())
			return false;
		else{
			if(parseTable[tableMap[temp]][parseTable[0].size()-1] !="\"\"")
				return false;
		}
	}
	return true;
}
set<string> getFollowSet(CFG &g,string nonTerminal){
	set<string> followSet;
	if(nonTerminal==g.startingTerminal)
		followSet.insert("$");
	bool containsEpsilon=false;
	for(auto it = g.productions.begin(); it!=g.productions.end();it++){
		for(unsigned int i=0;i<it->second.size();i++){
			vector<string> s;
			splitProduction(s,it->second[i]);
			containsEpsilon=false;
			for(unsigned int j=0;j<s.size();j++){
				if(s[j] == nonTerminal || containsEpsilon){
					if(j==s.size()-1 && s[j] == nonTerminal && nonTerminal != it->first){//right most position
						if(breakRecursion[nonTerminal]==it->first){
							break;
						}
						if(g.follow.find(it->first) == g.follow.end()){
							breakRecursion[it->first] = nonTerminal;
							g.follow[it->first] = getFollowSet(g,it->first);
						}
						containsEpsilon = mergeSets(followSet,g.follow[it->first]);
						break;
					}else if(j==s.size()-1 && s[j] == nonTerminal && nonTerminal == it->first)
						break;
					if(!containsEpsilon)
						j++;
					if(g.terminals.find(s[j]) != g.terminals.end() && s[j] != "\"\""){//terminal
						followSet.insert(s[j]);
						break;
					}else{                                                            //non-terminal
						containsEpsilon = mergeSets(followSet,g.first[s[j]]);
						if(!containsEpsilon)
							break;
						if(containsEpsilon && j==s.size()-1 && nonTerminal != it->first){
							if(g.follow.find(it->first) == g.follow.end())
								g.follow[it->first] = getFollowSet(g,it->first);	
							containsEpsilon=mergeSets(followSet,g.follow[it->first]);
						}
					}
				}
			}
		}
	}
	cout<<"FOLLOW("<<nonTerminal<<")={";
	for(string s:followSet)
		cout<<s<<", ";
	if(followSet.empty())
		cout<<"}\n";
	else
		cout<<"\b\b}\n";
	return followSet;
}
bool splitInput(vector<string> &inputSplited,const string &inputString,const CFG &g){
	string temp="";
	for(unsigned int i=0;i<inputString.length();i++){
		temp+=inputString[i];
		if(g.terminals.find(temp) != g.terminals.end()){
			inputSplited.push_back(temp);
			temp="";
		}
	}
	if(temp!="")
		return false;
	return true;
}
int main(){
	ifstream input("cfg.txt");
	string str,temp="";
	CFG g;
	bool isComplete=true;
	input>>str;
	for(unsigned int i=0;i<str.length();i++){
		temp+=str[i];
		if(str[i]=='\\' && isComplete)
			continue;
		else if(temp=="\\\""){
			isComplete=false;
			temp="";
		}
		if(temp.length()>=2 && !isComplete && temp[temp.length()-2] == '\\' && temp[temp.length()-1] == '\"'){
			temp=temp.substr(0,temp.length()-2);
			isComplete=true;
		}
		if(isComplete){
			g.terminals[temp]=true;
			temp="";
		}	
	}
	temp="";
	input>>str;
	g.startingTerminal = temp+str[0];
	for(unsigned int i=0;i<str.length();i++){
		temp+=str[i];
		g.nonTerminals[temp]=true;
		temp="";
	}
	g.terminals["\"\""]=true;
	isComplete=true;
	while(input>>str){
		string production;
		input>>production;
		g.productions[str].push_back(production);
	}
	for(auto it = g.productions.begin(); it!=g.productions.end();it++){
		cout << it->first<<"  ->  ";
		for(unsigned int i=0;i<it->second.size();i++)
			cout<<clean(it->second[i])<<"\t |";
		cout<<endl;
	}
	for(auto it = g.nonTerminals.begin(); it != g.nonTerminals.end();it++)
		if(g.first.find(it->first) == g.first.end())
			g.first[it->first] = getFirstSet(g,it->first,0);
	cout<<"\n\n";
	g.follow[g.startingTerminal] = getFollowSet(g,g.startingTerminal);
	for(auto it = g.nonTerminals.begin(); it != g.nonTerminals.end();it++)
		if(it->first!="\"\"")
			g.follow[it->first] = getFollowSet(g,it->first);
	vector<vector<string>> parseTable(g.nonTerminals.size(),vector<string>(g.terminals.size(),"^"));
	map<string,int> tableMap;
	int ij=0;
	for(auto it = g.nonTerminals.begin(); it != g.nonTerminals.end();it++){
		tableMap[it->first] = ij;
		ij++;
	}
	ij=0;
	for(auto it = g.terminals.begin(); it != g.terminals.end();it++){
		if(it->first!="\"\""){
			tableMap[it->first] = ij;
			ij++;
		}
	}
	tableMap["$"]=g.terminals.size()-1;
	for(auto it = g.nonTerminals.begin(); it != g.nonTerminals.end();it++){
		set<setElement> firstSet = g.first[it->first];
		for(setElement set:firstSet)
			if(set.element != "\"\""){
				parseTable[tableMap[it->first]][tableMap[set.element]]=clean(set.production);
			}
			else
				for(string follows:g.follow[it->first]){
					parseTable[tableMap[it->first]][tableMap[follows]]="\"\"";
				}
	}
	cout<<"\n\n\t  ";
	for(auto it = g.terminals.begin(); it != g.terminals.end();it++)
		if(it->first!="\"\"")
			cout<<it->first<<"\t\t";
	cout<<"$"<<endl<<"-------------------------------------------------------------------------------------------------\n";
	auto itt = g.nonTerminals.begin();
	for(unsigned int i=0;i<parseTable.size();i++){
		cout<<itt->first<<"\t | ";
		for(unsigned int j=0;j<parseTable[i].size();j++)
			cout<<parseTable[i][j]<<"\t\t";
		cout<<endl;
		itt++;
	}
	cout<<"-------------------------------------------------------------------------------------------------\n";
	input.close();
	input.open("inputStr.txt");
	Stack<string> check(STACK_MAX);
	string inputString;
	while(input>>inputString){
		cout<<inputString<<endl;
		vector<string> inputSplited;
		bool result=splitInput(inputSplited,inputString,g);
		check.clear();
		check.push("$");
		bool isRejected=false;
		check.push(g.startingTerminal);
		int pointer=0;
		if(result){
			while(1){
				if(pointer==inputSplited.size()){
					string s=check.pop();
					while (s!="$"){
						string pr=parseTable[tableMap[s]][g.terminals.size()-1];
						if(pr!="\"\"" || pr!="^"){
							cout<<"Input rejected!\n";
							isRejected=true;
							break;
						}
						s=check.pop();
					}
					if(!isRejected)
						cout<<"Input accepted!\n";
					break;
				}
				string tempP=check.pop();
				if(g.nonTerminals.find(tempP) != g.nonTerminals.end()){
					vector<string> pProductions;
					string pr=parseTable[tableMap[tempP]][tableMap[inputSplited[pointer]]];
					if(pr!="\"\"" && pr!="^"){
						if(g.terminals.find(pr) != g.terminals.end()){
							if(pr == inputSplited[pointer]){
								pointer++;
								continue;
							}else{
								cout<<"Input rejected!\n";
								break;
							}
						}else {
							splitProduction(pProductions,pr);
							for(int rev=pProductions.size()-1;rev>=0;rev--)
								check.push( pProductions[rev] );
							check.print();
						}
					}else if (pr=="^"){
						cout<<"Input rejected!\n";
						break;
					}
					else 
						continue;
				}else if(temp=="$"){//$
					if(pointer==inputString.length() && canEmptyStack(check,parseTable,g,tableMap))
						cout<<"Input accepted!\n";
					else
						cout<<"Input rejected!\n";
					break;
				} else{//other terminal
					if(tempP==inputSplited[pointer])
						pointer++;
					else{
						cout<<"Input rejected!\n";
						break;
					}
				}
			}
		}else
			cout<<"Input rejected!\n";
	}
	return 0;
}