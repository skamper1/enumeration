(* ::Package:: *)

BeginPackage["eaDataStructure`"]
makeGraphData;
addEdges;
getVertexGroup;
makeOldDataStructure;

getVertexGroupFrequency;
getVertexGroupVN;
splitVertexGroup;

getCurrentDialAndLocalisedBans;
setCurrentDialAndLocalisedBans;

getBannedSubgraphs;
setBannedSubgraphs;

getCurrentDegree;
setCurrentDegree;

getAdjacencyList;
setAdjacencyList;

getOriginalDegree;

setConnectingQ;
getConnectingQ;

getMatrixInfo;
setMatrixInfo;

getDegreeSequence;

getNumberOfGroups;
getFaceCounts;
setCycleBooleans;
getCycleBooleans;
getEdgeRules;
getMDegAndMEdges;
getPossibleDials;
getPartsForDial;
reIndexPerms;
getIndexPerms;
getConnectionOrder;


Begin["`Private`"]
(*original degree, current degree, frequency, start vertex, adjacentvertices, adjacent faces, bans, connecting?*)
(*will have to check for symmetry at some point*)
makeGroups[{ds10_,ds20_}]:=Module[{ds1=ds10,ds2=ds20,groups={},group,max,vN=1,freq,pos,count=1},
group={};
While[Length[ds1]>0,
max=Max[ds1];
pos=Position[ds1,max];
freq=Length[pos];
(*the count adds the first part of the current dial position*)
AppendTo[group,{max,max,freq,vN,{},{{count++,0},{}},{},False}];
vN=vN+freq;
ds1=Delete[ds1,pos];
];
AppendTo[groups,group];
group={};
While[Length[ds2]>0,
max=Max[ds2];
pos=Position[ds2,max];
freq=Length[pos];
AppendTo[group,{max,max,freq,vN,{},{{},{}},{},False}];
vN=vN+freq;
ds2=Delete[ds2,pos];
];
AppendTo[groups,group];
Return[groups];
];
makeMatrixInfo[{ds10_,ds20_}]:=Module[{ds1=ds10,ds2=ds20,index,mat1,mat2,lds1,lds2},
(* this is an index which keeps a track of where the vertices are (which matrix etc, for ease in other methods*)
lds1=Length[ds1];
lds2=Length[ds2];
index=Join[Table[{1,i},{i,1,lds1}],Table[{2,i},{i,1,lds2}]];
mat1=Table[Table[0,{i,1,lds1}],{j,1,lds1}];
mat2=Table[Table[0,{i,1,lds2}],{j,1,lds2}];

Return[{index,{mat1,mat2}}];
];




getFaceCounts[graphData_]:=Module[{},
Return[graphData[[3]][[2]]];
];
(*three tables for each degree sequence they will hold the counts of declaredfaces without final vertex, closed faces and total*)
(*total faces is a count used in testPartialMatrices*) 
makeFaceCounts[{ds1_,ds2_}]:=Module[{ds={ds1,ds2},t},
t=Table[Table[Table[0,{k,1,Length[ds[[i]]]}],{j,1,3}],{i,1,2}];
Return[t];
];
setCycleBooleans[graphData0_,cycleBooleans_]:=Module[{graphData=graphData0},
graphData[[3]][[3]]=cycleBooleans;
Return[graphData];
];
getCycleBooleans[graphData_]:=Module[{},
Return[graphData[[3]][[3]]];
];
getEdgeRules[graphData_]:=Module[{},
Return[graphData[[3]][[4]]];
];

getMDegAndMEdges[graphData_]:=Module[{},
Return[graphData[[3]][[5]]];
];

getPossibleDials[graphData_]:=Module[{},
Return[graphData[[3]][[6]]];
];

setIndexPermsL[indexPerms_]:=Module[{},
graphData[[3]][[7]]=indexPerms;
];

getIndexPermsL[]:=Module[{},
Return[graphData[[3]][[7]]];
];

getIndexPerms[graphData_]:=Module[{},
Return[graphData[[3]][[7]]];
];

getConnectionOrder[graphData_]:=Module[{},
Return[graphData[[3]][[8]]];
];


makeCycleBooleans[{ds1_,ds2_}]:=Module[{ds={ds1,ds2},t},
t=Table[False,{k,1,Length[ds[[2]]]}];
Return[t];
];

(*This is a data structure which will hold rules about cycles of edges when their face sharing is nearly closed*)
(*a rule is implemented on a group 2 vertex when it has totalFaceCount = OriginalDegree, and openFaceCounts = 2, only one per vertex*)
makeEdgeRules[{ds1_,ds2_}]:=Module[{ds={ds1,ds2},t},
t=Table[{},{k,1,Length[ds[[2]]]}];
Return[t];
];

makeMDegAndMEdges[]:=Module[{},
Return[{0,0}];
];

makePossibleDials[ds0_]:=Module[{ds1,ds2,g2Nums={},g2Bans={},l1,l2,i,count,g2Deg,vs,g1Deg,list,possibleDialNums={},possibleDials},
{ds1,ds2}=ds0;
l1=Length[ds1];
l2=Length[ds2];
i=1;
count=1;
While[i<=l2,
g2Deg=ds2[[i]];
vs=i+l1;
While[i<=l2&&ds2[[i]]==g2Deg,
AppendTo[g2Nums,count];
i++;
];
AppendTo[g2Bans,{vs,l1+i-1}];
count++;
];

While[Length[ds1]>0,
g1Deg=ds1[[1]];
AppendTo[possibleDialNums,Select[Permutations[g2Nums,{g1Deg}],OrderedQ]];
ds1=DeleteCases[ds1,g1Deg];
];
possibleDials=Table[Table[Table[g2Bans[[cDNijk]],{cDNijk,cDNij}],{cDNij,cDNi}],{cDNi,possibleDialNums}];
Return[possibleDials];
];

makeIndexPerms[{ds1_,ds2_}]:=Module[{i=1,from,count=1, perms={},indices,l1,l2},
l1=Length[ds1];
l2=Length[ds2];
indices=Table[0,{i,1,l2}];
While[i<=l2,
AppendTo[perms,{i+l1}];
from=i;
indices[[i]]=count;
While[i<l2&&ds2[[from]]==ds2[[i+1]],
i++;
indices[[i]]=count;
AppendTo[perms[[count]],i+l1];
];
count++;
i++;
];
Return[{indices,perms}];
];


makeConnectionOrder[{ds1_,ds2_}]:=Module[{group1,group2,sv,freq,i,connectionOrder,count=1,j},
{group1,group2}=makeGroups[{ds1,ds2}];
i=Length[group1];
connectionOrder=Table[0,{i,1,Length[ds1]}];
While[i>0,
sv=group1[[i]][[4]];
freq=group1[[i]][[3]];
For[j=0,j<freq,j++,
connectionOrder[[sv+j]]=count++;
];
i--;
];
Return[connectionOrder];
];


makeExtraData[{ds1_,ds2_}]:=Module[{},
Return[{{ds1,ds2},makeFaceCounts[{ds1,ds2}],makeCycleBooleans[{ds1,ds2}],makeEdgeRules[{ds1,ds2}],makeMDegAndMEdges[],makePossibleDials[{ds1,ds2}],makeIndexPerms[{ds1,ds2}],makeConnectionOrder[{ds1,ds2}],{}}]
];
(*original degree, current degree, frequency, start vertex, adjacentvertices, adjacent faces, bans, connecting?*)
(*will have to check for symmetry at some point*)
makeGraphData[{ds10_,ds20_}]:=Module[{ds1=ds10,ds2=ds20},
graphData={makeGroups[{ds1,ds2}],makeMatrixInfo[{ds1,ds2}],makeExtraData[{ds1,ds2}]};
selectNewVertexGroupL[];
Return[graphData];
];

makeOldDataStructure[graphData0_]:=Module[{oldGraphData},
oldGraphData={graphData0[[1]],graphData0[[2]]};
Return[oldGraphData];
];


currentDegreesOfGroup2L[]:=Module[{currentDegrees,l1,l2,gl2,i,cdi,fri,j,count=0,ds1,ds2},
{ds1,ds2}=getDegreeSequenceL[];
l1=Length[ds1];
l2=Length[ds2];
currentDegrees=Table[0,{i,1,l2}];
gl2=getNumberOfGroupsL[2];
For[i=1,i<=gl2,i++,
cdi=getCurrentDegreeL[2,i];
fri=getVertexGroupFrequencyL[2,i];
For[j=1,j<=fri,j++,
currentDegrees[[j+count]]=cdi;
];
count+=fri;
];
Return[currentDegrees];
];


reIndexPermsL[]:=Module[{},
graphData=reIndexPerms[graphData];
];


reIndexPerms[graphData0_]:=Module[{indices,perms,group2CurrentDegrees,i,j,l1,l2,currentDegreeT,jstart,count=0,jchanged,vnij,currentDegreeTij,permsNew={},indicesNew,vnT,ds1,ds2},
graphData=graphData0;
{ds1,ds2}=getDegreeSequenceL[];
{indices,perms}=getIndexPermsL[];
group2CurrentDegrees=currentDegreesOfGroup2L[];
l1=Length[ds1];
l2=Length[ds2];
indicesNew=Table[0,{i,1,l2}];
For[i=1,i<=Length[perms],i++,
jstart=1;
jchanged=True;
While[jstart<=Length[perms[[i]]]&&jchanged,
jchanged=False;
vnT=perms[[i]][[jstart]];
currentDegreeT=group2CurrentDegrees[[vnT-l1]];
count++;
AppendTo[permsNew,{vnT}];
indicesNew[[vnT-l1]]=count;
For[j=jstart+1,j<=Length[perms[[i]]],j++,
vnij=perms[[i]][[j]];
currentDegreeTij=group2CurrentDegrees[[vnij-l1]];
If[currentDegreeT==currentDegreeTij,
AppendTo[permsNew[[count]],vnij];
indicesNew[[vnij-l1]]=count,
If[!jchanged&&indicesNew[[vnij-l1]]==0,
jchanged=True;
jstart=j;
];
];
];
];
];
setIndexPermsL[{indicesNew,permsNew}];
Return[graphData];
];






(*this method gives an ordered adjacency list for a vertex and returns a boolean pass or fail with the matrix information*)
addEdges[graphData0_,vf0_,vtList0_,connectingGroup_,dial0_,currentDialAndLocalisedBans_]:=Module[{matrixInfo,vf=vf0,vtList=vtList0,pass=True,i},
(*flattening data by making graphData local*)
graphData=graphData0;
i=1;
While[i<=Length[vtList]&&pass,
pass=addEdgeL[vf,vtList[[i]],vtList[[Mod[i,Length[vtList]]+1]]];
i=i+1;
];
If[pass,
makeNewTemplateL[connectingGroup,dial0,currentDialAndLocalisedBans];
];
Return[{pass,graphData}];
];


addEdgeL[vf0_,vt10_,vt20_]:=Module[{vf=vf0,vt1=vt10,vt2=vt20,val1,val2,val3,val4,pass=True,pass1,pass2},
val1=getMatValL[vt1,vt2];
val2=getMatValL[vt2,vt1];
If[val1==0||val2==0,
(*there is space and the entry hasn't been added*)
If[val1==0,
setMatValL[vt1,vt2,vf];
val1=vf,
(*else val1 has an entry already*)
setMatValL[vt2,vt1,vf];
val2=vf;
];

(*val2 \[NotEqual] 0 means that there is a closed face, otherwise it is an open face*)
(*if val2 \[NotEqual] 0 then we need to verify the faces*)
If[val2!=0,
val3=getMatValL[val1,val2];
val4=getMatValL[val2,val1];

switchFromOpenToClosedFaceCountL[vt1];
switchFromOpenToClosedFaceCountL[vt2];

(*now check they are either empty or already hold the correct values*)
If[((val3==0||val3==vt1)&&(val4==0||val4==vt2))||((val4==0||val4==vt1)&&(val3==0||val3==vt2)),
matInfo=setMatValL[val1,val2,vt1];
matInfo=setMatValL[val2,val1,vt2];
pass1=newClosedFaceCountL[val1];
pass2=newClosedFaceCountL[val2];
If[!(pass1&&pass2),
Return[False];
];
,
(*else*)
pass=False;
],
(*one new open face for vt1 and vt2*)
pass1=newOpenFaceCountL[vt1];
pass2=newOpenFaceCountL[vt2];
If[!(pass1&&pass2),
Return[False];
];
],
(*else entry not in and no space*)
pass=False;
];
testForEdgeRuleL[vt1];
testForEdgeRuleL[vt2];
Return[pass];
];

(*************localmethods for addEdges*)
(*finding the two open vertices that share faces with vn and associating a rule*)
setEdgeRuleL[vn_]:=Module[{in1,in2,v1=0,v2=0,i,l1,l2,val1,val2},
{in1,in2}=graphData[[2]][[1]][[vn]];
(*l1 is the length of mat 1 (the number of vertices in group 1*)
l1=Length[graphData[[3]][[1]][[1]]];
(*l2 is the length of mat 2*)
l2=Length[graphData[[3]][[1]][[2]]];
i=1;
While[i<=l2&&v2==0,
If[i!=in2,
val1=graphData[[2]][[2]][[2]][[i]][[in2]];
val2=graphData[[2]][[2]][[2]][[in2]][[i]];
If[(val1==0&&val2!=0)||(val1!=0&&val2==0),
If[v1==0,
v1=i+l1,
v2=i+l1];
];
];
i++;
];
graphData[[3]][[4]][[in2]]={v1,v2};
];

testForEdgeRuleL[vn_]:=Module[{in1,in2,totalFaceCount,d,openFaceCount},
{in1,in2}=graphData[[2]][[1]][[vn]];
openFaceCount = graphData[[3]][[2]][[in1]][[1]][[in2]];
totalFaceCount = graphData[[3]][[2]][[in1]][[3]][[in2]];
d=graphData[[3]][[1]][[in1]][[in2]];
If[totalFaceCount==d && openFaceCount==2,
setEdgeRuleL[vn];
];
];

getMatValL[i_,j_]:=Module[{indexi,indexj},
indexi=graphData[[2]][[1]][[i]];
indexj=graphData[[2]][[1]][[j]];
Return[graphData[[2]][[2]][[indexi[[1]]]][[indexi[[2]]]][[indexj[[2]]]]];
];
setMatValL[i_,j_,val_]:=Module[{indexi,indexj},
indexi=graphData[[2]][[1]][[i]];
indexj=graphData[[2]][[1]][[j]];
graphData[[2]][[2]][[indexi[[1]]]][[indexi[[2]]]][[indexj[[2]]]]=val;
];

incrementOpenFaceCountL[vn_]:=Module[{in1,in2},
{in1,in2}=graphData[[2]][[1]][[vn]];
graphData[[3]][[2]][[in1]][[1]][[in2]]++;
];

decrementOpenFaceCountL[vn_]:=Module[{in1,in2},
{in1,in2}=graphData[[2]][[1]][[vn]];
graphData[[3]][[2]][[in1]][[1]][[in2]]--;
];


incrementClosedFaceCountL[vn_]:=Module[{in1,in2},
{in1,in2}=graphData[[2]][[1]][[vn]];
graphData[[3]][[2]][[in1]][[2]][[in2]]++;
];

incrementTotalFaceCountL[vn_]:=Module[{in1,in2,count,d},
{in1,in2}=graphData[[2]][[1]][[vn]];
count=++graphData[[3]][[2]][[in1]][[3]][[in2]];
d=graphData[[3]][[1]][[in1]][[in2]];
(*boolean return on testing number of faces*)
Return[d>=count];
];

switchFromOpenToClosedFaceCountL[vn_]:=Module[{},
incrementClosedFaceCountL[vn];
decrementOpenFaceCountL[vn];
];

newOpenFaceCountL[vn_]:=Module[{},
incrementOpenFaceCountL[vn];
Return[incrementTotalFaceCountL[vn]];
];

newClosedFaceCountL[vn_]:=Module[{},
incrementClosedFaceCountL[vn];
Return[incrementTotalFaceCountL[vn]];
];


getDegreeSequence[graphData_]:=Module[{},
Return[graphData[[3]][[1]]];
];
getDegreeSequenceL[]:=Module[{},
Return[graphData[[3]][[1]]];
];


getVertexGroup[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]]];
];

(*vertexGroup getter setter methods*)
(*setVertexGroupFrequency AND setVertexGroupVN are internal methods*)
getVertexGroupFrequency[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[3]]];
];
getVertexGroupVN[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[4]]];
];
splitVertexGroup[graphData0_,groupsNumber_,vertexGroupNumber_,frequency_]:=Module[{graphData=graphData0,vertexGroup,oldFrequency,vN},
vertexGroup=getVertexGroup[graphData,groupsNumber,vertexGroupNumber];
oldFrequency=getVertexGroupFrequency[graphData,groupsNumber,vertexGroupNumber];
graphData=insertNewVertexGroup[graphData,vertexGroup,groupsNumber,vertexGroupNumber+1];
graphData=setVertexGroupFrequency[graphData,groupsNumber,vertexGroupNumber,frequency];
graphData=setVertexGroupFrequency[graphData,groupsNumber,vertexGroupNumber+1,oldFrequency-frequency];
vN=getVertexGroupVN[graphData,groupsNumber,vertexGroupNumber];
graphData=setVertexGroupVN[graphData,groupsNumber,vertexGroupNumber+1,vN+frequency];
Return[graphData];
];
setVertexGroupFrequency[graphData0_,groupsNumber_,vertexGroupNumber_,frequency_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[3]]=frequency;
Return[graphData];
];
insertNewVertexGroup[graphData_,vertexGroup_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[Insert[graphData,vertexGroup,{1,groupsNumber,vertexGroupNumber}]];
];
setVertexGroupVN[graphData0_,groupsNumber_,vertexGroupNumber_,vN_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[4]]=vN;
Return[graphData];
];




getCurrentDialAndLocalisedBans[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[6]]];
];
setCurrentDialAndLocalisedBans[graphData0_,groupsNumber_,vertexGroupNumber_,currentDialAndLocalisedBans_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[6]]=currentDialAndLocalisedBans;
Return[graphData];
];

getBannedSubgraphs[graphData_]:=Module[{},
Return[graphData[[3]][[9]]];
];
setBannedSubgraphs[graphData0_,bannedSubgraphs_]:=Module[{graphData=graphData0},
graphData[[3]][[9]]=bannedSubgraphs;
Return[graphData];
];


getCurrentDegree[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[2]]];
];
setCurrentDegree[graphData0_,groupsNumber_,vertexGroupNumber_,currentDegree_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[2]]=currentDegree;
Return[graphData];
];

getAdjacencyList[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[5]]];
];
setAdjacencyList[graphData0_,groupsNumber_,vertexGroupNumber_,adjacentVertices_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[5]]=adjacentVertices;
Return[graphData];
];

getOriginalDegree[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[1]]];
];

getConnectingQ[graphData_,groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[8]]];
];
setConnectingQ[graphData0_,groupsNumber_,vertexGroupNumber_,connectingQ_]:=Module[{graphData=graphData0},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[8]]=connectingQ;
Return[graphData];
];

getMatrixInfo[graphData_]:=Module[{},
Return[graphData[[2]]];
];
setMatrixInfo[graphData0_,matrixInfo_]:=Module[{graphData=graphData0},
graphData[[2]]=matrixInfo;
Return[graphData];
];

getNumberOfGroups[graphData_,groupsNumber_]:=Module[{},
Return[Length[graphData[[1]][[groupsNumber]]]];
];

getPartsForDial[graphData0_]:=Module[{numberOfGroups,i,parts,cDi,sVi,freqi},
graphData=graphData0;
numberOfGroups=getNumberOfGroups[graphData,2];
i=numberOfGroups;
(*currentDegree,startVertex,frequency,edges,reverseAggregateFrequency,reverseAggregateEdges*)
parts=Table[{0,0,0,0,0,0},{j,1,i}];
While[i>0,
{cDi,sVi,freqi}=
{getCurrentDegreeL[2,i],
getVertexGroupVNL[2,i],
getVertexGroupFrequencyL[2,i]};
If[cDi==0,
freqi=0;
];
If[i<numberOfGroups,
parts[[i]]={cDi,freqi,sVi,freqi+parts[[i+1]][[4]],cDi*freqi+parts[[i+1]][[5]],cDi*freqi},
parts[[i]]={cDi,freqi,sVi,freqi,cDi*freqi,cDi*freqi};
];
i--;
];
Return[parts];
];


getNumberOfGroupsL[groupsNumber_]:=Module[{},
Return[Length[graphData[[1]][[groupsNumber]]]];
];

selectNewVertexGroupL[]:=Module[{i=getNumberOfGroupsL[1],mDeg=0,mEdges=0,freqi,degi,currentDialPosition},
While[i>0&&getCurrentDegreeL[1,i]==0,
i--;
];
If[i>0,
setConnectingQL[1,i,True];
(*keeping the currentDialPosition set at 0 here now*)
(*
{currentDialPosition,null}=getCurrentDialAndLocalisedBansL[1,i];
setCurrentDialAndLocalisedBansL[1,i,{{currentDialPosition[[1]],1},{}}];*)
];


i--;

While[i>0,
freqi=getVertexGroupFrequencyL[1,i];
degi=getCurrentDegreeL[1,i];
mDeg+=freqi;
mEdges+=freqi*degi;
i--;
];
setMDegAndMEdgesL[mDeg,mEdges];

];

setMDegAndMEdgesL[mDeg_,mEdges_]:=Module[{},
graphData[[3]][[5]]={mDeg,mEdges};
];

makeNewTemplateL[connectingGroup_,dial0_,currentDialAndLocalisedBans_]:=Module[{dial=dial0,max,pos,count,g2AdjacencyList,g1AdjacencyList,currentDegree,cv,g2VN,g2Frequency,vtList,i},
If[getVertexGroupFrequencyL[1,connectingGroup]>1,
splitVertexGroupL[1,connectingGroup,1];
setCurrentDegreeL[1,connectingGroup,0];
setConnectingQL[1,connectingGroup,False];
setCurrentDialAndLocalisedBansL[1,connectingGroup+1,currentDialAndLocalisedBans],


setCurrentDegreeL[1,connectingGroup,0];
setConnectingQL[1,connectingGroup,False];
(*select new vertex Group*)
selectNewVertexGroupL[];

];

setCurrentDialAndLocalisedBansL[1,connectingGroup,currentDialAndLocalisedBans[[1]]];

cv=getVertexGroupVNL[1,connectingGroup];

(*connect from dials backwards to avoid problems with splitting groups*)
While[Length[dial]>0,
(*Get maximum dial number*)
max=Max[dial];
(*get positions for deletion and number of them (count)*)
pos=Position[dial,max];
count=Length[pos];

If[getVertexGroupFrequencyL[2,max]>count,
splitVertexGroupL[2,max,count];
];

currentDegree=getCurrentDegreeL[2,max];
(*could have decrementCurrentDegree*)
setCurrentDegreeL[2,max,currentDegree-1];

g2AdjacencyList=getAdjacencyListL[2,max];
(*could have appendAdjacencyList*)
setAdjacencyListL[2,max,Append[g2AdjacencyList,cv]];

g2VN=getVertexGroupVNL[2,max];
g2Frequency=getVertexGroupFrequencyL[2,max];

g1AdjacencyList=getAdjacencyListL[1,connectingGroup];
setAdjacencyListL[1,connectingGroup,Join[Range[g2VN,g2VN+g2Frequency-1],g1AdjacencyList]];

dial=Delete[dial,pos];

(*changing so that if the vertex has degree > 3 all the vertices are split into separate groups, some stuff can be done with 4's but not yet implemented*)
(*if current vertex original vertex degree < 4 (i.e. 3)*)
If[getOriginalDegreeL[1,connectingGroup]>3,
(*split all vertices into separate groups*)
For[i=1,i<g2Frequency,i++,
splitVertexGroupL[2,max,g2Frequency-i];
];
];

];

(*currently setting the connect to false here*)
setConnectingQL[1,connectingGroup,False];

];
(***********************************LOCAL methods*)

getVertexGroupL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]]];
];

(*vertexGroup getter setter methods*)
(*setVertexGroupFrequency AND setVertexGroupVN are internal methods*)
getVertexGroupFrequencyL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[3]]];
];
getVertexGroupVNL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[4]]];
];
splitVertexGroupL[groupsNumber_,vertexGroupNumber_,frequency_]:=Module[{vertexGroup,oldFrequency,vN},
vertexGroup=getVertexGroupL[groupsNumber,vertexGroupNumber];
oldFrequency=getVertexGroupFrequencyL[groupsNumber,vertexGroupNumber];
insertNewVertexGroupL[vertexGroup,groupsNumber,vertexGroupNumber+1];
setVertexGroupFrequencyL[groupsNumber,vertexGroupNumber,frequency];
setVertexGroupFrequencyL[groupsNumber,vertexGroupNumber+1,oldFrequency-frequency];
vN=getVertexGroupVNL[groupsNumber,vertexGroupNumber];
setVertexGroupVNL[groupsNumber,vertexGroupNumber+1,vN+frequency];
];

setVertexGroupFrequencyL[groupsNumber_,vertexGroupNumber_,frequency_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[3]]=frequency;
];
insertNewVertexGroupL[vertexGroup_,groupsNumber_,vertexGroupNumber_]:=Module[{},
graphData=Insert[graphData,vertexGroup,{1,groupsNumber,vertexGroupNumber}];
];
setVertexGroupVNL[groupsNumber_,vertexGroupNumber_,vN_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[4]]=vN;
];


getCurrentDialAndLocalisedBansL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[6]]];
];
setCurrentDialAndLocalisedBansL[groupsNumber_,vertexGroupNumber_,currentDialAndLocalisedBans_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[6]]=currentDialAndLocalisedBans;
];

getBannedSubgraphsL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[7]]];
];
getBannedSubgraphsL[groupsNumber_,vertexGroupNumber_,bannedSubgraphs_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[7]]=bannedSubgraphs;
];

getCurrentDegreeL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[2]]];
];
setCurrentDegreeL[groupsNumber_,vertexGroupNumber_,currentDegree_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[2]]=currentDegree;
];

getAdjacencyListL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[5]]];
];
setAdjacencyListL[groupsNumber_,vertexGroupNumber_,adjacentVertices_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[5]]=adjacentVertices;
];

getOriginalDegreeL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[1]]];
];

getConnectingQL[groupsNumber_,vertexGroupNumber_]:=Module[{},
Return[graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[8]]];
];
setConnectingQL[groupsNumber_,vertexGroupNumber_,connectingQ_]:=Module[{},
graphData[[1]][[groupsNumber]][[vertexGroupNumber]][[8]]=connectingQ;
];

getMatrixInfoL[]:=Module[{},
Return[graphData[[2]]];
];
setMatrixInfoL[matrixInfo_]:=Module[{},
graphData[[2]]=matrixInfo;
];




End[]
EndPackage[]


