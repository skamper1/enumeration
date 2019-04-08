(* ::Package:: *)

BeginPackage["eaEnumeration`"]

connectionsPossible;
getCircularPermutations;
adjsFromEdgeRules;
cyclesFromAdjs;
getPermsOfAdjList
permuteCycles;
Begin["`Private`"]

Needs["eaDataStructure`",FileNameJoin[{NotebookDirectory[],"eaDataStructure.m"}]];

connectionsPossible[graphData_]:=Module[{numberOfGroups,connectingGroup,mDeg,mEdges,currentDialPosition,localisedBans,possibleDials,newCurrentDialPositionsAndLocalisedBans={},adjLists={},dialsRaw,degree,parts,dials={},i,j,currentDialPositioni, diali,adjListi,localisedBansi,prevLocalisedBansi,localisedBansT},


numberOfGroups=getNumberOfGroups[graphData,1];
connectingGroup=1;
While[connectingGroup<= numberOfGroups&&!getConnectingQ[graphData,1,connectingGroup],
connectingGroup++;
];

If[connectingGroup<=numberOfGroups,

{mDeg,mEdges}=getMDegAndMEdges[graphData];
{currentDialPosition,localisedBans}=getCurrentDialAndLocalisedBans[graphData,1,connectingGroup];
degree=getCurrentDegree[graphData,1,connectingGroup];
parts=getPartsForDial[graphData];
possibleDials=getPossibleDials[graphData];

dialsRaw=dialT[parts,degree,mDeg,mEdges,currentDialPosition,localisedBans,possibleDials];

(*iterate around different currentDialPositions, adding bans where neccessary*)
For[i=1,i<=Length[dialsRaw],i++,
For[j=1,j<=Length[dialsRaw[[i]]],j++,

(*currentDialPosition, dialCode, adjList, localisedBans*)
{currentDialPositioni, diali,adjListi,localisedBansi}=dialsRaw[[i]][[j]];
AppendTo[dials,diali];
AppendTo[adjLists,adjListi];
If[j>1,
If[i>1,
localisedBansT={prevLocalisedBansi},
localisedBansT=Join[localisedBans,{prevLocalisedBansi}];
];
AppendTo[newCurrentDialPositionsAndLocalisedBans,{currentDialPositioni, localisedBansT}],
If[i>1,
AppendTo[newCurrentDialPositionsAndLocalisedBans,{currentDialPositioni,{}}],
AppendTo[newCurrentDialPositionsAndLocalisedBans,{currentDialPositioni,localisedBans}];
];
];
prevLocalisedBansi=localisedBansi;
];
];
];
Return[{dials,newCurrentDialPositionsAndLocalisedBans,adjLists,connectingGroup,currentDialPosition}];
];





dialT[parts0_,degree_,mDeg_,mEdges_,currentDialPositions0_,localisedBans_,possibleDials0_]:=Module[{currentDialPositions=currentDialPositions0,parts=parts0,dials={},banCheckList,dialOutput,i,j,k},
banCheckList=Table[0,{i,1,Length[localisedBans]}];
If[currentDialPositions[[2]]==0,
currentDialPositions[[2]]=1;
];
seenOtherPossibleDialsQ=False;
reachedLimit=False;
maxEdgesLeave=mEdges;
maxDegreeLeave=mDeg;
possibleDials=possibleDials0;
currentDial=Extract[possibleDials,currentDialPositions];
possibleDialsForThis=possibleDials[[currentDialPositions[[1]]]];
listForBans=Table[{pi[[3]],pi[[3]]+pi[[2]]-1},{pi,parts}];
currentDialPosition=currentDialPositions[[2]];
(*calculating reached limit as group number which is static from the start, therefore meaning that the calculation is not done internally*)
(*find the first vertex group that still has to be connected by these vertices*)


i=1;
While[i<Length[parts]&&maxEdgesLeave>=0 && parts[[i]][[1]]<=maxDegreeLeave,
maxEdgesLeave-=parts[[i]][[6]];
i++;
];

j=currentDialPosition;
While[j<Length[possibleDialsForThis]&&parts[[i]][[3]]>possibleDialsForThis[[j]][[1]][[2]],
j++;
];

(*see if clipping is neccessary*)
If[i < Length[parts],
(*now see if there is another possibleDial that has the same start range*)
If[j<Length[possibleDialsForThis],
(*it will be adjacent*)
If[possibleDialsForThis[[j]][[1]]==possibleDialsForThis[[j+1]][[1]],

k=i;


While[k<Length[parts]&&parts[[k+1]][[3]]<=possibleDialsForThis[[j]][[1]][[2]],
k++;
];

reachLimit=k,
(*can clip as only possibleDial that can connect*)

reachLimit=i;
],
(*can clip early as this is the last possibleDial*)

reachLimit=i;
],

(*redundant, clipping is on last vertex group (which is i)*)
reachLimit = i;
];



dials=Join[dials,dialR[parts,degree,localisedBans,banCheckList,0,True,{},1,{},1,1,{}]];

dialOutput=Table[{},{i,currentDialPosition,Length[possibleDialsForThis]}];

For[i=1,i<=Length[dials],i++,
(*currentDialPosition, dialCode, adjList, localisedBans*)
AppendTo[dialOutput[[dials[[i]][[2]]-currentDialPosition+1]],{{currentDialPositions[[1]],dials[[i]][[2]]},dials[[i]][[3]],dials[[i]][[4]],dials[[i]][[5]]}];
];
Return[dialOutput];
];






dialR[parts0_,degree_,localisedBans_,banCheckList_,seenBans_,inCurrentDialQ_,partDial_,i0_,partAdjList_,dialNumber_,possibleDialNumber_,partBan_]:=Module[{i=i0,parts=parts0,dials={},banCheckListT,vN,j,seenBansT,anyBansLeft,possibleDialNumberT},
If[degree>0,

While[i<=Length[parts]&&!(dialNumber==1&&i>reachLimit)(*(!reachedLimit||(seenOtherPossibleDialsQ&&inCurrentDialQ))*),
If[parts[[i]][[4]]>= degree&&parts[[i]][[2]]>0&&(currentDial[[dialNumber]][[1]]<=parts[[i]][[3]]||!inCurrentDialQ),
parts[[i]][[2]]--;
vN=parts[[i]][[3]]++;
parts[[i]][[4]]--;
(*reset possibleDialNumber*)
possibleDialNumberT=possibleDialNumber;
While[possibleDialsForThis[[possibleDialNumberT]][[dialNumber]][[2]]<vN,
possibleDialNumberT++;
];
If[vN<=currentDial[[dialNumber]][[2]]&&inCurrentDialQ,
seenBansT=seenBans;
anyBansLeft=False;
banCheckListT=banCheckList;
j=1;
While[j<=Length[banCheckListT]&&seenBansT==0,
(*see if the dial has already surpassed the ban*)
If[banCheckListT[[j]]==0,
(*see if the dial is now out of order with the ban*)
If[vN<localisedBans[[j]][[dialNumber]][[1]],
banCheckListT[[j]]=-1;
seenBansT=-1,
(*see if the dial has now surpassed the ban*)
If[vN>localisedBans[[j]][[dialNumber]][[2]],
banCheckListT[[j]]=1,
anyBansLeft=True;
];
];
];
j++;
];
If[!anyBansLeft&&seenBansT==0,
seenBansT=1;
];
dials=Join[dials,dialR[parts,degree-1,localisedBans,banCheckListT,seenBansT,inCurrentDialQ,Append[partDial,i],i,Append[partAdjList,vN],dialNumber+1,possibleDialNumberT,Append[partBan,listForBans[[i]]]]],
seenOtherPossibleDialsQ=True;

dials=Join[dials,dialR[parts,degree-1,localisedBans,banCheckList,1,False,Append[partDial,i],i,Append[partAdjList,vN],dialNumber+1,possibleDialNumberT,Append[partBan,listForBans[[i]]]]];

];
parts[[i]][[2]]++;
parts[[i]][[3]]--;
parts[[i]][[4]]++;
];
i++;
];
Return[dials],
If[seenBans<1,

Return[{}];
];
Return[{{inCurrentDialQ,possibleDialNumber,partDial,partAdjList,partBan}}];
];
];




getCircularPermutations[adjList_]:=Module[{perms,min,i,del={},permi,j,permj,k,noMatch},
(*dial must be sorted!*)
If[Length[adjList]>1,
min=Min[adjList];
perms=Permutations[Delete[adjList,Position[adjList,min]]];

For[i=1,i<=Length[perms],i++,
If[First[perms[[i]]]>Last[perms[[i]]],
AppendTo[del,{i}];
];
];
perms=Delete[perms,del];
del={};

Return[Table[Prepend[permsi,min],{permsi,perms}]],
(*else*)
Return[adjList];
];
];
(*the edge rules are effectively adjacencylists for a cycle, or partial cycle*)
(*first you could fill the other vertices in the group with their adjacencies, one loop, with a seen variable which is set to whether or not it has it's own adjacencies*)
adjsFromEdgeRules[graphData_,vtList_]:=Module[{edgeRules,ds1,ds2,shift,seen,adjs,i,vti,edgeRuleVti,vtiSeen,vtiL,vtiR,vtiAdj,vtiLSeen,vtiRSeen,adjL,adjR,pass=True},
edgeRules=getEdgeRules[graphData];
(*quick ref for lengths v1 and v2*)
{ds1,ds2}=getDegreeSequence[graphData];
(*the index shift for group2 vertices*)
shift=Length[ds1];
(*array for tracking "seen" variables*)
seen=Table[-1,{i,1,Length[ds2]}];
adjs=Table[{},{i,1,Length[ds2]}];
(*set all vertices in vtList to 0;*)
For[i=1,i<=Length[vtList],i++,
vti=vtList[[i]];
seen[[vti-shift]]=0;
];
For[i=1,i<=Length[vtList],i++,
vti=vtList[[i]];
edgeRuleVti=edgeRules[[vti-shift]];
vtiSeen=seen[[vti-shift]];
vtiAdj=adjs[[vti-shift]];
(*if there is an adjacency to consider*)
If[Length[edgeRuleVti]>0,
{vtiL,vtiR}=edgeRuleVti;
vtiLSeen=seen[[vtiL-shift]];
vtiRSeen=seen[[vtiR-shift]];
(*if both adjacent vertices are in the vtlist
and (the adjList is currently empty
OR the adjList contains one of the adjacencent vertices
OR the adjList contains both of the adjacent vertices*)
If[(vtiLSeen>=0&&vtiRSeen>=0)&&
(vtiSeen==0||
(vtiSeen==1&&(vtiAdj=={vtiL}||vtiAdj=={vtiR}))||
(vtiSeen==2&&(vtiAdj=={vtiL,vtiR}||vtiAdj=={vtiR,vtiL}))),
seen[[vti-shift]]=2;
adjs[[vti-shift]]={vtiL,vtiR};

adjL=adjs[[vtiL-shift]];
adjR=adjs[[vtiR-shift]];
(*if the left vertex adjlist has space or already contains vti*)
If[vtiLSeen<2||(vtiLSeen==2&&MemberQ[adjL,vti]),
(*if vti needs to be added to the left vertex adjList*)
If[(vtiLSeen==1&&!MemberQ[adjL,vti])||vtiLSeen==0,
AppendTo[adjs[[vtiL-shift]],vti];
seen[[vtiL-shift]]++;
],
(*Fail*)
Return[{False,{}}];
];
If[vtiRSeen<2||(vtiRSeen==2&&MemberQ[adjR,vti]),
If[(vtiRSeen==1&&!MemberQ[adjR,vti])||vtiRSeen==0,
AppendTo[adjs[[vtiR-shift]],vti];
seen[[vtiR-shift]]++;
],
(*Fail*)
Return[{False,{}}];
];
,
(*FAIL*)
Return[{False,{}}];
];
];

];
(*dft adjs*)
Return[{pass,adjs}];
];
(*now we need to dft this list to pull cycles*)
cyclesFromAdjs[graphData_,vtList_,adjs_]:=Module[{ds1,ds2,seen,shift,i,vti,deg,zeroCount=0,oneCount=0,twoCount=0,vstart=0,seenCount=0,cycles={},cycle,vfrom,vto,vtoT,adj},
{ds1,ds2}=getDegreeSequence[graphData];
seen=Table[False,{i,1,Length[ds2]}];
shift=Length[ds1];
For[i=1,i<=Length[vtList],i++,
vti=vtList[[i]];
deg=Length[adjs[[vti-shift]]];
If[deg==0,
If[vstart==0,
vstart=vti;
];
zeroCount++,
If[deg==1,
If[vstart==0,
vstart=vti;
];
oneCount++,
twoCount++;
];
];
];
(*now if there are any vertices with a degree of zero, no vertices of degree one and some of degree 2 there is a fail as this implies a closed ring that doesn't include all vertices*)
If[zeroCount>0&&oneCount==0&&twoCount<0,
Return[{False,{}}];
];
(*if it is a continuous ring including all vertices, set to the first one*)
If[vstart==0,
vstart=vtList[[1]];
];

(*we have the lowest vertex with which to start a cycle*)
While[seenCount< Length[vtList],

vfrom=vstart;

seenCount++;
cycle={vfrom};
seen[[vfrom-shift]]=True;

adj=adjs[[vfrom-shift]];
(*if not a lonely vertex*)
If[Length[adj]>0,
vto=adj[[1]];
While[Length[adjs[[vto-shift]]]>1&&vto!=vstart&&seenCount<Length[vtList],
seenCount++;
seen[[vto-shift]]=True;
AppendTo[cycle,vto];
vtoT=vto;
adj=adjs[[vto-shift]];
If[vfrom==adj[[1]],
vto=adj[[2]],
vto=adj[[1]];
];
vfrom=vtoT;
];
(*now should just need to add last one on, OR Fail if not entire cycle!!*)
If[Length[adjs[[vto-shift]]]==1,
seenCount++;
seen[[vto-shift]]=True;
AppendTo[cycle,vto],
If[seenCount<Length[vtList],
Return[{False,{}}];
];
];
];

AppendTo[cycles,cycle];
If[seenCount<Length[vtList],
i=1;
vstart=vtList[[i]];
(*this needs to first grab any with a one count*)
While[i<=Length[vtList]&&(seen[[vstart-shift]]||Length[adjs[[vstart-shift]]]!=1),
i++;


If[i<=Length[vtList],
vstart=vtList[[i]];
];
];
(*this should get a vertex if there wasn't one with a one count*)
If[i>Length[vtList],
i=1;
vstart=vtList[[i]];
While[seen[[vstart-shift]],
i++;
vstart=vtList[[i]];
];
];
];
];
Return[{True,cycles}];
];


getPermsOfAdjList[graphData_,vtList_]:=Module[{pass,adjs,cycles,l,s,circPerms,binaryPerms,adjPerms,i,j},
{pass,adjs}=adjsFromEdgeRules[graphData,vtList];
If[pass,
{pass,cycles}=cyclesFromAdjs[graphData,vtList,adjs];
If[pass,
adjPerms=permuteCycles[cycles];
];
];
Return[{pass,adjPerms}];
];


permuteCycles[cycles_]:=Module[{l,adjPerms,circPerms,binaryPerms,i,s,j},
l=Length[cycles];
If[l==1,
adjPerms={cycles[[1]]},
If[l==2,
If[Length[cycles[[1]]]>1&&Length[cycles[[2]]]>1,
adjPerms={Flatten[{cycles[[1]],cycles[[2]]}],Flatten[{cycles[[1]],Reverse[cycles[[2]]]}]},
adjPerms={Flatten[{cycles[[1]],cycles[[2]]}]};
],
circPerms=getCircularPermutations[Range[l]];
binaryPerms={cycles};
For[i=1,i<=l,i++,
If[Length[cycles[[i]]]>1,
s=Length[binaryPerms];
For[j=1,j<=s,j++,

AppendTo[binaryPerms,ReplacePart[binaryPerms[[j]],i->Reverse[cycles[[i]]]]];

];
];
];
adjPerms={};
For[i=1,i<=Length[circPerms],i++,
For[j=1,j<=Length[binaryPerms],j++,
AppendTo[adjPerms,Flatten[Table[binaryPerms[[j]][[circPerms[[i]][[k]]]],{k,1,Length[circPerms[[i]]]}]]];
];
];
];
];
Return[adjPerms];
];
End[]
EndPackage[]









