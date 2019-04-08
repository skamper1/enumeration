(* ::Package:: *)

BeginPackage["ea18`"]

makeDir;
makeGraphs;
exportOutput;
exportTreeGraph;
dialCount;
connectCircCount;
totalDialTime;
totalConnectTime;
totalTestTime;
testCount;
totalCircTime;
totalMakeTemplateTime;
totalAddEdgeTime;
connectVertex;


getPairs;

Begin["`Private`"]
Needs["eaDataStructure`",FileNameJoin[{NotebookDirectory[],"eaDataStructure.m"}]]
Needs["eaTesting`",FileNameJoin[{NotebookDirectory[],"eaTesting.m"}]]
Needs["eaEnumeration`",FileNameJoin[{NotebookDirectory[],"eaEnumeration.m"}]]
Needs["eaInit`",FileNameJoin[{NotebookDirectory[],"eaInit.m"}]]
Needs["subgraphTemp`",FileNameJoin[{NotebookDirectory[],"subgraphTemp.m"}]]









connectVertex[graphData0_,connectingGroup_,dial0_,currentDialAndLocalisedBans0_,adjList_]:=
Module[{

matrixInfo,
vtList,vtLists,i,pass,graphs={},graphDataT,cv},



cv=getVertexGroupVN[graphData0,1,connectingGroup];
vtList=adjList;
{circTime,null}=AbsoluteTiming[
{pass,vtLists}=getPermsOfAdjList[graphData0,vtList];];
totalCircTime+=circTime;
If[pass,

For[i=1,i<=Length[vtLists],i++,
connectCircCount++;
(*this will be aloop dealing with the circular permutations, and splitting groups*)

{addEdgeTime,null}=AbsoluteTiming[
{pass,graphDataT}=addEdges[graphData0,cv,vtLists[[i]],connectingGroup,dial0,currentDialAndLocalisedBans0];];
totalAddEdgeTime+=addEdgeTime;

If[pass,
graphDataT=setAdjacencyList[graphDataT,1,connectingGroup,vtLists[[i]]];
(*quickcheck*)
testCount++;
{testTime,null}=AbsoluteTiming[
{pass,graphDataT}=testPartialMatrices[graphDataT,vtList];];
totalTestTime+=testTime;
If[pass,
AppendTo[graphs,graphDataT];
];
];
];
];
Return[graphs];
];
(*augmenting for graphing colours*)
iterate[graphData_,parentGraphNumber_]:=Module[{connectingGroup,t,dials,currentDialPositionAndLocalisedBans,vertexNumbers,graphs={},i,adjLists,oldDialPosition,reIndexedGraphExistsQ=False,reIndexedGraphData},

{dialTime,null}=AbsoluteTiming[
{dials,currentDialPositionAndLocalisedBans,adjLists,connectingGroup,oldDialPosition}=connectionsPossible[graphData];];
totalDialTime+=dialTime;

For[i=1,i<=Length[dials],i++,
dialCount++;
{connectTime,null}=AbsoluteTiming[
If[oldDialPosition[[2]]==0||currentDialPositionAndLocalisedBans[[i]][[1]]==oldDialPosition,
graphs=Join[graphs,connectVertex[graphData,connectingGroup,dials[[i]],currentDialPositionAndLocalisedBans[[i]],adjLists[[i]]]],
If[!reIndexedGraphExistsQ,
reIndexedGraphData=reIndexPerms[graphData];
reIndexedGraphExistsQ=True;
];
graphs=Join[graphs,connectVertex[reIndexedGraphData,connectingGroup,dials[[i]],currentDialPositionAndLocalisedBans[[i]],adjLists[[i]]]];
];
];
totalConnectTime+=connectTime;
];
If[Length[graphs]==0,
If[Length[dials]==0,
(*reason is no dials*)
AppendTo[vStyles,parentGraphNumber->Purple],
(*else all graphs failed*)
AppendTo[vStyles,parentGraphNumber->Gray];
];
]; 

graphs=getUniqueGraphs[graphs,oldDialPosition,connectingGroup];
Return[graphs];
];
(*added valid Branch boolean*)
recurI[parentGraph_,depth_,parentGraphNumber_]:=Module[{childGraphs,i,pos,validBranch=False,validBranchT},
If[depth<terminate,

childGraphs=iterate[parentGraph,parentGraphNumber];
(*added to check branchs which yield results*)
If[Length[childGraphs]==0,
validBranch=False;
];
For[i=1,i<=Length[childGraphs],i++,
graphC=graphC+1;
If[Length[numbers]>0&&(numbers=={-1}||MemberQ[numbers,graphC]),
AppendTo[graphsWanted,{graphC,childGraphs[[i]]}];
];

AppendTo[edges,parentGraphNumber<-> graphC];

validBranchT=recurI[childGraphs[[i]],depth+1,graphC]||validBranch;
If[validBranchT,
validBranch=True;
];
];
If[validBranch,
AppendTo[vStyles,parentGraphNumber->Green];
],

countT=countT+1;
AppendTo[vStyles,parentGraphNumber->Red];
AppendTo[graphsComplete,makeOldDataStructure[parentGraph]];
validBranch=True;


];
Return[validBranch];
];

makeDir[v0_]:=Module[{},
If[!FileExistsQ[FileNameJoin[{NotebookDirectory[],ToString[v0]}]],
CreateDirectory[FileNameJoin[{NotebookDirectory[],ToString[v0]}]];
];
];

exportOutput[]:=Module[{},
Export[FileNameJoin[{NotebookDirectory[],ToString[v],StringJoin[ToString[a],".txt"]}],graphsComplete];
Export[FileNameJoin[{NotebookDirectory[],ToString[v],StringJoin[ToString[a],"DATA.txt"]}],{a,graphC,timing,countT,vStyles}];
Export[FileNameJoin[{NotebookDirectory[],ToString[v],StringJoin[ToString[a]," ", ToString[countT]," graphs.txt"]}],
TreeGraph[edges,GraphLayout->{"LayeredEmbedding", "RootVertex"->1},VertexLabels->Placed["Name",{0.5,0.5}],VertexShapeFunction->"Square",VertexSize->0.4,VertexStyle->vStyles,ImageSize->graphC*20]];
];

makeGraphs[v0_,a0_,numbers0_]:=Module[{},
v=v0;
a=a0;
terminate=Length[a[[1]]];
vStyles={};
edges={};
numbers=numbers0;
graphsWanted={};
gd=makeGraphData[a];
countT=0;
(*Tracking Variables*)
dialCount=0;
connectCircCount=0;
totalDialTime=0;
totalConnectTime=0;
totalTestTime=0;
testCount=0;
totalCircTime=0;

totalAddEdgeTime=0;
graphC=1;
If[Length[numbers]>0&&(numbers=={-1}||MemberQ[numbers,graphC]),
AppendTo[graphsWanted,{graphC,gd}];
];
graphsComplete={};
{timing,null}=AbsoluteTiming[recurI[gd,0,1]];
Return[{timing,graphC,countT,graphsComplete,graphsWanted,edges,vStyles}];
];

getPairs[v0_]:=getPairsI[v0];

End[]
EndPackage[]




