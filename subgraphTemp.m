(* ::Package:: *)

BeginPackage["subgraphTemp`"]

getUniqueGraphs;
makeSubgraph;


Begin["`Private`"]

Needs["eaDataStructure`",FileNameJoin[{NotebookDirectory[],"eaDataStructure.m"}]];

getUniqueGraphs[graphs0_,oldDialPosition_,connectingGroup_]:=Module[{i=1,iold=1,graphs={},lastDialPosition},

While[i<=Length[graphs0],
lastDialPosition=getCurrentDialAndLocalisedBans[graphs0[[i]],1,connectingGroup];
iold=i;
While[i<Length[graphs0]&&lastDialPosition==getCurrentDialAndLocalisedBans[graphs0[[i+1]],1,connectingGroup],
i++;
];
graphs=Join[graphs,checkForBannedSubgraphs[Table[graphs0[[j]],{j,iold,i}],oldDialPosition==lastDialPosition]];
i++;
];
Return[graphs];
];

checkForBannedSubgraphs[graphs_,usePrevBannedSubgraphsQ_]:=Module[{output={},bannedSubgraphs={},i,graphi,subgraphi,pass,lowestConnectionNumberi},

If[usePrevBannedSubgraphsQ,
bannedSubgraphs=getBannedSubgraphs[graphs[[1]]];
];
For[i=1,i<=Length[graphs],i++,
graphi=graphs[[i]];
subgraphi=makeSubgraph[graphi];
lowestConnectionNumberi=subgraphi[[3]];
pass=checkGraphAgainstBannedSubgraphs[graphi,bannedSubgraphs,lowestConnectionNumberi];
If[pass,
(*need to set bannedsubgraphs herer*)
graphi=setBannedSubgraphs[graphi,bannedSubgraphs];
AppendTo[output,graphi];
AppendTo[bannedSubgraphs,subgraphi];
];
];
Return[output];
];


checkGraphAgainstBannedSubgraphs[graph_,bannedSubgraphs_,lowestConnectionNumber_]:=Module[{pass=False,mapping,i=1},
While[i<=Length[bannedSubgraphs]&&!pass,
If[bannedSubgraphs[[i]][[3]]<=lowestConnectionNumber,
{pass,mapping}=mapFirstVertex[graph,bannedSubgraphs[[i]]];
];
i++;
];
Return[!pass];
];



shareFaceQVL[vi_,vj_,vx_]:=Module[{viIndex,vjIndex,mat,vR},
viIndex=index[[vi]];
vjIndex=index[[vj]];
mat=mats[[viIndex[[1]]]];
If[mat[[viIndex[[2]]]][[vjIndex[[2]]]]==vx,
vR=mat[[vjIndex[[2]]]][[viIndex[[2]]]],
vR=mat[[viIndex[[2]]]][[vjIndex[[2]]]];
];
Return[{vR!=0,vR}];
];
makeGroupIndexL[]:=Module[{i,groupsiNum,j,groupijFreq,k,index={}},
For[i=1,i<=2,i++,
groupsiNum=getNumberOfGroups[graphData,i];
For[j=1,j<=groupsiNum,j++,
groupijFreq=getVertexGroupFrequency[graphData,i,j];
For[k=1,k<=groupijFreq,k++,
AppendTo[index,j];
];
];
];
Return[index];
];
(*returns {0,0} if no vertex has been connected*)
getLastConnectedVertexL[]:=Module[{connectionOrder,groups1Num,i,group1iCD,group1iVN,group1iConnectionNumber,maxConnectionNumber=0,vN=0,groupIndice=0,vNAdjList={}},
connectionOrder=getConnectionOrder[graphData];
groups1Num=getNumberOfGroups[graphData,1];
For[i=1,i<=groups1Num,i++,
group1iCD=getCurrentDegree[graphData,1,i];
group1iVN=getVertexGroupVN[graphData,1,i];
group1iConnectionNumber=connectionOrder[[group1iVN]];
If[group1iCD==0&&maxConnectionNumber<group1iConnectionNumber,
maxConnectionNumber=group1iConnectionNumber;
vN=group1iVN;
vNAdjList=getAdjacencyList[graphData,1,i];
groupIndice=i;
];
];
Return[{vN,groupIndice,vNAdjList,maxConnectionNumber}];
];


(*returns {} if no vertex has been connected*)
getLastConnectedVerticesL[]:=Module[{connectionOrder,i,group1iCD,group1iVN,maxConnectionNumber=0,vN=0,groupIndice=0,vNAdjList={},list={},dialPosition},
{vN,groupIndice,vNAdjList,maxConnectionNumber}=getLastConnectedVertexL[];
If[vN>0,
dialPosition=getCurrentDialAndLocalisedBans[graphData,1,groupIndice];
list={{vN,vNAdjList}};
i=groupIndice;
While[i>1&&getCurrentDialAndLocalisedBans[graphData,1,i-1]==dialPosition,
i--;
group1iCD=getCurrentDegree[graphData,1,i];
group1iVN=getVertexGroupVN[graphData,1,i];
vN=group1iVN;
vNAdjList=getAdjacencyList[graphData,1,i];
AppendTo[list,{vN,vNAdjList}];
];
];
Return[list];
];



makeSubgraph[graphData0_]:=Module[{vX,indiceX,adjListX,connectionNumberX,seen,connectionOrder,lowestConnectionNumber,
toSeeGroup1,toSeeGroup2,group1List,vi,connectionNumberVi,viAdj,i,vf,vt,pass,vR},
graphData=graphData0;
{index,mats}=getMatrixInfo[graphData];
{vX,indiceX,adjListX,connectionNumberX}=getLastConnectedVertexL[];
If[vX>0,
groupIndex=makeGroupIndexL[];
seen=Table[False,{i,1,Length[groupIndex]}];
seen[[vX]]=True;
connectionOrder=getConnectionOrder[graphData];
lowestConnectionNumber=connectionNumberX;
toSeeGroup1={vX};
toSeeGroup2={};
group1List={};
While[Length[toSeeGroup1]>0||Length[toSeeGroup2]>0,
While[Length[toSeeGroup1]>0,
vi=toSeeGroup1[[1]];
connectionNumberVi=connectionOrder[[vi]];
If[lowestConnectionNumber>connectionNumberVi,
lowestConnectionNumber=connectionNumberVi;
];
toSeeGroup1=Delete[toSeeGroup1,{1}];
viAdj=getAdjacencyList[graphData,1,groupIndex[[vi]]];
For[i=1,i<=Length[viAdj],i++,
If[!seen[[viAdj[[i]]]],
AppendTo[toSeeGroup2,viAdj[[i]]];
seen[[viAdj[[i]]]]=True;
];
vf=viAdj[[i]];
vt=viAdj[[Mod[i,Length[viAdj]]+1]];
{pass,vR}=shareFaceQVL[vf,vt,vi];
If[pass,
If[!seen[[vR]],
AppendTo[group1List,{vR,{vf,vt},vi}];

PrependTo[toSeeGroup1,vR];
seen[[vR]]=True;
];
];
];

];
While[Length[toSeeGroup2]>0,
vi=toSeeGroup2[[1]];
viAdj=getAdjacencyList[graphData,2,groupIndex[[vi]]];
For[i=1,i<=Length[viAdj],i++,
If[!seen[[viAdj[[i]]]],
AppendTo[toSeeGroup1,viAdj[[i]]];
seen[[viAdj[[i]]]]=True;
(*not linked to other group1vertices already traversed by faceShare*)
AppendTo[group1List,{viAdj[[i]],{vi},0}];
];
];
toSeeGroup2=Delete[toSeeGroup2,{1}];
];
];
];
Return[{vX,adjListX,lowestConnectionNumber,group1List}];
];


getRotationsOfAdjList[adjList_]:=Module[{},
Return[Join[Table[RotateRight[adjList,i],{i,0,Length[adjList]-1}],Table[Reverse[RotateRight[adjList,i]],{i,0,Length[adjList]-1}]]];
];





mappingFunction[mapping_,vOld_]:=Module[{},
Return[mapping[[1]][[vOld]]];
];


mappingInverseFunction[mapping_,vNew_]:=Module[{},
Return[mapping[[2]][[vNew]]];
];


setMapping[mapping0_,vOld_,vNew_]:=Module[{mapping=mapping0},
mapping[[1]][[vOld]]=vNew;
mapping[[2]][[vNew]]=vOld;
Return[mapping];
];


mapGroup1VertexL[mapping0_,vOld_,vNew_]:=Module[{pass=False,mapping=mapping0,vTest,vTest2,vOldIndex,vNewIndex,currentDialOld,currentDialNew},
vTest=mappingFunction[mapping,vOld];
vTest2=mappingInverseFunction[mapping,vNew];
If[(vTest==0&&vTest2==0)||(vTest==vNew&&vTest2==vOld),
If[vTest==0,
vOldIndex=groupIndex[[vOld]];
vNewIndex=groupIndex[[vNew]];
currentDialOld=getCurrentDialAndLocalisedBans[graphData,1,vOldIndex];
currentDialNew=getCurrentDialAndLocalisedBans[graphData,1,vNewIndex];
If[currentDialOld==currentDialNew,
pass=True;
mapping=setMapping[mapping,vOld,vNew];
],
pass=True;
];
];
Return[{pass,mapping}];
];


mapGroup2VerticesL[mapping0_,vOldList_,vNewList_]:=Module[{pass=True,mapping=mapping0,vOldi,vNewi,i=1,vTest,vTest2,indices,perms,vOldIndexi,vNewIndexi,ds1,ds2,shift},
{indices,perms}=getIndexPerms[graphData];
{ds1,ds2}=getDegreeSequence[graphData];
shift=Length[ds1];
While[i<=Length[vOldList]&&pass,
vOldi=vOldList[[i]];
vNewi=vNewList[[i]];
vTest=mappingFunction[mapping,vOldi];
vTest2=mappingInverseFunction[mapping,vNewi];
If[(vTest==0&&vTest2==0)||(vTest==vNewi&&vTest2==vOldi),
If[vTest==0,
vOldIndexi=indices[[vOldi-shift]];
vNewIndexi=indices[[vNewi-shift]];
If[vOldIndexi==vNewIndexi,
mapping=setMapping[mapping,vOldi,vNewi],
pass=False;
];
],
pass=False;
];
i++;
];
Return[{pass,mapping}];
];





getGroup1VerticesInSubgraph[subgraph_]:=Module[{i,list={subgraph[[1]]}},

For[i=1,i<=Length[subgraph[[4]]],i++,
If[subgraph[[4]][[i]][[1]]!=0&&!MemberQ[list,subgraph[[4]][[i]][[1]]],
AppendTo[list,subgraph[[4]][[i]][[1]]];
];
If[subgraph[[4]][[i]][[3]]!=0&&!MemberQ[list,subgraph[[4]][[i]][[3]]],
AppendTo[list,subgraph[[4]][[i]][[3]]];
];
];
Return[list];
];

mapFirstVertex[graphData0_,subGraph_]:=Module[{mapping,vN,groupIndice,vNAdjList,maxConnectionNumber,vX,adjListX,lowestConnectionNumber,group1List,pass=False,i,rotations,mappingTemp,j,lastConnectedVertices,lastConnectedVertex,group1VerticesInGraph},
graphData=graphData0;
{index,mats}=getMatrixInfo[graphData];
group1VerticesInGraph=getGroup1VerticesInSubgraph[makeSubgraph[graphData]];
{vX,adjListX,lowestConnectionNumber,group1List}=subGraph;
lastConnectedVertices=getLastConnectedVerticesL[];
j=1;
lastConnectedVertex=getLastConnectedVertexL[][[1]];
(*this means the vertex being mapped in the subgraph must be less or equal to than the vertex in the graph being offered up, unless
the subgraph being considered has been made at this stage, (inwhich case it would have the same vertex number as the first vN... in last connected vertices
that is the group 1 vertex connected at this stage, It also needs to make sure that it is not considering a vertex from a disconnected subgraph, hence the memberQ.  
Lowest connection number already verified in parent method.*)
While[j<=Length[lastConnectedVertices]&&!pass&&MemberQ[group1VerticesInGraph,lastConnectedVertices[[j]][[1]]],
{vN,vNAdjList}=lastConnectedVertices[[j]];
rotations=getRotationsOfAdjList[vNAdjList];
groupIndex=makeGroupIndexL[];
mapping={Table[0,{i,1,Length[groupIndex]}],Table[0,{i,1,Length[groupIndex]}]};
i=1;
While[i<=Length[rotations]&&!pass,
mappingTemp=mapping;
{pass,mappingTemp}=mapGroup1VertexL[mappingTemp,vX,vN];
(*not neccessary to check here when used inside algorithm, but for external checks against two graphs in the tree will terminate if conditions aren't met*)
If[pass,
{pass,mappingTemp}=mapGroup2VerticesL[mappingTemp,adjListX,rotations[[i]]];

If[pass,
{pass,mappingTemp}=mapListVertexL[mappingTemp,group1List,1];
];
];
i++;
];
j++;
];

Return[{pass,mappingTemp}];
];



mapListVertexL[mapping0_,group1List_,i_]:=Module[{group1Listi,pass,mapping=mapping0,mappings,passT=False,j=1},
If[i<=Length[group1List],
(*recur*)
group1Listi=group1List[[i]];
If[Length[group1Listi[[2]]]==2,
(*this is a face sharing listing*)

{pass,mapping}=mapFaceSharingVertexL[mapping,group1Listi];
If[pass,
Return[mapListVertexL[mapping,group1List,i+1]],
Return[{False,mapping}];
];
,
(*else this is an edge sharing listing as the length of group1Listi[[2]] is 1*)
{pass,mappings}=mapEdgeSharingVertexL[mapping,group1Listi];
If[pass,
While[j<=Length[mappings]&&!passT,
{passT,mapping}=mapListVertexL[mappings[[j]],group1List,i+1];
j++;
];
If[passT,
Return[{True,mapping}],
Return[{False,mapping}];
];
,
(*failed*)
Return[{False,mapping}];
];
];
,
(*else return pass as reached termination*)
Return[{True,mapping}];
];
];


mapFaceSharingVertexL[mapping0_,group1List_]:=Module[{vROld,vfOld,vtOld,vXOld,vRNew,vfNew,vtNew,vXNew,pass,mapping=mapping0,vROldAdjList,vRNewAdjList,rotsVRNewAdjList,pRotsVRNewAdjList,pOld,posValid},
(*this method finds vRNew from the faceShare method and maps vROld to it*)
{vROld,{vfOld,vtOld},vXOld}=group1List;
vfNew=mappingFunction[mapping,vfOld];
vtNew=mappingFunction[mapping,vtOld];
vXNew=mappingFunction[mapping,vXOld];
(*no need to verify, these must have been found previously*)
{pass,vRNew}=shareFaceQVL[vfNew,vtNew,vXNew];
If[pass,
{pass,mapping}=mapGroup1VertexL[mapping,vROld,vRNew];
If[pass,
vROldAdjList=getAdjacencyList[graphData,1,groupIndex[[vROld]]];
vRNewAdjList=getAdjacencyList[graphData,1,groupIndex[[vRNew]]];
rotsVRNewAdjList=getRotationsOfAdjList[vRNewAdjList];
pOld={Position[vROldAdjList,vfOld],Position[vROldAdjList,vtOld]};
pRotsVRNewAdjList=Table[{Position[rotsi,vfNew],Position[rotsi,vtNew]},{rotsi,rotsVRNewAdjList}];
posValid=Flatten[Position[pRotsVRNewAdjList,pOld]][[1]];
{pass,mapping}=mapGroup2VerticesL[mapping,vROldAdjList,rotsVRNewAdjList[[posValid]]];
];
];
Return[{pass,mapping}];
];


mapEdgeSharingVertexL[mapping0_,group1List_]:=Module[{vROld,vfOld,vRNew,vfNew,pass=False,null,mapping=mapping0,vROldAdjList,vfNewAdjList,mappings={},i,mappingTemp,passTemp,mappingTemp2,vRNewAdjList,vRNewAdjListPair,j},
(*this method finds all possible group1 vertices that could map to vROld by looking at vertices connected to vfOld*)
{vROld,{vfOld},null}=group1List;
vfNew=mappingFunction[mapping,vfOld];
(*this is used later if a pass is achieved on one of the vfNewAdjList entries*)
vROldAdjList=getAdjacencyList[graphData,1,groupIndex[[vROld]]];
vROldAdjList=RotateLeft[vROldAdjList,Flatten[Position[vROldAdjList,vfOld]][[1]]-1];
(*these are the potential mappings to vRNew*)
vfNewAdjList=getAdjacencyList[graphData,2,groupIndex[[vfNew]]];
For[i=1,i<=Length[vfNewAdjList],i++,
vRNew=vfNewAdjList[[i]];
{passTemp,mappingTemp}=mapGroup1VertexL[mapping,vROld,vRNew];
If[passTemp,
vRNewAdjList=getAdjacencyList[graphData,1,groupIndex[[vRNew]]];
vRNewAdjListPair={RotateLeft[vRNewAdjList,Flatten[Position[vRNewAdjList,vfNew]][[1]]-1],RotateLeft[Reverse[vRNewAdjList],Flatten[Position[Reverse[vRNewAdjList],vfNew]][[1]]-1]};
For[j=1,j<=2,j++,
{passTemp,mappingTemp2}=mapGroup2VerticesL[mappingTemp,vROldAdjList,vRNewAdjListPair[[j]]];
If[passTemp,
pass=True;
AppendTo[mappings,mappingTemp2];
];
];
];
];
Return[{pass,mappings}];
];

End[];
EndPackage[];
