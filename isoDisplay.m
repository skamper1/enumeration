(* ::Package:: *)

BeginPackage["isoDisplay`"];

isoDisplayV;

Begin["Private`"];

cols={Blue,Cyan,Magenta,Yellow,Brown,Orange,Pink,Purple,LightRed,LightGreen};

isoDisplayV[v_,a_]:=Module[{fnsDATA,fnsGRAPH,fnsCOMPLETE,aP,timing,countT,gra,allCOMPLETE,positionDS,positionGRAPHS,completeGRAPHS},
fnsDATA=FileNames[ToString[a]<>"*DATA.txt",FileNameJoin[{NotebookDirectory[],ToString[v]}]];
fnsGRAPH=FileNames[ToString[a]<>"*graphs.txt",FileNameJoin[{NotebookDirectory[],ToString[v]}]];
fnsCOMPLETE=FileNames[StringJoin[ToString[v],"V","*.txt"],{FileNameJoin[{NotebookDirectory[],"graphdata"}]}];
{aP,graphC,timing,countT,vStyles}=ReadList[fnsDATA[[1]]];
gra=ToExpression[Import[fnsGRAPH[[1]]]];
allCOMPLETE=ReadList[fnsCOMPLETE[[1]]];
positionDS=Position[allCOMPLETE,a];
positionGRAPHS=positionDS[[1]][[1]];
completeGRAPHS=allCOMPLETE[[positionGRAPHS]][[2]];
edges=EdgeList[gra];

completeGraphs=getCompleteGraphs[];
validGraphs=Union[Flatten[Table[findAncestors[cGi],{cGi,completeGraphs}]]];
dftForValidGraphs[];
graphV=Graph[edgesV,VertexLabels->Placed["Name",{0.5,0.5}],GraphLayout->{"LayeredEmbedding", "RootVertex"->1},VertexShapeFunction->"Square",VertexSize->0.8,VertexStyle->vStylesV];
Return[Column[{Row[{Column[{"Degree Sequences",a}],Column[{"graphC",graphC}],Column[{"timing",timing}],Column[{"countT",countT}]},"      "],getFormattedGraph2[completeGRAPHS,graphV,a,oldIndex,oldVertexList,vStylesV],Row[{"Number of Unique Graphs: ", Length[completeGRAPHS],Column[Table[Prepend[completeGRAPHSi[[3]],completeGRAPHSi[[2]]],{completeGRAPHSi,completeGRAPHS}]]}]}]];

Return[getFormattedGraph2[completeGRAPHS,graphV,a,oldIndex,oldVertexList,vStylesV]];
];


getSames[cgs_]:=Module[{i,j,sames={}},
For[i=1,i<=Length[cgs],i++,
AppendTo[sames,Prepend[cgs[[i]][[3]],cgs[[i]][[2]]]];
];
Return[sames];
];



getFormattedGraph2[cgs_,gra_,a0_,oldIndex_,oldVertexList_,oldStyles_]:=Module[{sames,oldEdges,oldCoords,newEdges,newCoords,newStyles,newLabels,newEdgeStyles,a=a0},
sames=getSames[cgs];
oldEdges=EdgeList[gra];
oldCoords=AbsoluteOptions[gra,VertexCoordinates][[1]][[2]];
{newEdges,newCoords,newStyles,newLabels,newEdgeStyles}=vCoordsStylesAndLabels2[oldEdges,oldCoords,oldStyles,oldIndex,oldVertexList,sames,a];
Return[Graph[newEdges,VertexCoordinates->newCoords(*,EdgeStyle->newEdgeStyles,*),VertexLabels->newLabels,VertexShapeFunction->"Square",VertexSize->0.8,VertexStyle->newStyles,ImageSize->{800,400}]];
];

(*arranges the complete graph groups into a minimal (+1) amount of levels, the first level is only for individual graphs*)  
levels[sams_]:=Module[{levs={{}},i,j,first,last},
For[i=1,i<=Length[sams],i++,
If[Length[sams[[i]]]==1,
AppendTo[levs[[1]],sams[[i]]],
j=2;
first=sams[[i]][[1]];
last=Last[sams[[i]]];
While[j<=Length[levs]&&(first<Last[Last[levs[[j]]]]),
j=j+1;
];
If[j<=Length[levs],
AppendTo[levs[[j]],sams[[i]]],
AppendTo[levs,{sams[[i]]}];
];
];
];
Return[levs];
];
vCoordsStylesAndLabels2[oldEdges_,oldCoords_,oldStyles_,oldIndex_,oldVertexList_,sames_,a0_]:=Module[{a=a0,levs,newEdges,newCoords,newStyles,newLabels,newEdgeStyles={},vDist,vN,flatLevs,from,to,newVn,co,i,j,k},
levs=levels[sames];
flatLevs=Flatten[levs];
newEdges=oldEdges;
newCoords=oldCoords;
newStyles=oldStyles;
newLabels=Table[oldVertexList[[i]]->Placed[ToString[oldVertexList[[i]]],{0.5,0.5}],{i,1,Length[newCoords]}];
newVn=graphC+1;
If[Length[flatLevs]>0,
vN=flatLevs[[1]];
vDist=(newCoords[[1]][[2]]-newCoords[[oldIndex[[vN]]]][[2]])/(Length[a[[1]]]-1);

For[i=1,i<=Length[levs],i++,
For[j=1,j<=Length[levs[[i]]],j++,
For[k=1,k<=Length[levs[[i]][[j]]],k++,
from=levs[[i]][[j]][[k]];
to=newVn;
newVn=newVn+1;
co=newCoords[[oldIndex[[from]]]];
AppendTo[newCoords,co-{0,i*vDist}];
AppendTo[newLabels,to->Placed[ToString[from],{0.5,0.5}]];
AppendTo[newStyles,to->cols[[Mod[Length[levs[[i]][[j]]]-1,10]+1]]];
(*edges showing the extension down*)
(*
AppendTo[newEdges,from\[UndirectedEdge]to];*)
(*Has to be included if not showing all extensions can delete if uncommenting line above*)
If[i==1,
AppendTo[newEdges,from<->to];
];
If[k>=2,
AppendTo[newEdges,to-1<->to];
AppendTo[newEdgeStyles,to-1<->to->Directive[Thick,Black]];
];
];
];
];
];

Return[{newEdges,newCoords,newStyles,newLabels,newEdgeStyles}];

];
findParent[vc_]:=Module[{edge},
If[MemberQ[edges,_<->vc],
edge=FirstCase[edges,_<->vc];
Return[edge[[1]]],
Return[0];
];
];
findChildren[vp_]:=Module[{allEdges},
If[MemberQ[edges,vp<->_],
allEdges=Cases[edges,vp<->_];
Return[Table[aei[[2]],{aei,allEdges}]],
Return[{}];
];
];
findAncestors[vc_]:=Module[{ancestors={},ayoung=vc,aold},
While[ayoung!=0,
aold=findParent[ayoung];
PrependTo[ancestors,ayoung];
ayoung=aold;
];
Return[ancestors];
];
getCompleteGraphs[]:=Module[{p,vs},
p=Position[vStyles,Red];
vs=Table[vStyles[[pi[[1]]]][[1]],{pi,p}];
Return[vs];
];

dftForValidGraphs[]:=Module[{children,i},
edgesV={};
vStylesV={};
oldIndex=Table[0,{i,1,graphC}];
oldVertexList={1};
count=1;
oldIndex[[1]]=1;
children=findChildren[1];
For[i=1,i<=Length[children],i++,
If[validGraphQ[children[[i]]],
AppendTo[edgesV,1<->children[[i]]];
If[completeGraphQ[children[[i]]],
AppendTo[vStylesV,children[[i]]->Red],
AppendTo[vStylesV,children[[i]]->Green];
];
oldIndex[[children[[i]]]]=++count;
AppendTo[oldVertexList,children[[i]]];
dftForValidGraphsR[children[[i]]];
];
];
If[Length[edgesV]>0,
PrependTo[vStylesV,1->Green];
];
];
dftForValidGraphsR[v_]:=Module[{children,i},
children=findChildren[v];
For[i=1,i<=Length[children],i++,
If[validGraphQ[children[[i]]],
AppendTo[edgesV,v<->children[[i]]];
If[completeGraphQ[children[[i]]],
AppendTo[vStylesV,children[[i]]->Red],
AppendTo[vStylesV,children[[i]]->Green];
];
oldIndex[[children[[i]]]]=++count;

AppendTo[oldVertexList,children[[i]]];
dftForValidGraphsR[children[[i]]];
];
];
];



validGraphQ[v_]:=Module[{},
Return[MemberQ[validGraphs,v]];
];
completeGraphQ[v_]:=Module[{},
Return[MemberQ[completeGraphs,v]];
];
End[];
EndPackage[];



