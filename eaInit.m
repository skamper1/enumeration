(* ::Package:: *)

BeginPackage["eaInit`"]

getPairsI;


Begin["`Private`"]
getPairsI[v0_]:=Module[{v=v0,vList,dss,size,l,term,h,small,big,i,j,smalli,bigj,smalli3Count,bigj3Count,a},
vList={};
dss=getAllDegreeSequences[v];
size=Length[dss];
l=1;
term=Ceiling[size/2];
While[l<=term,
h=size-l+1;
small=Table[dss[[l]][[i]],{i,2,Length[dss[[l]]]}];
big=Table[dss[[h]][[i]],{i,2,Length[dss[[h]]]}];
For[i=1,i<=Length[small],i++,
(*if statement stops repetition on the ds pairs of size v/2*)
For[j=If[h==l,i,1],j<=Length[big],j++,
smalli=small[[i]];
bigj=big[[j]];
smalli3Count=Count[smalli,3];
bigj3Count=Count[bigj,3];
If[bigj3Count>= smalli3Count,
a={bigj,smalli},
(*else*)
a={smalli,bigj};
];
AppendTo[vList,a];
];
];
l=l+1;
];
Return[vList];
];



genAllDegSeqs[v0_]:=Module[{v=v0,e,f,maxVa,minVa,excess,list},
e=2*v-4;
f=v-2;
maxVa=Floor[(2*v-4)/3];
minVa=v-maxVa;
list=Table[{vi,e-3*(vi)},{vi,minVa,maxVa}];
Return[list];
]
getAllDegreeSequences[v0_]:=Module[{v=v0,va,excess,list,i,parts,j,del,outlist,ds,k,p,outlisti},
list=genAllDegSeqs[v];
outlist={};
For[i=1,i<=Length[list],i++,
{va,excess}=list[[i]];
outlisti={va};
parts=IntegerPartitions[excess,va];
del={};
For[j=1,j<=Length[parts],j++,
If[Max[parts[[j]]]+3>=va,
AppendTo[del,{j}];
];
];
parts=Delete[parts,del];
For[j=1,j<=Length[parts],j++,
ds=Table[3,{l,1,va}];
For[k=1,k<=Length[parts[[j]]],k++,
ds[[k]]=ds[[k]]+parts[[j]][[k]];
];
AppendTo[outlisti,ds];
];
AppendTo[outlist,outlisti];
];
Return[outlist];
];

End[];

EndPackage[];


